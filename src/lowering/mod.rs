use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicMetadataTypeEnum,
    values::{AnyValue, AnyValueEnum, BasicValue, FunctionValue},
    IntPredicate,
};

use crate::{
    ast::{nodes::*, Ast, AstVisitor},
    compilation::{CompilationUnit, Function, Type, VariableId},
    diagnostics::TextSpan,
};

pub struct Compiler<'ctx> {
    unit: &'ctx CompilationUnit, // maybe needs separate lifetime?
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    variables: HashMap<VariableId, AnyValueEnum<'ctx>>,
    last_value: Option<Box<dyn BasicValue<'ctx> + 'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    fn new(unit: &'ctx CompilationUnit, context: &'ctx Context) -> Self {
        let module = context.create_module("test");
        let builder = context.create_builder();
        Self {
            unit,
            context,
            module,
            builder,
            variables: HashMap::new(),
            last_value: None,
        }
    }

    pub fn compile(unit: &CompilationUnit) {
        let context = Context::create();
        let mut compiler = Compiler::new(unit, &context);

        unit.ast.visit(&mut compiler);

        compiler.module.print_to_stderr();
    }

    fn build_fn(&mut self, func: &Function) -> FunctionValue<'ctx> {
        let param_vars: Vec<_> = func
            .params
            .iter()
            .map(|id| &self.unit.scope.variables[*id])
            .collect();

        let param_types: Vec<_> = param_vars
            .iter()
            .map(|var| self.create_basic_meta_type(&var.typ))
            .collect();
        let fn_type = match func.sig.return_type {
            Type::Int => {
                let return_type = self.context.i32_type();
                return_type.fn_type(param_types.as_slice(), false)
            }
            Type::Bool => {
                let return_type = self.context.bool_type();
                return_type.fn_type(param_types.as_slice(), false)
            }
            Type::Void => {
                let return_type = self.context.void_type();
                return_type.fn_type(param_types.as_slice(), false)
            }
            Type::Func(_) => unimplemented!("function return type llvm conversion"),
            Type::Unresolved => panic!("unresolved type"),
        };

        let func = self.module.add_function(&func.name, fn_type, None);
        let mut new_variables = HashMap::new();
        func.get_param_iter()
            .zip(param_vars)
            .for_each(|(param, var)| {
                param.set_name(&var.identifier);
                new_variables.insert(var.id, param.as_any_value_enum());
            });

        self.variables.extend(new_variables);
        func
    }

    fn create_basic_meta_type(&self, typ: &Type) -> BasicMetadataTypeEnum<'ctx> {
        match typ {
            Type::Int => self.context.i32_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Func(_) => unimplemented!("function type llvm conversion"),
            Type::Void => panic!("void is not a valid basic metadata type"),
            Type::Unresolved => panic!("unresolved type"),
        }
    }
}

impl<'ctx> AstVisitor for Compiler<'ctx> {
    fn visit_func_decl(&mut self, ast: &Ast, func: &FunctionDecl, _item: &Item) {
        let func_def = &self.unit.scope.functions[func.id];
        let llvm_func = self.build_fn(&func_def);

        // set up start of function
        let entry = self.context.append_basic_block(llvm_func, "entry");
        self.builder.position_at_end(entry);

        // write body
        self.visit_expr(ast, func.body);
        // build return statement
        let last_value = self.last_value.take();
        self.builder.build_return(last_value.as_deref()).unwrap();

        if llvm_func.verify(true) {
            // do something?
        } else {
            self.module.print_to_stderr();
            panic!("Invalid function definition");
        }
    }

    fn visit_variable_decl(&mut self, _ast: &Ast, _variable_decl: &VariableDecl, _stmt: &Stmt) {
        todo!()
    }

    fn visit_while_stmt(&mut self, _ast: &Ast, _while_stmt: &WhileStmt, _stmt: &Stmt) {
        todo!()
    }

    fn visit_return_stmt(&mut self, _ast: &Ast, _return_stmt: &ReturnStmt, _stmt: &Stmt) {
        todo!()
    }

    fn visit_error(&mut self, _ast: &Ast, _span: &TextSpan, _expr: &Expr) {
        todo!()
    }

    fn visit_integer_expr(&mut self, _ast: &Ast, int_expr: &IntegerExpr, _expr: &Expr) {
        let value = self
            .context
            .i32_type()
            .const_int(int_expr.value.try_into().unwrap(), true);
        self.last_value = Some(Box::new(value));
    }

    fn visit_boolean_expr(&mut self, _ast: &Ast, bool_expr: &BooleanExpr, _expr: &Expr) {
        let value = self
            .context
            .bool_type()
            .const_int(bool_expr.value.into(), true);
        self.last_value = Some(Box::new(value));
    }

    fn visit_assign_expr(&mut self, _ast: &Ast, _assign_expr: &AssignExpr, _expr: &Expr) {
        todo!()
    }

    fn visit_binary_expr(&mut self, ast: &Ast, binary_expr: &BinaryExpr, _expr: &Expr) {
        self.visit_expr(ast, binary_expr.left);
        let left = self.last_value.take().unwrap();
        self.visit_expr(ast, binary_expr.right);
        let right = self.last_value.take().unwrap();

        let kind = binary_expr.operator.kind;
        match kind {
            BinaryOperatorKind::Add
            | BinaryOperatorKind::Subtract
            | BinaryOperatorKind::Mulitply
            | BinaryOperatorKind::Divide
            | BinaryOperatorKind::BitwiseAnd
            | BinaryOperatorKind::BitwiseOr
            | BinaryOperatorKind::Equals
            | BinaryOperatorKind::NotEquals
            | BinaryOperatorKind::LessThan
            | BinaryOperatorKind::LessThanOrEquals
            | BinaryOperatorKind::GreaterThan
            | BinaryOperatorKind::GreaterThanOrEquals => {
                let lhs = left.as_basic_value_enum().into_int_value();
                let rhs = right.as_basic_value_enum().into_int_value();

                let op = match kind {
                    BinaryOperatorKind::Add => self.builder.build_int_add(lhs, rhs, "add"),
                    BinaryOperatorKind::Subtract => self.builder.build_int_sub(lhs, rhs, "add"),
                    BinaryOperatorKind::Mulitply => self.builder.build_int_mul(lhs, rhs, "add"),
                    BinaryOperatorKind::Divide => {
                        self.builder.build_int_signed_div(lhs, rhs, "add")
                    }
                    BinaryOperatorKind::BitwiseAnd => self.builder.build_and(lhs, rhs, "bitand"),
                    BinaryOperatorKind::BitwiseOr => self.builder.build_or(lhs, rhs, "bitor"),
                    BinaryOperatorKind::Equals => {
                        self.builder
                            .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq")
                    }
                    BinaryOperatorKind::NotEquals => {
                        self.builder
                            .build_int_compare(IntPredicate::NE, lhs, rhs, "eq")
                    }
                    BinaryOperatorKind::LessThan => {
                        self.builder
                            .build_int_compare(IntPredicate::SLT, lhs, rhs, "eq")
                    }
                    BinaryOperatorKind::LessThanOrEquals => {
                        self.builder
                            .build_int_compare(IntPredicate::SLE, lhs, rhs, "eq")
                    }
                    BinaryOperatorKind::GreaterThan => {
                        self.builder
                            .build_int_compare(IntPredicate::SGT, lhs, rhs, "eq")
                    }
                    BinaryOperatorKind::GreaterThanOrEquals => {
                        self.builder
                            .build_int_compare(IntPredicate::SGE, lhs, rhs, "eq")
                    }
                    _ => unreachable!(),
                }
                .unwrap();
                self.last_value = Some(Box::new(op));
            }
            BinaryOperatorKind::LogicalAnd | BinaryOperatorKind::LogicalOr => {
                let lhs = left.as_basic_value_enum().into_int_value();
                let rhs = right.as_basic_value_enum().into_int_value();

                let op = match kind {
                    BinaryOperatorKind::LogicalAnd => self.builder.build_and(lhs, rhs, "and"),
                    BinaryOperatorKind::LogicalOr => self.builder.build_or(lhs, rhs, "or"),
                    _ => unreachable!(),
                }
                .unwrap();
                self.last_value = Some(Box::new(op));
            }
        }
    }

    fn visit_unary_expr(&mut self, _ast: &Ast, _unary_expr: &UnaryExpr, _expr: &Expr) {
        todo!()
    }

    fn visit_variable_expr(&mut self, _ast: &Ast, variable_expr: &VariableExpr, _expr: &Expr) {
        let id = variable_expr.id;
        let var = self.variables.get(&id).unwrap();
        let var_value = match self.unit.scope.variables[id].typ {
            Type::Int | Type::Bool => Box::new(var.into_int_value()),
            Type::Unresolved => todo!(),
            Type::Void => todo!(),
            Type::Func(_) => todo!(),
        };
        self.last_value = Some(var_value);
    }

    fn visit_if_expr(&mut self, _ast: &Ast, _if_expr: &IfExpr, _expr: &Expr) {
        todo!()
    }

    fn visit_call_expr(&mut self, _ast: &Ast, _call_expr: &CallExpr, _expr: &Expr) {
        todo!()
    }
}
