use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    types::{BasicMetadataTypeEnum, BasicTypeEnum},
    values::{
        AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, InstructionOpcode,
        PointerValue,
    },
    IntPredicate,
};

use super::{
    ast::{node::*, Ast, AstVisitor},
    diagnostics::TextSpan,
    CompilationUnit, Function, GlobalScope, Type, VariableId,
};

struct FunctionContext<'ctx> {
    function: FunctionValue<'ctx>,
    return_value: Option<PointerValue<'ctx>>,
    return_block: BasicBlock<'ctx>,
}

enum LastValue<'ctx> {
    BasicValue(BasicValueEnum<'ctx>),
    FunctionValue(FunctionValue<'ctx>),
}

impl<'ctx> LastValue<'ctx> {
    fn as_basic_value(self) -> BasicValueEnum<'ctx> {
        match self {
            LastValue::BasicValue(basic) => basic,
            LastValue::FunctionValue(_) => panic!("expected basic value but found function"),
        }
    }

    fn as_func_value(self) -> FunctionValue<'ctx> {
        match self {
            LastValue::BasicValue(_) => panic!("expected function value but found basic"),
            LastValue::FunctionValue(func) => func,
        }
    }
}

pub struct IRBuilder<'ctx, 'a> {
    scope: &'a GlobalScope, // maybe needs separate lifetime?
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    current_function: Option<FunctionContext<'ctx>>,
    variables: HashMap<VariableId, AnyValueEnum<'ctx>>,
    last_value: Option<LastValue<'ctx>>,
}

impl<'ctx, 'a> IRBuilder<'ctx, 'a> {
    fn new(scope: &'a GlobalScope, context: &'ctx Context) -> Self {
        let module = context.create_module("test");
        let builder = context.create_builder();
        Self {
            scope,
            context,
            module,
            builder,
            current_function: None,
            variables: HashMap::new(),
            last_value: None,
        }
    }

    pub fn build(compilation: &'a CompilationUnit, context: &'ctx Context) -> Module<'ctx> {
        let mut compiler = IRBuilder::new(&compilation.scope, context);

        let fpm = PassManager::create(&compiler.module);

        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();

        fpm.initialize();

        compilation.ast.visit(&mut compiler);

        let module = compiler.module;

        module.get_functions().for_each(|f| {
            fpm.run_on(&f);
        });

        module
    }

    fn build_fn(&mut self, func: &Function) -> FunctionValue<'ctx> {
        let param_vars: Vec<_> = func
            .params
            .iter()
            .map(|id| &self.scope.variables[*id])
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
            Type::Func(_) => unimplemented!("function meta type llvm conversion"),
            Type::Void => panic!("void is not a valid basic metadata type"),
            Type::Unresolved => panic!("unresolved type"),
        }
    }

    fn create_basic_type(&self, typ: &Type) -> BasicTypeEnum<'ctx> {
        match typ {
            Type::Int => self.context.i32_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Func(_) => unimplemented!("function type llvm conversion"),
            Type::Void => panic!("void is not a valid basic type"),
            Type::Unresolved => panic!("unresolved type"),
        }
    }

    fn get_last_basic_value(&mut self) -> Option<BasicValueEnum<'ctx>> {
        self.last_value.take().map(|v| v.as_basic_value())
    }

    fn get_last_func_value(&mut self) -> Option<FunctionValue<'ctx>> {
        self.last_value.take().map(|v| v.as_func_value())
    }

    fn set_last_basic_value(&mut self, value: impl BasicValue<'ctx>) {
        self.last_value = Some(LastValue::BasicValue(value.as_basic_value_enum()))
    }

    fn set_last_func_value(&mut self, func: FunctionValue<'ctx>) {
        self.last_value = Some(LastValue::FunctionValue(func));
    }

    fn append_block(&mut self, label: &str) -> BasicBlock<'ctx> {
        self.context
            .append_basic_block(self.current_function.as_ref().unwrap().function, label)
    }

    fn try_append_break(&mut self, to_block: BasicBlock<'ctx>) {
        // if the then block already already broke out (i.e. return), then we don't need to jump
        // to the ifcont block
        let from_block = self.builder.get_insert_block().unwrap();
        let already_has_br = from_block
            .get_last_instruction()
            .as_ref()
            .filter(|i| i.get_opcode() == InstructionOpcode::Br)
            .is_some();
        if !already_has_br {
            self.builder.build_unconditional_branch(to_block).unwrap();
        }
    }
}

impl<'ctx, 'a> AstVisitor for IRBuilder<'ctx, 'a> {
    fn visit_expr_stmt(&mut self, ast: &Ast, expr_stmt: &ExprStmt, _stmt: &Stmt) {
        self.visit_expr(ast, expr_stmt.expr);
        if expr_stmt.semicolon.is_some() {
            // if it ends with a semicolon it's value cannot be used in future instructions
            self.get_last_basic_value();
        }
    }

    fn visit_func_decl(&mut self, ast: &Ast, func: &FunctionDecl, _item: &Item) {
        let func_def = &self.scope.functions[func.id];
        let llvm_func = self.build_fn(&func_def);

        // register function as variable so it can be called later
        self.variables
            .insert(func_def.var, llvm_func.as_any_value_enum());

        // set up start of function
        let entry = self.context.append_basic_block(llvm_func, "entry");
        // return label will need to be repositioned after visiting the body, but we need it to
        // exist so arbitrary return statements inside the function can jump to it
        let return_block = self.context.append_basic_block(llvm_func, "return");

        let return_type = &func_def.sig.return_type;

        self.current_function = Some(FunctionContext {
            function: llvm_func,
            return_value: None,
            return_block,
        });
        self.builder.position_at_end(entry);

        match return_type {
            Type::Void => {
                self.visit_expr(ast, func.body);

                // need to break to return block otherwise entry could be unterminated
                self.try_append_break(return_block);

                self.builder.position_at_end(return_block);
                self.builder.build_return(None).unwrap();
            }
            _ => {
                let llvm_type = self.create_basic_type(&return_type);
                // return value is stored on the stack to make it easy for arbitrary statements to
                // return without having to worry about phi nodes
                let ret_val = self.builder.build_alloca(llvm_type, "retval").unwrap();
                self.current_function.as_mut().unwrap().return_value = Some(ret_val);

                self.visit_expr(ast, func.body);

                match self.get_last_basic_value() {
                    // function body has an implicit return value so we must store it
                    Some(value) => {
                        self.builder
                            .build_store(ret_val, value.as_basic_value_enum())
                            .unwrap();
                    }
                    // function did not have an implicit return, no need to add one
                    None => (),
                }
                // jump to return in case last call had implicit return
                self.try_append_break(return_block);

                self.builder.position_at_end(return_block);
                // return value must be loaded into a register before we can return it
                let loaded_ret_val = self.builder.build_load(ret_val, "retval").unwrap();
                self.builder.build_return(Some(&loaded_ret_val)).unwrap();
            }
        }

        // reposition return block to end of function
        // if any statements in the function add labels, they will be appended after the return
        // one, so we have to move it back to the end
        let current_block = llvm_func.get_last_basic_block().unwrap();
        return_block.move_after(current_block).unwrap();

        self.current_function.take();
        if llvm_func.verify(true) {
            // do something?
        } else {
            self.module.print_to_stderr();
            panic!("Invalid function definition");
        }
    }

    fn visit_variable_decl(&mut self, ast: &Ast, variable_decl: &VariableDecl, _stmt: &Stmt) {
        self.visit_expr(ast, variable_decl.initial);
        let initial_value = self.get_last_basic_value().unwrap();

        let symbol = &self.scope.variables[variable_decl.variable];
        let typ = self.create_basic_type(&symbol.typ);
        let value = if symbol.is_mutable {
            let alloc = self.builder.build_alloca(typ, &symbol.identifier).unwrap();
            self.builder
                .build_store(alloc, initial_value.as_basic_value_enum())
                .unwrap();
            alloc.as_basic_value_enum()
        } else {
            initial_value.set_name(&symbol.identifier);
            initial_value
        };
        self.variables.insert(symbol.id, value.as_any_value_enum());
    }

    fn visit_while_stmt(&mut self, ast: &Ast, while_stmt: &WhileStmt, _stmt: &Stmt) {
        let cond_block = self.append_block("loop_cond");
        let loop_block = self.append_block("loop");
        let end_block = self.append_block("loop_end");

        // should this use the try method? i can't imagine a scenario where there would be a break
        // immediately before a while and it being valid code
        self.builder.build_unconditional_branch(cond_block).unwrap();

        self.builder.position_at_end(cond_block);
        self.visit_expr(ast, while_stmt.condition);
        let cond = self.get_last_basic_value().unwrap();
        self.builder
            .build_conditional_branch(cond.into_int_value(), loop_block, end_block)
            .unwrap();

        self.builder.position_at_end(loop_block);
        self.visit_expr(ast, while_stmt.body);
        self.try_append_break(cond_block);

        self.builder.position_at_end(end_block);
    }

    fn visit_return_stmt(&mut self, ast: &Ast, return_stmt: &ReturnStmt, _stmt: &Stmt) {
        let current_func = self.current_function.as_ref().unwrap();
        let ret_val = current_func.return_value;
        let ret_block = current_func.return_block;

        if return_stmt.value.is_some() {
            self.visit_expr(ast, return_stmt.value.unwrap());
            let last_value = self.get_last_basic_value().unwrap();
            let ret_val = ret_val.unwrap();
            self.builder.build_store(ret_val, last_value).unwrap();
        }
        self.builder.build_unconditional_branch(ret_block).unwrap();
    }

    fn visit_error(&mut self, _ast: &Ast, _span: &TextSpan, _expr: &Expr) {
        unreachable!("cannot compile error node")
    }

    fn visit_integer_expr(&mut self, _ast: &Ast, int_expr: &IntegerExpr, _expr: &Expr) {
        let value = self
            .context
            .i32_type()
            .const_int(int_expr.value.try_into().unwrap(), true);
        self.set_last_basic_value(value);
    }

    fn visit_boolean_expr(&mut self, _ast: &Ast, bool_expr: &BooleanExpr, _expr: &Expr) {
        let value = self
            .context
            .bool_type()
            .const_int(bool_expr.value.into(), true);
        self.set_last_basic_value(value);
    }

    fn visit_assign_expr(&mut self, ast: &Ast, assign_expr: &AssignExpr, _expr: &Expr) {
        let symbol = &self.scope.variables[assign_expr.variable];

        self.visit_expr(ast, assign_expr.rhs);
        let rhs = self.get_last_basic_value().unwrap();
        rhs.set_name(&symbol.identifier);

        let variable = self.variables.get(&symbol.id).unwrap().into_pointer_value();

        self.builder
            .build_store(variable, rhs.as_basic_value_enum())
            .unwrap();

        self.set_last_basic_value(variable);
    }

    fn visit_binary_expr(&mut self, ast: &Ast, binary_expr: &BinaryExpr, _expr: &Expr) {
        self.visit_expr(ast, binary_expr.left);
        let lhs = self.get_last_basic_value().unwrap();
        self.visit_expr(ast, binary_expr.right);
        let rhs = self.get_last_basic_value().unwrap();

        let kind = binary_expr.operator.kind;
        // for now all binary operations are on int values
        let lhs = lhs.into_int_value();
        let rhs = rhs.into_int_value();
        let value = match kind {
            BinaryOperatorKind::Add => self.builder.build_int_add(lhs, rhs, "add"),
            BinaryOperatorKind::Subtract => self.builder.build_int_sub(lhs, rhs, "sub"),
            BinaryOperatorKind::Mulitply => self.builder.build_int_mul(lhs, rhs, "mul"),
            BinaryOperatorKind::Divide => self.builder.build_int_signed_div(lhs, rhs, "div"),
            BinaryOperatorKind::BitwiseAnd => self.builder.build_and(lhs, rhs, "bitand"),
            BinaryOperatorKind::BitwiseOr => self.builder.build_or(lhs, rhs, "bitor"),
            BinaryOperatorKind::Equals => {
                self.builder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq")
            }
            BinaryOperatorKind::NotEquals => {
                self.builder
                    .build_int_compare(IntPredicate::NE, lhs, rhs, "neq")
            }
            BinaryOperatorKind::LessThan => {
                self.builder
                    .build_int_compare(IntPredicate::SLT, lhs, rhs, "lt")
            }
            BinaryOperatorKind::LessThanOrEquals => {
                self.builder
                    .build_int_compare(IntPredicate::SLE, lhs, rhs, "lte")
            }
            BinaryOperatorKind::GreaterThan => {
                self.builder
                    .build_int_compare(IntPredicate::SGT, lhs, rhs, "gt")
            }
            BinaryOperatorKind::GreaterThanOrEquals => {
                self.builder
                    .build_int_compare(IntPredicate::SGE, lhs, rhs, "gte")
            }
            BinaryOperatorKind::LogicalAnd => self.builder.build_and(lhs, rhs, "and"),
            BinaryOperatorKind::LogicalOr => self.builder.build_or(lhs, rhs, "or"),
        }
        .unwrap()
        .as_basic_value_enum();

        self.set_last_basic_value(value);
    }

    fn visit_unary_expr(&mut self, ast: &Ast, unary_expr: &UnaryExpr, _expr: &Expr) {
        self.visit_expr(ast, unary_expr.operand);
        let result = self.get_last_basic_value().unwrap();

        let result = result.into_int_value();
        let value = match unary_expr.operator.kind {
            UnaryOperatorKind::Identity => result,
            UnaryOperatorKind::Negate => self.builder.build_int_neg(result, "neg").unwrap(),
            UnaryOperatorKind::LogicalNot => self.builder.build_not(result, "not").unwrap(),
        };

        self.set_last_basic_value(value);
    }

    fn visit_variable_expr(&mut self, _ast: &Ast, variable_expr: &VariableExpr, _expr: &Expr) {
        let id = variable_expr.id;
        let var = self.variables.get(&id).unwrap();
        let symbol = &self.scope.variables[id];
        match symbol.typ {
            Type::Int | Type::Bool => {
                let value = if symbol.is_mutable {
                    self.builder
                        .build_load(var.into_pointer_value(), &symbol.identifier)
                        .unwrap()
                        .into_int_value()
                } else {
                    var.into_int_value()
                };
                self.set_last_basic_value(value);
            }
            Type::Unresolved => todo!(),
            Type::Void => todo!(),
            Type::Func(_) => self.set_last_func_value(var.into_function_value()),
        }
    }

    fn visit_if_expr(&mut self, ast: &Ast, if_expr: &IfExpr, expr: &Expr) {
        // create the necessary blocks
        let then_block = self.append_block("then");
        let cont_block = self.append_block("ifcont");
        let else_block = self.append_block("else");

        // if
        self.visit_expr(ast, if_expr.condition);
        let comparison = self.get_last_basic_value().unwrap();
        self.builder
            .build_conditional_branch(comparison.into_int_value(), then_block, else_block)
            .unwrap();

        // then
        self.builder.position_at_end(then_block);
        self.visit_expr(ast, if_expr.then_clause);
        let then_value = self.get_last_basic_value();

        self.try_append_break(cont_block);

        // else
        self.builder.position_at_end(else_block);
        let else_value = if_expr.else_clause.as_ref().and_then(|els| {
            self.visit_expr(ast, els.body);
            self.get_last_basic_value()
        });
        self.try_append_break(cont_block);

        self.builder.position_at_end(cont_block);

        if !matches!(expr.typ, Type::Void) {
            let llvm_type = self.create_basic_type(&expr.typ);
            let phi = self.builder.build_phi(llvm_type, "iftmp").unwrap();
            vec![(then_value, then_block), (else_value, else_block)]
                .into_iter()
                .filter_map(|(value, block)| value.map(|value| (value, block)))
                .for_each(|(value, block)| phi.add_incoming(&[(&value, block)]));
            self.set_last_basic_value(phi.as_basic_value());
        }
    }

    fn visit_call_expr(&mut self, ast: &Ast, call_expr: &CallExpr, _expr: &Expr) {
        // visit arguments
        let mut args = vec![];
        for arg in call_expr.args.iter_items() {
            self.visit_expr(ast, *arg);
            let arg_value = self.get_last_basic_value().unwrap();
            args.push(arg_value.into());
        }

        // visit callee
        self.visit_expr(ast, call_expr.callee);
        let target = self.get_last_func_value().unwrap();

        let value = self.builder.build_call(target, &args, "call").unwrap();

        value
            .try_as_basic_value()
            .map_left(|value| self.set_last_basic_value(value));
    }
}
