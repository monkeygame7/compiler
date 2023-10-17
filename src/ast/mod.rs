use std::fmt::Display;

use crate::{id::Idx, id::IdxVec, idx, scope::VariableId, text::TextSpan};

use self::{lexer::SyntaxToken, printer::AstPrinter, visitor::AstVisitor};

pub mod lexer;
pub mod parser;
pub mod printer;
pub mod visitor;

idx!(ItemId);
idx!(StmtId);
idx!(ExprId);

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Int,
    Bool,
    Unresolved,
}

pub struct Ast {
    statements: IdxVec<StmtId, Stmt>,
    expressions: IdxVec<ExprId, Expr>,
    items: IdxVec<ItemId, Item>,
}

impl Ast {
    pub fn new() -> Self {
        Ast {
            statements: IdxVec::new(),
            expressions: IdxVec::new(),
            items: IdxVec::new(),
        }
    }

    pub fn visit(&mut self, visitor: &mut dyn AstVisitor) {
        for item in self.items.clone().iter() {
            visitor.visit_item(self, item.id);
        }
    }

    pub fn print(&mut self) {
        let mut printer = AstPrinter::new();
        self.visit(&mut printer);
        println!("{}", printer.result);
    }

    pub fn query_item(&self, id: ItemId) -> &Item {
        &self.items[id]
    }

    pub fn query_stmt(&self, id: StmtId) -> &Stmt {
        &self.statements[id]
    }

    pub fn query_expr(&self, id: ExprId) -> &Expr {
        &self.expressions[id]
    }

    pub fn query_expr_mut(&mut self, id: ExprId) -> &mut Expr {
        &mut self.expressions[id]
    }

    pub fn set_type(&mut self, id: ExprId, typ: Type) {
        let expr = self.query_expr_mut(id);
        expr.typ = typ;
    }

    fn create_item(&mut self, kind: ItemKind) -> ItemId {
        let item = Item::new(kind);
        let id = self.items.push(item);
        self.items[id].id = id;
        id
    }

    fn create_stmt(&mut self, kind: StmtKind) -> StmtId {
        let stmt = Stmt::new(kind);
        let id = self.statements.push(stmt);
        self.statements[id].id = id;
        id
    }

    fn create_expr(&mut self, kind: ExprKind) -> ExprId {
        let expr = Expr::new(kind);
        let id = self.expressions.push(expr);
        self.expressions[id].id = id;
        id
    }

    fn create_expr_stmt(&mut self, expr: ExprId) -> StmtId {
        self.create_stmt(StmtKind::Expr(expr))
    }

    fn create_let_stmt(
        &mut self,
        keyword: SyntaxToken,
        identifier: SyntaxToken,
        equals_token: SyntaxToken,
        expr: ExprId,
    ) -> StmtId {
        self.create_stmt(StmtKind::Let(LetStmt {
            keyword,
            identifier,
            variable: VariableId::default(),
            equals_token,
            expr,
        }))
    }

    fn create_error_expr(&mut self, span: TextSpan) -> ExprId {
        self.create_expr(ExprKind::Error(span))
    }

    fn create_block_expr(
        &mut self,
        open_token: SyntaxToken,
        stmts: Vec<StmtId>,
        close_token: SyntaxToken,
    ) -> ExprId {
        self.create_expr(ExprKind::Block(BlockExpr {
            open_token,
            stmts,
            close_token,
        }))
    }

    fn create_binary_expr(
        &mut self,
        left: ExprId,
        operator: BinaryOperator,
        right: ExprId,
    ) -> ExprId {
        self.create_expr(ExprKind::Binary(BinaryExpr {
            left,
            operator,
            right,
        }))
    }

    fn create_unary_expr(&mut self, operator: UnaryOperator, expr: ExprId) -> ExprId {
        self.create_expr(ExprKind::Unary(UnaryExpr {
            operator,
            operand: expr,
        }))
    }

    fn create_assign_expr(&mut self, lhs: SyntaxToken, equals: SyntaxToken, rhs: ExprId) -> ExprId {
        self.create_expr(ExprKind::Assign(AssignExpr {
            identifier: lhs,
            equals,
            rhs,
            variable: VariableId::default(),
        }))
    }

    fn create_paren_expr(&mut self, open: SyntaxToken, expr: ExprId, close: SyntaxToken) -> ExprId {
        self.create_expr(ExprKind::Paren(ParenExpr { open, expr, close }))
    }

    fn create_integer_expr(&mut self, value: i32, token: SyntaxToken) -> ExprId {
        self.create_expr(ExprKind::Integer(IntegerExpr { token, value }))
    }

    fn create_boolean_expr(&mut self, value: bool, token: SyntaxToken) -> ExprId {
        self.create_expr(ExprKind::Boolean(BooleanExpr { token, value }))
    }

    fn create_variable_expr(&mut self, token: SyntaxToken) -> ExprId {
        self.create_expr(ExprKind::Variable(VariableExpr {
            token,
            variable_id: VariableId::default(),
            typ: Type::Unresolved,
        }))
    }
}

#[derive(Debug, Clone)]
pub struct Item {
    kind: ItemKind,
    id: ItemId,
}

impl Item {
    fn new(kind: ItemKind) -> Self {
        Self {
            kind,
            id: ItemId::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Stmt(StmtId),
}

#[derive(Debug, Clone)]
pub struct Stmt {
    kind: StmtKind,
    id: StmtId,
}

impl Stmt {
    fn new(kind: StmtKind) -> Self {
        Self {
            kind,
            id: StmtId::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Expr(ExprId),
    Let(LetStmt),
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub keyword: SyntaxToken,
    pub identifier: SyntaxToken,
    pub variable: VariableId,
    pub equals_token: SyntaxToken,
    pub expr: ExprId,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub id: ExprId,
    pub typ: Type,
}

impl Expr {
    fn new(kind: ExprKind) -> Self {
        Self {
            kind,
            id: ExprId::default(),
            typ: Type::Unresolved,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Error(TextSpan),
    Integer(IntegerExpr),
    Boolean(BooleanExpr),
    Paren(ParenExpr),
    Assign(AssignExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Block(BlockExpr),
    Variable(VariableExpr),
}

#[derive(Debug, Clone)]
pub struct IntegerExpr {
    pub token: SyntaxToken,
    pub value: i32,
}

#[derive(Debug, Clone)]
pub struct BooleanExpr {
    pub token: SyntaxToken,
    pub value: bool,
}

#[derive(Debug, Clone)]
pub struct ParenExpr {
    pub open: SyntaxToken,
    pub expr: ExprId,
    pub close: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct AssignExpr {
    pub identifier: SyntaxToken,
    pub equals: SyntaxToken,
    pub rhs: ExprId,
    pub variable: VariableId,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: ExprId,
    pub operator: BinaryOperator,
    pub right: ExprId,
}

#[derive(Debug, Clone)]
pub struct BinaryOperator {
    pub kind: BinaryOperatorKind,
    pub token: SyntaxToken,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum BinaryOperatorKind {
    Add,
    Subtract,
    Mulitply,
    Divide,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEquals,
    GreaterThan,
    GreaterThanOrEquals,
}

impl BinaryOperatorKind {
    pub fn priority(&self) -> usize {
        match self {
            BinaryOperatorKind::Mulitply | BinaryOperatorKind::Divide => 12,
            BinaryOperatorKind::Add | BinaryOperatorKind::Subtract => 11,
            BinaryOperatorKind::Equals
            | BinaryOperatorKind::NotEquals
            | BinaryOperatorKind::LessThan
            | BinaryOperatorKind::LessThanOrEquals
            | BinaryOperatorKind::GreaterThan
            | BinaryOperatorKind::GreaterThanOrEquals => 8,
            BinaryOperatorKind::BitwiseAnd => 7,
            BinaryOperatorKind::BitwiseOr => 5,
            BinaryOperatorKind::LogicalAnd => 4,
            BinaryOperatorKind::LogicalOr => 3,
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub operand: ExprId,
}

#[derive(Debug, Clone)]
pub struct UnaryOperator {
    pub kind: UnaryOperatorKind,
    pub token: SyntaxToken,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum UnaryOperatorKind {
    Identity,
    Negate,
    LogicalNot,
}

impl UnaryOperatorKind {
    pub fn priority(&self) -> usize {
        match self {
            UnaryOperatorKind::Identity
            | UnaryOperatorKind::Negate
            | UnaryOperatorKind::LogicalNot => 13,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub open_token: SyntaxToken,
    pub stmts: Vec<StmtId>,
    pub close_token: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct VariableExpr {
    pub token: SyntaxToken,
    pub variable_id: VariableId,
    pub typ: Type,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.token.literal)
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.token.literal)
    }
}
