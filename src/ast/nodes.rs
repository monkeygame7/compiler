use std::fmt::Display;

use crate::{
    compilation::{FunctionId, Type, VariableId},
    diagnostics::TextSpan,
    parsing::SyntaxToken,
};

use super::{ExprId, ItemId, StmtId};

#[derive(Debug, Clone)]
pub struct Item {
    pub kind: ItemKind,
    pub id: ItemId,
}

impl Item {
    pub fn new(kind: ItemKind) -> Self {
        Self {
            kind,
            id: ItemId::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Stmt(StmtId),
    Func(FunctionDecl),
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub id: StmtId,
    pub span: TextSpan,
}

impl Stmt {
    pub fn new(kind: StmtKind, span: TextSpan) -> Self {
        Self {
            kind,
            id: StmtId::default(),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Expr(ExprStmt),
    If(ExprId),
    Decl(VariableDecl),
    While(WhileStmt),
    Return(ReturnStmt),
}

#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub expr: ExprId,
    pub semicolon: Option<SyntaxToken>,
}

#[derive(Debug, Clone)]
pub struct VariableDecl {
    pub keyword: SyntaxToken,
    pub is_mutable: bool,
    pub identifier: SyntaxToken,
    pub variable: VariableId,
    pub type_decl: Option<TypeDecl>,
    pub equals_token: SyntaxToken,
    pub initial: ExprId,
    pub semicolon: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub keyword: SyntaxToken,
    pub condition: ExprId,
    pub body: ExprId,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub keyword: SyntaxToken,
    pub value: Option<ExprId>,
    pub semicolon: SyntaxToken,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub id: ExprId,
    pub typ: Type,
    pub span: TextSpan,
}

impl Expr {
    pub fn new(kind: ExprKind, span: TextSpan) -> Self {
        Self {
            kind,
            id: ExprId::default(),
            typ: Type::Unresolved,
            span,
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
    If(IfExpr),
    CallExpr(CallExpr),
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
pub struct TypeDecl {
    pub colon: SyntaxToken,
    pub typ: SyntaxToken,
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
    pub id: VariableId,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub keyword: SyntaxToken,
    pub condition: ExprId,
    pub then_clause: ExprId,
    pub else_clause: Option<ElseClause>,
}

#[derive(Debug, Clone)]
pub struct ElseClause {
    pub keyword: SyntaxToken,
    pub body: ExprId,
    pub span: TextSpan,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub keyword: SyntaxToken,
    pub name: SyntaxToken,
    pub return_type: Option<TypeDecl>,
    pub open_paren: SyntaxToken,
    pub params: DelimitedSequence<FunctionParam>,
    pub close_paren: SyntaxToken,
    pub body: ExprId,
    pub id: FunctionId,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub token: SyntaxToken,
    pub id: VariableId,
    pub type_decl: TypeDecl,
}

impl FunctionParam {
    pub fn new(token: SyntaxToken, type_decl: TypeDecl) -> Self {
        Self {
            token,
            id: VariableId::default(),
            type_decl,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub callee: ExprId,
    pub open_paren: SyntaxToken,
    pub args: DelimitedSequence<ExprId>,
    pub close_paren: SyntaxToken,
    pub callee_id: FunctionId,
}

impl CallExpr {
    pub fn new(
        callee: ExprId,
        open_paren: SyntaxToken,
        args: DelimitedSequence<ExprId>,
        close_paren: SyntaxToken,
    ) -> Self {
        Self {
            callee,
            open_paren,
            args,
            close_paren,
            callee_id: FunctionId::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct DelimitedItem<T> {
    pub item: T,
    pub delim: Option<SyntaxToken>,
}

#[derive(Debug, Clone)]
pub struct DelimitedSequence<T> {
    pub items: Vec<DelimitedItem<T>>,
}

impl<T> DelimitedSequence<T> {
    pub fn new() -> Self {
        DelimitedSequence { items: vec![] }
    }

    pub fn push(&mut self, item: T, comma: Option<SyntaxToken>) {
        self.items.push(DelimitedItem { item, delim: comma })
    }
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
