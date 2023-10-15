use std::fmt::Display;

use colored::Colorize;

use crate::{id::Idx, id::IdxVec, idx, text::TextSpan};

use self::lexer::SyntaxToken;

pub mod lexer;
pub mod parser;
mod visitor;

idx!(ItemId);
idx!(StmtId);
idx!(ExprId);
// TODO: Move to compilation unit
idx!(VariableId);

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

    pub fn query_item(&self, id: ItemId) -> &Item {
        &self.items[id]
    }

    pub fn query_stmt(&self, id: StmtId) -> &Stmt {
        &self.statements[id]
    }

    pub fn query_expr(&self, id: ExprId) -> &Expr {
        &self.expressions[id]
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

    fn create_error_expr(&mut self, span: TextSpan) -> ExprId {
        self.create_expr(ExprKind::Error(span))
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
        self.create_expr(ExprKind::Unary(UnaryExpr { operator, expr }))
    }

    fn create_paren_expr(&mut self, open: SyntaxToken, expr: ExprId, close: SyntaxToken) -> ExprId {
        self.create_expr(ExprKind::Paren(ParenExpr { open, expr, close }))
    }

    fn create_integer_expr(&mut self, value: i32, token: SyntaxToken) -> ExprId {
        self.create_expr(ExprKind::Integer(IntegerExpr { value, token }))
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
    pub expr: ExprId,
}

#[derive(Debug, Clone)]
pub struct Expr {
    kind: ExprKind,
    id: ExprId,
    typ: Type,
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
    Paren(ParenExpr),
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
pub struct ParenExpr {
    pub open: SyntaxToken,
    pub expr: ExprId,
    pub close: SyntaxToken,
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
    pub expr: ExprId,
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
    pub open: SyntaxToken,
    pub stmts: Vec<StmtId>,
    pub close: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct VariableExpr {
    pub token: SyntaxToken,
    pub variable_id: VariableId,
    pub typ: Type,
}

// todo: print token from BinaryOperator
impl Display for BinaryOperatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BinaryOperatorKind::Add => "+",
            BinaryOperatorKind::Subtract => "-",
            BinaryOperatorKind::Mulitply => "*",
            BinaryOperatorKind::Divide => "/",
            BinaryOperatorKind::LogicalAnd => "&&",
            BinaryOperatorKind::LogicalOr => "||",
            BinaryOperatorKind::BitwiseAnd => "&",
            BinaryOperatorKind::BitwiseOr => "|",
            BinaryOperatorKind::Equals => "==",
            BinaryOperatorKind::NotEquals => "!=",
            BinaryOperatorKind::LessThan => "<",
            BinaryOperatorKind::LessThanOrEquals => "<=",
            BinaryOperatorKind::GreaterThan => ">",
            BinaryOperatorKind::GreaterThanOrEquals => ">=",
        };
        f.write_str(s)
    }
}

// todo: print token from UnaryOperator
impl Display for UnaryOperatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            UnaryOperatorKind::Identity => "+",
            UnaryOperatorKind::Negate => "-",
            UnaryOperatorKind::LogicalNot => "!",
        };
        f.write_str(s)
    }
}

// fn display_helper(
//     node: &AstNode,
//     f: &mut std::fmt::Formatter<'_>,
//     padding: &str,
//     is_last: bool,
//     is_root: bool,
// ) -> std::fmt::Result {
//     let last_marker = " └──";
//     let middle_marker = " ├──";
//     let marker = if is_root {
//         "───"
//     } else if is_last {
//         last_marker
//     } else {
//         middle_marker
//     };
//     let child_padding = if is_root {
//         "   ".to_owned()
//     } else if is_last {
//         padding.to_owned() + "    "
//     } else {
//         padding.to_owned() + " │  "
//     };
//
//     match node.kind.as_ref() {
//         AstNodeKind::BadNode => {
//             f.write_fmt(format_args!("{}{} {}\n", padding, marker, "ERROR".red()))
//         }
//         AstNodeKind::IntegerLiteral(i) => f.write_fmt(format_args!(
//             "{}{} {}\n",
//             padding,
//             marker,
//             i.to_string().blue()
//         )),
//         AstNodeKind::BooleanLiteral(b) => f.write_fmt(format_args!(
//             "{}{} {}\n",
//             padding,
//             marker,
//             b.to_string().bright_yellow()
//         )),
//         AstNodeKind::Identifier(s) => {
//             f.write_fmt(format_args!("{}{} {}\n", padding, marker, s.green()))
//         }
//         AstNodeKind::BinaryExpression(l, op, r) => {
//             f.write_fmt(format_args!(
//                 "{}{} {}\n",
//                 padding,
//                 marker,
//                 op.kind.to_string().white().on_truecolor(50, 50, 50)
//             ))?;
//             display_helper(&l, f, &child_padding, false, false)?;
//             display_helper(&r, f, &child_padding, true, false)
//         }
//         AstNodeKind::UnaryExpression(op, expr) => {
//             f.write_fmt(format_args!(
//                 "{}{} {}\n",
//                 padding,
//                 marker,
//                 op.kind.to_string().white().on_truecolor(50, 50, 50)
//             ))?;
//             display_helper(expr, f, &child_padding, true, false)
//         }
//         AstNodeKind::Scope(expr) => {
//             f.write_fmt(format_args!("{}{}{}\n", padding, marker, "{ }"))?;
//             display_helper(expr, f, &child_padding, true, false)
//         }
//         AstNodeKind::LetDeclaration(identifier, expr) => {
//             write!(f, "{}{} {}\n", padding, marker, "<let declaration>")?;
//             display_helper(identifier, f, &child_padding, false, false)?;
//             display_helper(expr, f, &child_padding, true, false)
//         }
//         AstNodeKind::Statement => todo!(),
//     }
// }
