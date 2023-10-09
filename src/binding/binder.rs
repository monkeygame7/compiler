use crate::parse::lexer::{SyntaxToken, TokenKind};
use crate::parse::parser::{SyntaxKind, SyntaxNode, SyntaxTree};

enum Type {
    Integer(Box<dyn BoundNode<i32>>),
}

impl Type {
    fn integer(item: impl BoundNode<i32> + 'static) -> Type {
        Type::Integer(Box::new(item))
    }
}

pub struct BoundLiteralNode<T>(pub T);

pub struct BoundAdditionNode {
    pub left: Box<dyn BoundNode<i32>>,
    pub right: Box<dyn BoundNode<i32>>,
}

pub struct BoundSubtractionNode {
    pub left: Box<dyn BoundNode<i32>>,
    pub right: Box<dyn BoundNode<i32>>,
}

pub struct BoundIdentityNode {
    pub exp: Box<dyn BoundNode<i32>>,
}

pub struct BoundNegationNode {
    pub exp: Box<dyn BoundNode<i32>>,
}

pub trait BoundNode<T> {}

impl<T> BoundNode<T> for BoundLiteralNode<T> {}
impl BoundNode<i32> for BoundAdditionNode {}
impl BoundNode<i32> for BoundSubtractionNode {}
impl BoundNode<i32> for BoundIdentityNode {}
impl BoundNode<i32> for BoundNegationNode {}

pub struct BoundTree {
    root: Type,
}

struct Binder;

impl Binder {
    pub fn bind(cst: SyntaxTree) -> BoundTree {
        let root = Binder::bind_expression(cst.root);
        let mut foo = 4;
        foo = foo + 5 * foo - 3;
        BoundTree { root }
    }

    fn bind_expression(root: SyntaxNode) -> Type {
        match root.kind {
            SyntaxKind::BadExpression => panic!("what to do with bad expression"),
            SyntaxKind::IntegerExpression(_, i) => Binder::bind_integer(i),
            SyntaxKind::BinaryExpression(left, op, right) => {
                Binder::bind_binary_expression(*left, op, *right)
            }
            SyntaxKind::UnaryExpression(op, exp) => Binder::bind_unary_expression(op, *exp),
            SyntaxKind::GroupExpression(_, exp, _) => Binder::bind_expression(*exp),
        }
    }

    fn bind_integer(value: i32) -> Type {
        Type::integer(BoundLiteralNode(value))
    }

    fn bind_binary_expression(left: SyntaxNode, op: SyntaxToken, right: SyntaxNode) -> Type {
        let left = Binder::bind_expression(left);
        let right = Binder::bind_expression(right);

        match op.kind {
            TokenKind::PlusToken(_) => Binder::bind_addition(left, right),
            TokenKind::DashToken(_) => Binder::bind_subtraction(left, right),
            _ => panic!("Unsupported binary operation"),
        }
    }

    fn bind_unary_expression(op: SyntaxToken, exp: SyntaxNode) -> Type {
        let value = Binder::bind_expression(exp);
        match op.kind {
            TokenKind::PlusToken(_) => Binder::bind_identity(value),
            TokenKind::DashToken(_) => Binder::bind_negation(value),
            _ => panic!("Unuspported unary operation"),
        }
    }

    fn bind_addition(left: Type, right: Type) -> Type {
        match (left, right) {
            (Type::Integer(l), Type::Integer(r)) => {
                Type::integer(BoundAdditionNode { left: l, right: r })
            }
            _ => panic!("unsupported operation + between types"),
        }
    }

    fn bind_subtraction(left: Type, right: Type) -> Type {
        match (left, right) {
            (Type::Integer(l), Type::Integer(r)) => {
                Type::integer(BoundSubtractionNode { left: l, right: r })
            }
            _ => panic!("unsupported operation + between types"),
        }
    }

    fn bind_identity(value: Type) -> Type {
        match value {
            Type::Integer(exp) => Type::integer(BoundIdentityNode { exp }),
            _ => panic!("identity not supported for type"),
        }
    }

    fn bind_negation(value: Type) -> Type {
        match value {
            Type::Integer(exp) => Type::integer(BoundNegationNode { exp }),
            _ => panic!("identity not supported for type"),
        }
    }
}

fn test() {
    let left = BoundLiteralNode(32);
    let right = BoundLiteralNode(23);
    let kind = BoundAdditionNode {
        left: Box::new(left),
        right: Box::new(right),
    };

    let result = Type::Integer(Box::new(kind));
    let extra = match result {
        Type::Integer(node) => BoundAdditionNode {
            left: Box::new(BoundLiteralNode(32)),
            right: node,
        },
        _ => todo!(),
    };
}
