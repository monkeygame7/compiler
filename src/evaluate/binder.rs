use std::{any::Any, marker::PhantomData};

use crate::parse::parser::{
    BinaryOperatorKind, BinaryOperatorToken, Program, SyntaxKind, SyntaxNode,
};

enum Type {
    Integer(Box<dyn BoundNode<i32>>),
}

impl Type {
    fn integer(item: impl BoundNode<i32> + 'static) -> Type {
        Type::Integer(Box::new(item))
    }
}

struct BoundLiteralNode<T>(T);

struct BoundAdditionNode {
    left: Box<dyn BoundNode<i32>>,
    right: Box<dyn BoundNode<i32>>,
}

struct BoundSubtractionNode {
    left: Box<dyn BoundNode<i32>>,
    right: Box<dyn BoundNode<i32>>,
}

trait BoundNode<T> {}

impl<T> BoundNode<T> for BoundLiteralNode<T> {}
impl BoundNode<i32> for BoundAdditionNode {}
impl BoundNode<i32> for BoundSubtractionNode {}

struct BoundTree {
    root: Type,
}

struct Binder;

impl Binder {
    pub fn bind(program: Program) -> BoundTree {
        let root = Binder::bind_expression(program.root);
        BoundTree { root }
    }

    fn bind_expression(root: SyntaxNode) -> Type {
        match root.kind {
            SyntaxKind::BadExpression => panic!("what to do with bad expression"),
            SyntaxKind::IntegerExpression(i) => Binder::bind_integer(i),
            SyntaxKind::BinaryExpression(left, op, right) => {
                Binder::bind_binary_expression(*left, op, *right)
            }
            SyntaxKind::UnaryExpression(op, exp) => Binder::bind_unary_expression(op, *exp),
            SyntaxKind::GroupExpression(_, exp, _) => Binder::bind_expression(*exp),
        }
    }

    fn bind_integer(i: i32) -> Type {
        Type::Integer(Box::new(BoundLiteralNode(i)))
    }

    fn bind_binary_expression(
        left: SyntaxNode,
        op: BinaryOperatorToken,
        right: SyntaxNode,
    ) -> Type {
        let left = Binder::bind_expression(left);
        let right = Binder::bind_expression(right);

        match op.kind {
            BinaryOperatorKind::Addition => Binder::bind_addition(left, right),
            BinaryOperatorKind::Subtraction => Binder::bind_subtraction(left, right),
            _ => panic!("Unsupported operation"),
        }
    }

    fn bind_unary_expression(
        op: crate::parse::parser::UnaryOperatorToken,
        exp: SyntaxNode,
    ) -> Type {
        todo!()
    }

    fn bind_addition(left: Type, right: Type) -> Type {
        match (left, right) {
            (Type::Integer(l), Type::Integer(r)) => {
                Type::integer(BoundAdditionNode{left: l, right: r})
            },
            _ => panic!("unsupported operation + between types"),
        }
    }

    fn bind_subtraction(left: Type, right: Type) -> Type {
        match (left, right) {
            (Type::Integer(l), Type::Integer(r)) => {
                Type::integer(BoundSubtractionNode{left: l, right: r})
            },
            _ => panic!("unsupported operation + between types"),
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
