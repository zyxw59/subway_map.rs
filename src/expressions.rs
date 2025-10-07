use std::rc::Rc;

use smol_str::SmolStr;

use crate::{
    error::MathError,
    evaluator::{evaluate_expression, EvaluationContext},
    operators::{BinaryOperator, UnaryOperator},
    parser::{Position, Span},
    points::PointCollection,
    values::{Result, Value},
};

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub expression: Expression,
    pub num_args: usize,
}

impl Function {
    pub fn apply(&self, args: &[Value], context: &mut dyn EvaluationContext) -> Result<Value> {
        let expected = self.num_args;
        let actual = args.len();
        if expected != actual {
            return Err(MathError::Arguments { expected, actual });
        }
        let mut locals = FunctionEvaluator {
            parent: context,
            args,
        };
        evaluate_expression(&mut locals, &self.expression)
    }
}

pub struct FunctionEvaluator<'a> {
    parent: &'a mut dyn EvaluationContext,
    args: &'a [Value],
}

impl EvaluationContext for FunctionEvaluator<'_> {
    fn get_variable(&self, name: &str) -> Option<Value> {
        self.parent.get_variable(name)
    }

    fn get_fn_arg(&self, idx: usize) -> Option<Value> {
        self.args.get(idx).cloned()
    }

    fn point_collection(&mut self) -> &mut PointCollection {
        self.parent.point_collection()
    }
}

pub type Variable = SmolStr;

pub fn zero_expression(span: crate::parser::Span) -> Expression {
    use expr_parser::evaluate::ExpressionTree;
    Expression::from_node(
        span,
        ExpressionNode::Term {
            value: Term::Number(0.0),
        },
    )
}

#[derive(Clone)]
pub struct Expression {
    pub inner_span: Span,
    pub outer_span: Span,
    pub node: Rc<ExpressionNode>,
}

/// Equality ignores spans
impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &*self.node {
            ExpressionNode::Binary {
                operator,
                left,
                right,
            } => {
                f.write_str("'")?;
                operator.fmt(f)?;
                f.write_str("'(")?;
                left.fmt(f)?;
                f.write_str(", ")?;
                right.fmt(f)?;
                f.write_str(")")
            }
            ExpressionNode::Unary { operator, argument } => {
                f.write_str("'")?;
                operator.fmt(f)?;
                f.write_str("'(")?;
                argument.fmt(f)?;
                f.write_str(")")
            }
            ExpressionNode::Term { value } => value.fmt(f),
        }
    }
}

impl expr_parser::evaluate::ExpressionTree<Position, BinaryOperator, UnaryOperator, Term>
    for Expression
{
    fn from_node(inner_span: Span, node: ExpressionNode) -> Self {
        let outer_span = match &node {
            ExpressionNode::Binary { left, right, .. } => {
                left.outer_span.join(right.outer_span).join(inner_span)
            }
            ExpressionNode::Unary { argument, .. } => argument.outer_span.join(inner_span),
            ExpressionNode::Term { .. } => inner_span,
        };
        Self {
            inner_span,
            outer_span,
            node: Rc::new(node),
        }
    }
}

pub type ExpressionNode =
    expr_parser::evaluate::ExpressionNode<Expression, BinaryOperator, UnaryOperator, Term>;

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Boolean(bool),
    Number(f64),
    String(String),
    Variable(Variable),
    FnArg(usize),
}

impl Term {
    pub fn get_primitive(tag: &str) -> Option<Self> {
        match tag {
            "true" => Some(Term::Boolean(true)),
            "false" => Some(Term::Boolean(false)),
            _ => None,
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use expr_parser::evaluate::ExpressionTree;

    use crate::{
        expressions::{Expression, ExpressionNode, Term},
        operators::{BinaryOperator, UnaryOperator},
        parser::{Position, Span},
    };

    const EMPTY_SPAN: Span = Span {
        start: Position::ZERO,
        end: Position::ZERO,
    };

    impl From<bool> for Term {
        fn from(x: bool) -> Self {
            Term::Boolean(x)
        }
    }

    impl From<f64> for Term {
        fn from(x: f64) -> Self {
            Term::Number(x)
        }
    }

    impl From<i32> for Term {
        fn from(x: i32) -> Self {
            Term::Number(x as _)
        }
    }

    impl From<&str> for Term {
        fn from(s: &str) -> Self {
            Term::String(s.into())
        }
    }

    impl From<&str> for UnaryOperator {
        fn from(s: &str) -> Self {
            Self::get(s).unwrap().1
        }
    }

    impl From<&str> for BinaryOperator {
        fn from(s: &str) -> Self {
            Self::get(s).unwrap().1
        }
    }

    pub fn var(s: &str) -> Expression {
        Expression::from_node(
            EMPTY_SPAN,
            ExpressionNode::Term {
                value: Term::Variable(s.into()),
            },
        )
    }

    pub fn t(t: impl Into<Term>) -> Expression {
        Expression::from_node(EMPTY_SPAN, ExpressionNode::Term { value: t.into() })
    }

    pub fn u(u: impl Into<UnaryOperator>, argument: Expression) -> Expression {
        Expression::from_node(
            EMPTY_SPAN,
            ExpressionNode::Unary {
                operator: u.into(),
                argument,
            },
        )
    }

    pub fn dot(s: &str, argument: Expression) -> Expression {
        Expression::from_node(
            EMPTY_SPAN,
            ExpressionNode::Unary {
                operator: UnaryOperator::FieldAccess(s.into()),
                argument,
            },
        )
    }

    pub fn b(b: impl Into<BinaryOperator>, left: Expression, right: Expression) -> Expression {
        Expression::from_node(
            EMPTY_SPAN,
            ExpressionNode::Binary {
                operator: b.into(),
                left,
                right,
            },
        )
    }
}
