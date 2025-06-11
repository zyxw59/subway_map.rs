use std::rc::Rc;

pub use expr_parser::expression;
use smol_str::SmolStr;

use crate::{
    error::MathError,
    evaluator::{evaluate_expression, EvaluationContext},
    operators::{BinaryOperator, UnaryOperator},
    parser::Position,
    values::{Result, Value},
};

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub expression: Rc<[ExpressionBit]>,
    pub num_args: usize,
}

impl Function {
    pub fn apply(&self, args: &[Value], context: &dyn EvaluationContext) -> Result<Value> {
        let expected = self.num_args;
        let actual = args.len();
        if expected != actual {
            return Err(MathError::Arguments { expected, actual });
        }
        let locals = FunctionEvaluator {
            parent: context,
            args,
        };
        evaluate_expression(&locals, self.expression.iter().cloned())
    }
}

pub struct FunctionEvaluator<'a> {
    parent: &'a dyn EvaluationContext,
    args: &'a [Value],
}

impl EvaluationContext for FunctionEvaluator<'_> {
    fn get_variable(&self, name: &str) -> Option<Value> {
        self.parent.get_variable(name)
    }

    fn get_fn_arg(&self, idx: usize) -> Option<Value> {
        self.args.get(idx).cloned()
    }
}

pub type Variable = SmolStr;

pub type ExpressionBit = expression::Expression<Position, BinaryOperator, UnaryOperator, Term>;
pub type Expression = Vec<ExpressionBit>;

pub fn zero_expression(span: crate::parser::Span) -> Expression {
    vec![ExpressionBit {
        span,
        kind: expression::ExpressionKind::Term(Term::Number(0.0)),
    }]
}

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Number(f64),
    String(String),
    Variable(Variable),
    FnArg(usize),
}

#[cfg(test)]
pub(crate) mod tests {
    use crate::{
        expressions::{ExpressionBit, Term},
        operators::{BinaryOperator, UnaryOperator},
    };

    pub type Expr = expr_parser::expression::ExpressionKind<BinaryOperator, UnaryOperator, Term>;

    pub fn expression_full<const N: usize>(expr: [Expr; N]) -> [ExpressionBit; N] {
        use crate::parser::{Position, Span};
        expr.map(|kind| ExpressionBit {
            kind,
            span: Span::new(Position::default()..Position::default()),
        })
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

    pub fn var(s: &str) -> Expr {
        Expr::Term(Term::Variable(s.into()))
    }

    pub fn t(t: impl Into<Term>) -> Expr {
        Expr::Term(t.into())
    }

    pub fn u(u: impl Into<UnaryOperator>) -> Expr {
        Expr::UnaryOperator(u.into())
    }

    pub fn dot(s: &str) -> Expr {
        Expr::UnaryOperator(UnaryOperator::FieldAccess(s.into()))
    }

    pub fn b(b: impl Into<BinaryOperator>) -> Expr {
        Expr::BinaryOperator(b.into())
    }
}
