pub use expr_parser::expression;
use smol_str::SmolStr;

use crate::{
    error::MathError,
    evaluator::{evaluate_expression, EvaluationContext},
    operators::{BinaryOperator, UnaryOperator},
    values::{Result, Value},
};

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub expression: Expression,
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

impl<'a> EvaluationContext for FunctionEvaluator<'a> {
    fn get_variable(&self, name: &str) -> Option<Value> {
        self.parent.get_variable(name)
    }

    fn get_fn_arg(&self, idx: usize) -> Option<Value> {
        self.args.get(idx).cloned()
    }
}

pub type Variable = SmolStr;

pub type ExpressionBit = expression::Expression<BinaryOperator, UnaryOperator, Term>;
pub type Expression = Vec<ExpressionBit>;

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Number(f64),
    String(String),
    Variable(Variable),
    FnArg(usize),
}

#[cfg(test)]
pub(crate) mod tests {
    use expr_parser::expression::ExpressionKind;

    use crate::{
        expressions::Term,
        operators::{BinaryOperator, UnaryOperator},
    };

    macro_rules! expression_map {
        ($id:ident => $mapped:expr; $($x:expr),* $(,)?) => {
            [$({
                let $id = ::expr_parser::expression::ExpressionKind::from(
                    $crate::expressions::tests::Expr::from($x),
                );
                $mapped
            }),*]
        };
    }

    macro_rules! expression {
        ($($x:expr),* $(,)?) => {
            $crate::expressions::tests::expression_map!(expr => expr; $($x),*)
        };
    }

    macro_rules! expression_full {
        ($($x:expr),* $(,)?) => {
            $crate::expressions::tests::expression_map!(kind => ::expr_parser::expression::Expression {
                kind,
                span: ::expr_parser::Span::new(0..0),
            }; $($x),*)
        };
    }

    pub(crate) use {expression, expression_full, expression_map};

    pub enum Expr {
        Term(Term),
        UnaryOperator(UnaryOperator),
        BinaryOperator(BinaryOperator),
    }

    impl From<Expr> for ExpressionKind<BinaryOperator, UnaryOperator, Term> {
        fn from(expr: Expr) -> Self {
            match expr {
                Expr::Term(t) => Self::Term(t),
                Expr::UnaryOperator(u) => Self::UnaryOperator(u),
                Expr::BinaryOperator(b) => Self::BinaryOperator(b),
            }
        }
    }

    impl From<f64> for Expr {
        fn from(x: f64) -> Self {
            Self::Term(Term::Number(x))
        }
    }

    impl From<i32> for Expr {
        fn from(x: i32) -> Self {
            Self::Term(Term::Number(x as _))
        }
    }

    impl From<BinaryOperator> for Expr {
        fn from(op: BinaryOperator) -> Self {
            Self::BinaryOperator(op)
        }
    }

    impl From<UnaryOperator> for Expr {
        fn from(op: UnaryOperator) -> Self {
            Self::UnaryOperator(op)
        }
    }

    impl From<&str> for Expr {
        fn from(s: &str) -> Self {
            Self::Term(Term::String(s.into()))
        }
    }

    pub fn b(s: &str) -> Expr {
        Expr::BinaryOperator(BinaryOperator::get(s).unwrap().1)
    }

    pub fn u(s: &str) -> Expr {
        Expr::UnaryOperator(UnaryOperator::get(s).unwrap().1)
    }

    pub fn var(s: &str) -> Expr {
        Expr::Term(Term::Variable(s.into()))
    }
}
