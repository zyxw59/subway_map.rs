use std::collections::{HashMap, VecDeque};

pub use expr_parser::expression::Expression as ExpressionBit;

use crate::{
    error::MathError,
    evaluator::EvaluationContext,
    operators::{BinaryOperator, UnaryOperator},
    values::{Result, Value},
};

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: Variable,
    pub args: HashMap<Variable, usize>,
    pub expression: Expression2,
}

impl Function {
    pub fn apply(&self, args: Vec<Value>, context: &dyn EvaluationContext) -> Result<Value> {
        let expected = self.args.len();
        let actual = args.len();
        if expected != actual {
            return Err(MathError::Arguments {
                name: self.name.clone(),
                expected,
                actual,
            });
        }
        let locals = FunctionEvaluator {
            parent: context,
            arg_order: &self.args,
            args,
        };
        self.expression.evaluate(&locals)
    }
}

pub struct FunctionEvaluator<'a, 'b> {
    parent: &'a dyn EvaluationContext,
    arg_order: &'b HashMap<Variable, usize>,
    args: Vec<Value>,
}

impl<'a, 'b> EvaluationContext for FunctionEvaluator<'a, 'b> {
    fn get_variable(&self, name: &str) -> Option<Value> {
        self.arg_order
            .get(name)
            .and_then(|&i| self.args.get(i))
            .cloned()
            .or_else(|| self.parent.get_variable(name))
    }
}

pub type Variable = String;

pub type Expression2 = VecDeque<ExpressionBit<BinaryOperator, UnaryOperator, Term>>;

pub trait ExpressionExt {
    fn evaluate(&self, context: &impl EvaluationContext) -> Result<Value>;
}

impl ExpressionExt for Expression2 {
    fn evaluate(&self, _context: &impl EvaluationContext) -> Result<Value> {
        todo!();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {}

impl Expression {
    pub fn evaluate(&self, _context: &impl EvaluationContext) -> Result<Value> {
        todo!();
        // Ok(match self {
        //     Expression::Value(v) => v.clone(),
        //     Expression::Point(p) => {
        //         let (x, y) = p.as_ref();
        //         Value::point(x.evaluate(context)?, y.evaluate(context)?)?
        //     }
        //     // Expression::BinaryOperator(op, args) => {
        //     //     let (lhs, rhs) = args.as_ref();
        //     //     op.apply(lhs.evaluate(context)?, rhs.evaluate(context)?)?
        //     // }
        //     // Expression::UnaryOperator(op, arg) => op.apply(arg.evaluate(context)?)?,
        //     Expression::Function(func_name, args) => match context.get_function(func_name) {
        //         None => return Err(MathError::Function(func_name.clone())),
        //         Some(func) => func.apply(args, context)?,
        //     },
        //     Expression::Variable(var) => match context.get_variable(var) {
        //         None => return Err(MathError::Variable(var.clone())),
        //         Some(val) => val,
        //     },
        // })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Number(f64),
    String(String),
    Variable(Variable),
}

#[cfg(test)]
pub(crate) mod tests {
    use expr_parser::expression::ExpressionKind;

    use crate::{
        expressions::Term,
        operators::{BinaryOperator, UnaryOperator},
    };

    macro_rules! expression {
        ($($x:expr),* $(,)?) => {
            [$(::expr_parser::expression::ExpressionKind::from(
                    $crate::expressions::tests::Expr::from($x),
            )),*]
        };
    }

    pub(crate) use expression;

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
