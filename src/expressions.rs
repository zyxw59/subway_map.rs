use std::collections::HashMap;

use crate::error::MathError;
use crate::evaluator::EvaluationContext;
use crate::operators::{BinaryOperator, UnaryOperator};
use crate::values::Value;

pub type EResult<T> = Result<T, MathError>;

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: Variable,
    pub args: HashMap<Variable, usize>,
    pub expression: Expression,
}

impl Function {
    fn apply(&self, args: &[Expression], context: &impl EvaluationContext) -> EResult<Value> {
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
            args: args
                .iter()
                .map(|arg| arg.evaluate(context))
                .collect::<EResult<Vec<Value>>>()?,
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
            .copied()
            .or_else(|| self.parent.get_variable(name))
    }

    fn get_function(&self, name: &str) -> Option<&Function> {
        self.parent.get_function(name)
    }
}

pub type Variable = String;

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Value(Value),
    Point(Box<(Expression, Expression)>),
    BinaryOperator(
        &'static BinaryOperator<'static>,
        Box<(Expression, Expression)>,
    ),
    UnaryOperator(&'static UnaryOperator<'static>, Box<Expression>),
    Function(Variable, Vec<Expression>),
    Variable(Variable),
}

impl Expression {
    pub fn evaluate(&self, context: &impl EvaluationContext) -> EResult<Value> {
        Ok(match self {
            Expression::Value(v) => *v,
            Expression::Point(p) => {
                let (x, y) = p.as_ref();
                Value::point(x.evaluate(context)?, y.evaluate(context)?)?
            }
            Expression::BinaryOperator(op, args) => {
                let (lhs, rhs) = args.as_ref();
                op.apply(lhs.evaluate(context)?, rhs.evaluate(context)?)?
            }
            Expression::UnaryOperator(op, arg) => op.apply(arg.evaluate(context)?)?,
            Expression::Function(func_name, args) => match context.get_function(func_name) {
                None => return Err(MathError::Function(func_name.clone())),
                Some(func) => func.apply(args, context)?,
            },
            Expression::Variable(var) => match context.get_variable(var) {
                None => return Err(MathError::Variable(var.clone())),
                Some(val) => val,
            },
        })
    }
}
