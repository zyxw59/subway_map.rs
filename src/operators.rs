use std::fmt;

use crate::{
    evaluator::EvaluationContext,
    expressions::{Expression, Variable},
    values::{Result, Value},
};

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Precedence {
    /// The `,` operator
    Comma,
    /// Short-circuiting `or`
    Or,
    /// Short-circuiting `and`
    And,
    /// Comparison operators, such as `==`, `>`, `<>`, etc.
    Comparison,
    /// Additive operators, such as `+`, `-`, `++`, etc.
    Additive,
    /// Multiplicative operators, such as `*`, `/`, `&`, etc., as well as unary minus.
    Multiplicative,
    /// Exponential operators, such as `^`, as well as unary sine and cosine.
    Exponential,
    /// Field access operators
    Field,
}

#[derive(Clone, Eq, PartialEq)]
pub struct NamedFunction<F> {
    pub function: F,
    pub name: Variable,
}

impl<F> fmt::Debug for NamedFunction<F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.name)
    }
}

type EvalCtx<'a> = &'a mut dyn EvaluationContext;

#[derive(Clone, Copy, PartialEq)]
pub enum LazyOrStrict<L, S> {
    Lazy(L),
    Strict(S),
}

impl<L, S> LazyOrStrict<L, S> {
    pub fn call<T>(self, expr: &Expression, ctx: EvalCtx) -> Result<T>
    where
        L: FnOnce(Expression, EvalCtx) -> Result<T>,
        S: FnOnce(Value, EvalCtx) -> Result<T>,
    {
        match self {
            Self::Lazy(function) => function(expr.clone(), ctx),
            Self::Strict(function) => {
                let val = crate::evaluator::evaluate_expression(ctx, expr)?;
                function(val, ctx)
            }
        }
    }
}

macro_rules! strict {
    ($body:expr) => {
        LazyOrStrict::Strict(|a, _ctx| {
            Ok(LazyOrStrict::Strict(Box::new(move |b, ctx| {
                let f: fn(Value, Value, EvalCtx) -> Result = $body;
                f(a, b, ctx)
            })))
        })
    };
}

pub type BinaryOperator = NamedFunction<LazyOrStrict<BinaryFn<Expression>, BinaryFn<Value>>>;
pub type BinaryFn<T> = fn(T, EvalCtx) -> Result<CurriedBinaryOperator>;

pub type CurriedBinaryOperator = LazyOrStrict<StackUnaryFn<Expression>, StackUnaryFn<Value>>;
pub type StackUnaryFn<T> = Box<dyn FnOnce(T, EvalCtx) -> Result>;

pub const COMMA: BinaryOperator = BinaryOperator {
    function: strict!(|a, b, _| a.comma(b)),
    name: Variable::new_static(","),
};

pub const COMMA_UNARY: UnaryOperator = UnaryOperator::Function(NamedFunction {
    function: |a, _| a.comma_unary(),
    name: Variable::new_static(","),
});

pub const FN_CALL: BinaryOperator = BinaryOperator {
    function: strict!(Value::fn_call),
    name: Variable::new_static("()"),
};

pub const FN_CALL_UNARY: UnaryOperator = UnaryOperator::Function(NamedFunction {
    function: Value::fn_call_unary,
    name: Variable::new_static("()"),
});

pub const PAREN_UNARY: UnaryOperator = UnaryOperator::Function(NamedFunction {
    function: Value::paren_unary,
    name: Variable::new_static("()"),
});

macro_rules! get_binary_builtin {
    (match $key:ident { $($name:literal => ($fixity:expr, $fn:expr)),* $(,)? }) => {
        match $key {
            $($name => Some(($fixity, BinaryOperator {
                function: $fn,
                name: $name.into(),
            })),)*
            _ => None,
        }
    }
}

macro_rules! get_unary_builtin {
    (match $key:ident { $($name:literal => ($prec:ident, $fn:expr)),* $(,)? }) => {
        match $key {
            $($name => Some((Precedence::$prec, UnaryOperator::Function(NamedFunction {
                function: $fn,
                name: $name.into(),
            }))),)*
            _ => None,
        }
    }
}

impl BinaryOperator {
    pub fn get(key: &str) -> Option<(expr_parser::operator::Fixity<Precedence>, Self)> {
        use self::Precedence::*;
        use expr_parser::operator::Fixity::*;
        get_binary_builtin!(match key {
            "==" => (Left(Comparison), strict!(|a, b, _| a.eq(b))),
            "!=" => (Left(Comparison), strict!(|a, b, _| a.ne(b))),
            "<" => (Left(Comparison), strict!(|a, b, _| a.lt(b))),
            "<=" => (Left(Comparison), strict!(|a, b, _| a.le(b))),
            ">" => (Left(Comparison), strict!(|a, b, _| a.gt(b))),
            ">=" => (Left(Comparison), strict!(|a, b, _| a.ge(b))),
            "max" => (Left(Comparison), strict!(Value::max)),
            "min" => (Left(Comparison), strict!(Value::min)),
            "+" => (Left(Additive), strict!(Value::add)),
            "-" => (Left(Additive), strict!(Value::sub)),
            "++" => (Left(Additive), strict!(|a, b, _| a.hypot(b))),
            "+-+" => (Left(Additive), strict!(|a, b, _| a.hypot_sub(b))),
            "*" => (Left(Multiplicative), strict!(Value::mul)),
            "/" => (Left(Multiplicative), strict!(Value::div)),
            "&" => (Left(Multiplicative), strict!(Value::intersect)),
            "^" => (Right(Exponential), strict!(|a, b, _| a.pow(b))),
            "<>" => (Left(Exponential), strict!(Value::line_between)),
            ">>" => (Left(Exponential), strict!(Value::line_vector)),
            "^^" => (Left(Exponential), strict!(Value::line_offset)),
            "and" => (
                Left(And),
                LazyOrStrict::Strict(|a, _| if a.is_truthy()? {
                    Ok(LazyOrStrict::Strict(Box::new(|b, _| Ok(b))))
                } else {
                    Ok(LazyOrStrict::Lazy(Box::new(|_, _| Ok(a))))
                })
            ),
            "or" => (
                Left(Or),
                LazyOrStrict::Strict(|a, _| if !a.is_truthy()? {
                    Ok(LazyOrStrict::Strict(Box::new(|b, _| Ok(b))))
                } else {
                    Ok(LazyOrStrict::Lazy(Box::new(|_, _| Ok(a))))
                })
            ),
        })
    }
}

#[derive(Clone, Eq, PartialEq)]
pub enum UnaryOperator {
    Function(NamedFunction<fn(Value, &mut dyn EvaluationContext) -> Result>),
    FieldAccess(Variable),
}

impl UnaryOperator {
    pub fn call(&self, value: Value, ctx: &mut dyn EvaluationContext) -> Result {
        match self {
            Self::Function(f) => (f.function)(value, ctx),
            Self::FieldAccess(field) => value.field_access(field.clone()),
        }
    }

    pub fn get(key: &str) -> Option<(Precedence, Self)> {
        get_unary_builtin!(match key {
            "-" => (Multiplicative, Value::neg),
            "xpart" => (Multiplicative, |a, _| a.xpart()),
            "ypart" => (Multiplicative, |a, _| a.ypart()),
            "cos" => (Exponential, |a, _| a.cos()),
            "sin" => (Exponential, |a, _| a.sin()),
            "dir" => (Exponential, Value::dir),
            "angle" => (Exponential, |a, _| a.angle()),
        })
    }
}

impl fmt::Debug for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Function(func) => fmt::Debug::fmt(func, f),
            Self::FieldAccess(field) => write!(f, ".{field}"),
        }
    }
}
