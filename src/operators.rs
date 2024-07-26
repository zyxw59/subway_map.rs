use std::fmt;

use crate::{
    evaluator::EvaluationContext,
    expressions::Variable,
    values::{Result, Value},
};

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Precedence {
    /// The `,` operator
    Comma,
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

macro_rules! as_binary_operator {
    ($fn:path) => {{
        fn _f(a: Value, b: Value, _: &dyn EvaluationContext) -> Result {
            $fn(a, b)
        }
        _f
    }};
}

macro_rules! as_unary_operator {
    ($fn:path) => {{
        fn _f(a: Value, _: &dyn EvaluationContext) -> Result {
            $fn(a)
        }
        _f
    }};
}

pub const COMMA: BinaryOperator = BinaryOperator {
    function: as_binary_operator!(Value::comma),
    name: Variable::new_static(","),
};

pub const COMMA_UNARY: UnaryOperator = UnaryOperator::Function(NamedFunction {
    function: as_unary_operator!(Value::comma_unary),
    name: Variable::new_static(","),
});

pub const FN_CALL: BinaryOperator = BinaryOperator {
    function: Value::fn_call,
    name: Variable::new_static("()"),
};

pub const FN_CALL_UNARY: UnaryOperator = UnaryOperator::Function(NamedFunction {
    function: Value::fn_call_unary,
    name: Variable::new_static("()"),
});

pub const PAREN_UNARY: UnaryOperator = UnaryOperator::Function(NamedFunction {
    function: as_unary_operator!(Value::paren_unary),
    name: Variable::new_static("()"),
});

macro_rules! get_binary_builtin {
    (match $key:ident { $($name:literal => ($fixity:expr, $fn:ident)),* $(,)? }) => {
        match $key {
            $($name => Some(($fixity, BinaryOperator {
                function: as_binary_operator!(Value::$fn),
                name: $name.into(),
            })),)*
            _ => None,
        }
    }
}

macro_rules! get_unary_builtin {
    (match $key:ident { $($name:literal => ($prec:ident, $fn:ident)),* $(,)? }) => {
        match $key {
            $($name => Some((Precedence::$prec, UnaryOperator::Function(NamedFunction {
                function: as_unary_operator!(Value::$fn),
                name: $name.into(),
            }))),)*
            _ => None,
        }
    }
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

pub type BinaryOperator = NamedFunction<fn(Value, Value, &dyn EvaluationContext) -> Result>;

impl BinaryOperator {
    pub fn get(key: &str) -> Option<(expr_parser::operator::Fixity<Precedence>, Self)> {
        use self::Precedence::*;
        use expr_parser::operator::Fixity::*;
        get_binary_builtin!(match key {
            "==" => (Left(Comparison), eq),
            "!=" => (Left(Comparison), ne),
            "<" => (Left(Comparison), lt),
            "<=" => (Left(Comparison), le),
            ">" => (Left(Comparison), gt),
            ">=" => (Left(Comparison), ge),
            "max" => (Left(Comparison), max),
            "min" => (Left(Comparison), min),
            "+" => (Left(Additive), add),
            "-" => (Left(Additive), sub),
            "++" => (Left(Additive), hypot),
            "+-+" => (Left(Additive), hypot_sub),
            "*" => (Left(Multiplicative), mul),
            "/" => (Left(Multiplicative), div),
            "&" => (Left(Multiplicative), intersect),
            "^" => (Right(Exponential), pow),
            "<>" => (Left(Exponential), line_between),
            ">>" => (Left(Exponential), line_vector),
            "^^" => (Left(Exponential), line_offset),
        })
    }
}

#[derive(Clone, Eq, PartialEq)]
pub enum UnaryOperator {
    Function(NamedFunction<fn(Value, &dyn EvaluationContext) -> Result>),
    FieldAccess(Variable),
}

impl UnaryOperator {
    pub fn call(&self, value: Value, ctx: &dyn EvaluationContext) -> Result {
        match self {
            Self::Function(f) => (f.function)(value, ctx),
            Self::FieldAccess(field) => todo!(),
        }
    }

    pub fn get(key: &str) -> Option<(Precedence, Self)> {
        get_unary_builtin!(match key {
            "-" => (Multiplicative, neg),
            "xpart" => (Multiplicative, xpart),
            "ypart" => (Multiplicative, ypart),
            "cos" => (Exponential, cos),
            "sin" => (Exponential, sin),
            "dir" => (Exponential, dir),
            "angle" => (Exponential, angle),
        })
    }

    pub fn field_access(name: Variable) -> Self {
        Self::FieldAccess(name)
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
