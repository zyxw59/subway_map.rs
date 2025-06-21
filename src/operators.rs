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
        fn _f(a: Value, b: Value, _: &mut dyn EvaluationContext) -> Result {
            $fn(a, b)
        }
        _f
    }};
}

macro_rules! as_unary_operator {
    ($fn:path) => {{
        fn _f(a: Value, _: &mut dyn EvaluationContext) -> Result {
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

pub type BinaryOperator = NamedFunction<fn(Value, Value, &mut dyn EvaluationContext) -> Result>;

impl BinaryOperator {
    pub fn get(key: &str) -> Option<(expr_parser::operator::Fixity<Precedence>, Self)> {
        use self::Precedence::*;
        use expr_parser::operator::Fixity::*;
        get_binary_builtin!(match key {
            "==" => (Left(Comparison), as_binary_operator!(Value::eq)),
            "!=" => (Left(Comparison), as_binary_operator!(Value::ne)),
            "<" => (Left(Comparison), as_binary_operator!(Value::lt)),
            "<=" => (Left(Comparison), as_binary_operator!(Value::le)),
            ">" => (Left(Comparison), as_binary_operator!(Value::gt)),
            ">=" => (Left(Comparison), as_binary_operator!(Value::ge)),
            "max" => (Left(Comparison), Value::max),
            "min" => (Left(Comparison), Value::min),
            "+" => (Left(Additive), Value::add),
            "-" => (Left(Additive), Value::sub),
            "++" => (Left(Additive), as_binary_operator!(Value::hypot)),
            "+-+" => (Left(Additive), as_binary_operator!(Value::hypot_sub)),
            "*" => (Left(Multiplicative), Value::mul),
            "/" => (Left(Multiplicative), Value::div),
            "&" => (Left(Multiplicative), Value::intersect),
            "^" => (Right(Exponential), as_binary_operator!(Value::pow)),
            "<>" => (Left(Exponential), Value::line_between),
            ">>" => (Left(Exponential), Value::line_vector),
            "^^" => (Left(Exponential), Value::line_offset),
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
            "xpart" => (Multiplicative, as_unary_operator!(Value::xpart)),
            "ypart" => (Multiplicative, as_unary_operator!(Value::ypart)),
            "cos" => (Exponential, as_unary_operator!(Value::cos)),
            "sin" => (Exponential, as_unary_operator!(Value::sin)),
            "dir" => (Exponential, Value::dir),
            "angle" => (Exponential, as_unary_operator!(Value::angle)),
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
