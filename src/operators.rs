use std::cmp::PartialEq;
use std::fmt;

use crate::error::MathError;
use crate::expressions::Expression;
use crate::values::Value;

type EResult<T> = Result<T, MathError>;

enum Precedence {
    /// Comparison operators, such as `==`, `>`, `<>`, etc.
    Comparison,
    /// Additive operators, such as `+`, `-`, `++`, etc.
    Additive,
    /// Multiplicative operators, such as `*`, `/`, `&`, etc., as well as unary minus.
    Multiplicative,
    /// Exponential operators, such as `^`, as well as unary sine and cosine.
    Exponential,
}

mod builtins {
    use crate::values::Value;

    use super::{BinaryOperator, Precedence, UnaryOperator};

    macro_rules! builtins {
        ($ty:ident: $($id:ident ( $prec:ident, $fun:path, $name:literal )),* $(,)?) => {
            $(
                pub const $id: $ty = $ty {
                    precedence: Precedence::$prec as usize,
                    function: $fun,
                    name: $name,
                };
            )*
            impl $ty {
                pub fn builtin(key: &str) -> Option<&'static Self> {
                    match key {
                        $(
                            $name => Some(&$id),
                        )*
                        _ => None,
                    }
                }
            }
        };
    }

    builtins! {BinaryOperator:
        EQ(Comparison, Value::eq, "=="),
        NE(Comparison, Value::ne, "!="),
        LT(Comparison, Value::lt, ">"),
        LE(Comparison, Value::le, ">="),
        GT(Comparison, Value::gt, "<"),
        GE(Comparison, Value::ge, "<="),
        MAX(Comparison, Value::max, "max"),
        MIN(Comparison, Value::min, "min"),
        ADD(Additive, Value::add, "+"),
        SUB(Additive, Value::sub, "-"),
        HYPOT(Additive, Value::hypot, "++"),
        HYPOT_SUB(Additive, Value::hypot_sub, "+-+"),
        MUL(Multiplicative, Value::mul, "*"),
        DIV(Multiplicative, Value::div, "/"),
        POW(Exponential, Value::pow, "^"),
        LINE_BETWEEN(Exponential, Value::line_between, "<>"),
        LINE_VECTOR(Exponential, Value::line_vector, ">>"),
        INTERSECT(Multiplicative, Value::intersect, "&"),
        LINE_OFFSET(Exponential, Value::line_offset, "^^"),
    }

    builtins! {UnaryOperator:
        NEG(Multiplicative, Value::neg, "-"),
        COS(Exponential, Value::cos, "cos"),
        SIN(Exponential, Value::sin, "sin"),
        DIR(Exponential, Value::dir, "dir"),
        ANGLE(Exponential, Value::angle, "angle"),
        XPART(Multiplicative, Value::xpart, "xpart"),
        YPART(Multiplicative, Value::ypart, "ypart"),
    }
}

#[derive(Clone)]
pub struct BinaryOperator {
    pub precedence: usize,
    function: fn(Value, Value) -> EResult<Value>,
    name: &'static str,
}

impl BinaryOperator {
    pub fn apply(&self, lhs: Value, rhs: Value) -> EResult<Value> {
        (self.function)(lhs, rhs)
    }

    pub fn expression(&'static self, lhs: Expression, rhs: Expression) -> Expression {
        Expression::BinaryOperator(self, Box::new((lhs, rhs)))
    }
}

impl fmt::Debug for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl PartialEq for BinaryOperator {
    fn eq(&self, other: &Self) -> bool {
        self.precedence == other.precedence && self.name == other.name
    }
}

#[derive(Clone)]
pub struct UnaryOperator {
    pub precedence: usize,
    function: fn(Value) -> EResult<Value>,
    name: &'static str,
}

impl UnaryOperator {
    pub fn apply(&self, argument: Value) -> EResult<Value> {
        (self.function)(argument)
    }

    pub fn expression(&'static self, argument: Expression) -> Expression {
        Expression::UnaryOperator(self, Box::new(argument))
    }
}

impl fmt::Debug for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl PartialEq for UnaryOperator {
    fn eq(&self, other: &Self) -> bool {
        self.precedence == other.precedence && self.name == other.name
    }
}
