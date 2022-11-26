#[cfg(test)]
macro_rules! token {
    (.) => {
        $crate::lexer::Token::Dot
    };
    ((l)) => {
        $crate::lexer::Token::LeftParen
    };
    ((r)) => {
        $crate::lexer::Token::RightParen
    };
    (,) => {
        $crate::lexer::Token::Comma
    };
    (;) => {
        $crate::lexer::Token::Semicolon
    };
    (=) => {
        $crate::lexer::Token::Equal
    };
    (#$tag:expr) => {
        $crate::lexer::Token::Tag(String::from($tag))
    };
    ($value:expr) => {
        $crate::lexer::Token::Number($value as f64)
    };
    (@$str:expr) => {
        $crate::lexer::Token::String(String::from($str))
    };
}

#[cfg(test)]
/// Macro for building Expressions
macro_rules! expression {
    ($op:tt, ($($x:tt)+), ($($y:tt)+)) => {{
        $crate::expressions::Expression::BinaryOperator(
            $crate::operators::BinaryBuiltins.get($op).unwrap(),
            Box::new((expression!($($x)+), expression!($($y)+))),
        )
    }};
    ($op:tt, ($($x:tt)+), $y:expr) => {
        expression!($op, ($($x)+), ($y))
    };
    ($op:tt, ($($x:tt)+)) => {{
        $crate::expressions::Expression::UnaryOperator(
            $crate::operators::UnaryBuiltins.get($op).unwrap(),
            Box::new(expression!($($x)+)),
        )
    }};
    ($op:tt, $x:expr, ($($y:tt)+)) => {
        expression!($op, ($x), ($($y)+))
    };
    ($op:tt, $x:expr, $y:expr) => {
        expression!($op, ($x), ($y))
    };
    ($op:tt, $x:expr) => {
        expression!($op, ($x))
    };
    ($fn:tt[$(($($x:tt)+)),*]) => {
        $crate::expressions::Expression::Function(
            $crate::expressions::Variable::from($fn),
            vec![$(expression!($($x)+)),*],
        )
    };
    (@($($x:tt)+), ($($y:tt)+)) => {
        $crate::expressions::Expression::Point(
            Box::new((expression!($($x)+), expression!($($y)+))),
        )
    };
    (@$x:expr, ($($y:tt)+)) => {
        expression!(@($x), ($($y)+))
    };
    (@($($x:tt)+), $y:expr) => {
        expression!(@($($x)+), ($y))
    };
    (@$x:expr, $y:expr) => {
        expression!(@($x), ($y))
    };
    (#$var:expr) => {
        $crate::expressions::Expression::Variable($crate::expressions::Variable::from($var))
    };
    ($x:expr) => {
        $crate::expressions::Expression::Value($crate::values::Value::Number($x as f64))
    };
    (@$s:expr) => {
        $crate::expressions::Expression::Value($crate::values::Value::String($s.into()))
    };
}

#[cfg(test)]
macro_rules! value {
    ($x:expr) => {
        $crate::values::Value::Number($x as f64)
    };
    ($x:expr, $y:expr) => {
        $crate::values::Value::Point(
            $crate::values::Point($x as f64, $y as f64),
            $crate::values::PointProvenance::None,
        )
    };
    ($x:expr, $y:expr, $id:expr) => {
        $crate::values::Value::Point(
            $crate::values::Point($x as f64, $y as f64),
            $crate::values::PointProvenance::Named($id),
        )
    };
    (@$s:expr) => {
        $crate::values::Value::String($s.into())
    };
}

#[cfg(test)]
macro_rules! segment {
    ($start:expr, $end:expr, $($expr:tt)*) => {
        $crate::statement::Segment {
            start: $crate::expressions::Variable::from($start),
            end: $crate::expressions::Variable::from($end),
            offset: expression!($($expr)*),
        }
    }
}
