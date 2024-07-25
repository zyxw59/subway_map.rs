#[cfg(test)]
macro_rules! token {
    (.$tag:expr) => {
        $crate::lexer::TokenKind::DotTag(smol_str::SmolStr::from($tag))
    };
    ((l)) => {
        $crate::lexer::TokenKind::LeftParen
    };
    ((r)) => {
        $crate::lexer::TokenKind::RightParen
    };
    (,) => {
        $crate::lexer::TokenKind::Comma
    };
    (;) => {
        $crate::lexer::TokenKind::Semicolon
    };
    (#$tag:expr) => {
        $crate::lexer::TokenKind::Tag(smol_str::SmolStr::from($tag))
    };
    ($value:expr) => {
        $crate::lexer::TokenKind::Number($value as f64)
    };
    (@$str:expr) => {
        $crate::lexer::TokenKind::String(String::from($str))
    };
}

#[cfg(test)]
macro_rules! value {
    (($x:expr, $y:expr)) => {
        value!($x, $y)
    };
    (($x:expr, $y:expr, $id:expr)) => {
        value!($x, $y, $id)
    };
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
    (($x1:expr, $y1:expr) -> ($x2:expr, $y2:expr)) => {
        $crate::values::Value::Line(
            $crate::values::Point($x1 as f64, $y1 as f64),
            $crate::values::Point(($x2 - $x1) as f64, ($y2 - $y1) as f64),
            None,
        )
    };
    (@$s:expr) => {
        $crate::values::Value::String($s.into())
    };
}

#[cfg(test)]
macro_rules! segment {
    ($start:expr, $end:expr, [$($expr:expr),*$(,)?]) => {
        $crate::statement::Segment {
            start: $crate::expressions::Variable::from($start),
            end: $crate::expressions::Variable::from($end),
            offset: $crate::expressions::tests::expression_full![$($expr),*].into(),
        }
    }
}
