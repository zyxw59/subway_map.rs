use std::{
    collections::{hash_map::Entry, HashMap},
    fmt, iter,
};

use expr_parser::parser::ParseState;

use crate::{
    error::{LexerError, ParserError, Result},
    expressions::{Expression, Function, Variable},
    lexer::{Token, TokenKind},
    statement::{Segment, Statement, StatementKind, Stop},
};

mod expression;

type TokenResult = Result<Token, LexerError>;

pub trait LexerExt: Iterator<Item = TokenResult> {
    fn into_parser(self) -> impl Iterator<Item = Result<Statement>>
    where
        Self: Sized,
    {
        use itertools::Itertools;
        self.batching(|tokens| parse_statement(tokens).transpose())
    }
}

impl<T: Iterator<Item = TokenResult>> LexerExt for T {}

pub type Span<T = Position> = expr_parser::Span<T>;

#[derive(Clone, Copy, Default, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub byte_idx: usize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

pub fn parse_statement(tokens: impl IntoIterator<Item = TokenResult>) -> Result<Option<Statement>> {
    let mut tokens = tokens.into_iter();
    let Some(initial_token) = tokens.next().transpose()? else {
        return Ok(None);
    };
    let mut semicolon = None;
    let tokens = tokens.take_while(|res| {
        res.as_ref().map_or(true, |tok| {
            if tok.kind == TokenKind::Semicolon {
                semicolon = Some(tok.span);
                false
            } else {
                true
            }
        })
    });
    let line = initial_token.span.start.line;
    let statement = parse_statement_kind(initial_token, tokens).map_err(|err| {
        // handle unexpected end of input caused by semicolons
        use crate::error::Error;
        if let (Error::Parser(ParserError::EndOfInput), Some(span)) = (&err, semicolon) {
            ParserError::Token(TokenKind::Semicolon, span).into()
        } else {
            err
        }
    })?;
    Ok(Some(Statement { line, statement }))
}

fn parse_statement_kind(
    initial_token: Token,
    mut tokens: impl Iterator<Item = TokenResult>,
) -> Result<StatementKind> {
    match initial_token.kind {
        // tag; start of an assignment expression or function definition
        TokenKind::Tag(tag) => {
            match tag.as_ref() {
                // function definition
                "fn" => {
                    let (func_name, func) = parse_function_def(tokens)?;
                    Ok(StatementKind::Function(func_name, func))
                }
                // single point
                "point" => {
                    let name = expect_get_tag(tokens.next())?;
                    expect_tag(tokens.next(), "=")?;
                    let expr = parse_expression(tokens, HashMap::new())?;
                    Ok(StatementKind::PointSingle(name, expr))
                }
                // sequence of points
                "points" => parse_points_statement(tokens),
                // route
                "route" => {
                    let (styles, next) = parse_dot_list(&mut tokens)?;
                    let name = expect_get_tag(next.map(Ok))?;
                    expect_tag(tokens.next(), ":")?;
                    let segments = parse_route(tokens)?;
                    Ok(StatementKind::Route {
                        name,
                        styles,
                        segments,
                    })
                }
                // stop
                "stop" => parse_stop_statement(tokens),
                // stylesheet
                "style" => {
                    let style = expect_get_string(tokens.next())?;
                    Ok(StatementKind::Style(style))
                }
                // title
                "title" => {
                    let title = expect_get_string(tokens.next())?;
                    Ok(StatementKind::Title(title))
                }
                // other (variable assignment)
                _ => {
                    let (fields, next) = parse_dot_list(&mut tokens)?;
                    expect_tag(next.map(Ok), "=")?;
                    let expr = parse_expression(tokens, HashMap::new())?;
                    Ok(StatementKind::Variable(tag, fields, expr))
                }
            }
        }
        // semicolon; null statement
        TokenKind::Semicolon => Ok(StatementKind::Null),
        // other token; unexpected
        _ => Err(ParserError::Token(initial_token.kind, initial_token.span).into()),
    }
}

pub fn parse_expression(
    tokens: impl Iterator<Item = TokenResult>,
    args: HashMap<Variable, usize>,
) -> Result<Expression> {
    let mut state = ParseState::new(expression::Parser::new(args));
    state.extend(tokens);
    state.finish().map_err(|e| panic!("{e}"))
}

fn parse_expression_until(
    tokens: impl IntoIterator<Item = TokenResult>,
    mut until: impl for<'a> FnMut(&'a TokenKind) -> bool,
) -> Result<Expression> {
    parse_expression(
        tokens
            .into_iter()
            .take_while(|res| res.as_ref().map_or(true, |tok| !until(&tok.kind))),
        HashMap::new(),
    )
}

fn parse_delimited_expression(tokens: impl IntoIterator<Item = TokenResult>) -> Result<Expression> {
    let mut state = ParseState::new(expression::Parser::new(HashMap::new()));
    for token in tokens {
        state.parse_result(token);
        if state.has_parsed_expression() {
            break;
        }
    }
    state.finish().map_err(|e| panic!("{e}"))
}

/// Parses a function definition.
///
/// On success, returns a tuple of the function name and the function definition.
fn parse_function_def(
    mut tokens: impl Iterator<Item = TokenResult>,
) -> Result<(Variable, Function)> {
    let name = expect_get_tag(tokens.next())?;
    let (_, start_span) = expect_if(tokens.next(), None, |tok| {
        matches!(tok, TokenKind::LeftParen)
    })?;
    // maps argument names to their index in the function signature
    let mut args = HashMap::new();
    loop {
        let (token, span) = expect_next_token(tokens.next(), Some(start_span), |token, span| {
            Ok((token, span))
        })?;
        let index = args.len();
        match token {
            // a named argument
            TokenKind::Tag(arg) => {
                // insert the new argument into the hashmap
                match args.entry(arg) {
                    // there's already an argument with this name
                    Entry::Occupied(e) => {
                        return Err(ParserError::Argument {
                            argument: e.remove_entry().0,
                            function: name,
                            span,
                        }
                        .into());
                    }
                    Entry::Vacant(e) => e.insert(index),
                };
                if expect_next_token(tokens.next(), Some(start_span), |tok, _| match tok {
                    TokenKind::Comma => Ok(false),
                    TokenKind::RightParen => Ok(true),
                    _ => Err(tok),
                })? {
                    break;
                }
            }
            // end of the arguments
            TokenKind::RightParen => break,
            _ => return Err(ParserError::Token(token, span).into()),
        }
    }
    expect_tag(tokens.next(), "=")?;
    // get the function body, as an expression tree
    let num_args = args.len();
    let expression = parse_expression(tokens, args)?.into();
    Ok((
        name,
        Function {
            expression,
            num_args,
        },
    ))
}

fn parse_point_list(
    mut tokens: impl Iterator<Item = TokenResult>,
) -> Result<Vec<(Option<Expression>, Variable)>> {
    let mut points = Vec::new();
    while let Some(token) = tokens.next().transpose()? {
        let point = match token.kind {
            TokenKind::LeftParen => {
                let multiplier =
                    parse_delimited_expression(iter::once(Ok(token)).chain(&mut tokens))?;
                let ident = expect_get_tag(tokens.next())?;
                (Some(multiplier), ident)
            }
            TokenKind::Tag(ident) => (None, ident),
            _ => return Err(ParserError::Token(token.kind, token.span).into()),
        };
        points.push(point);
        let Some(next) = tokens.next().transpose()? else {
            break;
        };
        if next.kind != TokenKind::Comma {
            return Err(ParserError::Token(next.kind, next.span).into());
        }
    }
    Ok(points)
}

fn parse_route(mut tokens: impl Iterator<Item = TokenResult>) -> Result<Vec<Segment>> {
    let mut route = Vec::new();
    let mut start = expect_get_tag(tokens.next())?;
    while let Some(token) = tokens.next().transpose()? {
        expect_if(Some(Ok(token)), None, |tok| tok.as_tag() == Some("--"))?;
        let token = tokens.next().ok_or(ParserError::EndOfInput)??;
        let (offset, end) = match token.kind {
            TokenKind::LeftParen => {
                let offset = parse_delimited_expression(iter::once(Ok(token)).chain(&mut tokens))?;
                let end = expect_get_tag(tokens.next())?;
                (offset, end)
            }
            TokenKind::Tag(tag) => {
                // TODO: zero expression
                (vec![], tag)
            }
            _ => return Err(ParserError::Token(token.kind, token.span).into()),
        };
        let next = end.clone();
        route.push(Segment { start, end, offset });
        start = next;
    }
    Ok(route)
}

fn parse_dot_list(
    mut tokens: impl Iterator<Item = TokenResult>,
) -> Result<(Vec<Variable>, Option<Token>)> {
    let mut list = Vec::new();
    let mut next = None;
    while let Some(token) = tokens.next().transpose()? {
        if let TokenKind::DotTag(tag) = token.kind {
            list.push(tag);
        } else {
            next = Some(token);
            break;
        }
    }
    Ok((list, next))
}

fn parse_points_statement(mut tokens: impl Iterator<Item = TokenResult>) -> Result<StatementKind> {
    expect_tag(tokens.next(), "from")?;
    let from = expect_get_tag(tokens.next())?;
    enum PointsKind {
        Spaced,
        To,
        Past,
    }
    let kind = expect_next_token(tokens.next(), None, |tok, _| match tok.as_tag() {
        Some("spaced") => Ok(PointsKind::Spaced),
        Some("to") => Ok(PointsKind::To),
        Some("past") => Ok(PointsKind::Past),
        _ => Err(tok),
    })?;
    match kind {
        // from ... spaced
        PointsKind::Spaced => {
            let spaced = parse_expression_until(&mut tokens, |tok| tok.as_tag() == Some(":"))?;
            let points = parse_point_list(tokens)?;
            Ok(StatementKind::PointSpaced {
                from,
                spaced,
                points,
            })
        }
        // from ... to
        PointsKind::To => parse_points_extend_statement(tokens, from, false),
        // from ... past
        PointsKind::Past => parse_points_extend_statement(tokens, from, true),
    }
}

fn parse_points_extend_statement(
    mut tokens: impl Iterator<Item = TokenResult>,
    from: Variable,
    is_past: bool,
) -> Result<StatementKind> {
    let token = tokens.next().ok_or(ParserError::EndOfInput)??;
    let to = match token.kind {
        TokenKind::LeftParen => {
            let multiplier = parse_delimited_expression(iter::once(Ok(token)).chain(&mut tokens))?;
            let ident = expect_get_tag(tokens.next())?;
            (Some(multiplier), ident)
        }
        TokenKind::Tag(ident) => (None, ident),
        _ => return Err(ParserError::Token(token.kind, token.span).into()),
    };
    expect_tag(tokens.next(), ":")?;
    let points = parse_point_list(tokens)?;
    Ok(StatementKind::PointExtend {
        from,
        to,
        points,
        is_past,
    })
}

fn parse_stop_statement(mut tokens: impl Iterator<Item = TokenResult>) -> Result<StatementKind> {
    let (styles, token) = parse_dot_list(&mut tokens)?;
    let token = token.ok_or(ParserError::EndOfInput)?;
    let point = parse_expression_until(iter::once(Ok(token)).chain(&mut tokens), |tok| {
        tok.as_tag() == Some("marker")
    })?;
    let marker_type = expect_get_tag(tokens.next())?;
    let marker_parameters = parse_marker_params(tokens)?;
    Ok(StatementKind::Stop(Stop {
        point,
        styles,
        marker_type,
        marker_parameters,
    }))
}

fn parse_marker_params(
    mut tokens: impl Iterator<Item = TokenResult>,
) -> Result<HashMap<Variable, Expression>> {
    let mut params = HashMap::new();
    while let Some(token) = tokens.next().transpose()? {
        match token.kind {
            TokenKind::Comma => {}
            TokenKind::Tag(tag) => {
                let paren = tokens.next().ok_or(ParserError::EndOfInput)??;
                let expr = parse_delimited_expression(iter::once(Ok(paren)).chain(&mut tokens))?;
                match params.entry(tag) {
                    // there's already an argument with this name
                    Entry::Occupied(e) => {
                        return Err(ParserError::Argument {
                            argument: e.remove_entry().0,
                            function: "marker".into(),
                            span: token.span,
                        }
                        .into());
                    }
                    Entry::Vacant(e) => e.insert(expr),
                };
            }
            _ => return Err(ParserError::Token(token.kind, token.span).into()),
        }
    }
    Ok(params)
}

fn expect_next_token<U>(
    token: Option<TokenResult>,
    paren_span: Option<Span>,
    pred: impl FnOnce(TokenKind, Span) -> Result<U, TokenKind>,
) -> Result<U> {
    let Token { kind, span } = token.ok_or({
        if let Some(span) = paren_span {
            ParserError::Parentheses(span)
        } else {
            ParserError::EndOfInput
        }
    })??;
    pred(kind, span).map_err(|kind| ParserError::Token(kind, span).into())
}

fn expect_if(
    token: Option<TokenResult>,
    paren_span: Option<Span>,
    pred: impl FnOnce(&TokenKind) -> bool,
) -> Result<(TokenKind, Span)> {
    expect_next_token(token, paren_span, |tok, span| {
        if pred(&tok) {
            Ok((tok, span))
        } else {
            Err(tok)
        }
    })
}

fn expect_get_tag(token: Option<TokenResult>) -> Result<Variable> {
    expect_next_token(token, None, |tok, _| match tok {
        TokenKind::Tag(t) => Ok(t),
        _ => Err(tok),
    })
}

fn expect_get_string(token: Option<TokenResult>) -> Result<String> {
    expect_next_token(token, None, |tok, _| match tok {
        TokenKind::String(s) => Ok(s),
        _ => Err(tok),
    })
}

fn expect_tag(token: Option<TokenResult>, tag: &str) -> Result<()> {
    expect_if(token, None, |tok| tok.as_tag() == Some(tag))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        expressions::tests::{b, dot, expression_full, u, var},
        lexer::Lexer,
        operators::{COMMA, COMMA_UNARY, FN_CALL, FN_CALL_UNARY, PAREN_UNARY},
        statement::{StatementKind, Stop},
    };

    use super::{parse_expression, parse_statement};

    macro_rules! assert_expression {
        ($text:expr, [$($expr:tt)*]) => {{
            let result = parse_expression(Lexer::new($text), HashMap::new())
                .unwrap()
                .into_iter()
                .map(|expr| expr.kind)
                .collect::<Vec<_>>();
            assert_eq!(result, crate::expressions::tests::expression!($($expr)*));
        }};
    }

    #[test]
    fn basic_arithmetic() {
        assert_expression!("1+2*3+4", [1, 2, 3, b("*"), b("+"), 4, b("+")]);
    }

    #[test]
    fn basic_arithmetic_2() {
        assert_expression!("1-2*3+4", [1, 2, 3, b("*"), b("-"), 4, b("+")]);
    }

    #[test]
    fn basic_arithmetic_3() {
        assert_expression!("1-3/2*5", [1, 3, 2, b("/"), 5, b("*"), b("-")]);
    }

    #[test]
    fn hypot() {
        assert_expression!("3++4", [3, 4, b("++")]);
    }

    #[test]
    fn hypot_sub() {
        assert_expression!("5+-+3", [5, 3, b("+-+")]);
    }

    #[test]
    fn pow() {
        assert_expression!("3^4^5", [3, 4, 5, b("^"), b("^")]);
    }

    #[test]
    fn parentheses() {
        assert_expression!(
            "(1+2)*3+4",
            [1, 2, b("+"), PAREN_UNARY, 3, b("*"), 4, b("+")]
        );
    }

    #[test]
    fn points() {
        assert_expression!(
            "(1,2) + (3,4)",
            [1, 2, COMMA, PAREN_UNARY, 3, 4, COMMA, PAREN_UNARY, b("+")]
        );
    }

    #[test]
    fn trailing_comma() {
        assert_expression!("(1, 2,)", [1, 2, COMMA, COMMA_UNARY, PAREN_UNARY]);
    }

    #[test]
    fn dot_product() {
        assert_expression!(
            "(1,2) * (3,4)",
            [1, 2, COMMA, PAREN_UNARY, 3, 4, COMMA, PAREN_UNARY, b("*")]
        );
    }

    #[test]
    fn scalar_product() {
        assert_expression!("3 * (1,2)", [3, 1, 2, COMMA, PAREN_UNARY, b("*")]);
    }

    #[test]
    fn angle() {
        assert_expression!("angle (3, 3)", [3, 3, COMMA, PAREN_UNARY, u("angle")]);
    }

    #[test]
    fn unary_minus() {
        assert_expression!("3* -2", [3, 2, u("-"), b("*")]);
    }

    #[test]
    fn unary_minus_2() {
        assert_expression!(
            "-(1,2)*(3,4)",
            [
                1,
                2,
                COMMA,
                PAREN_UNARY,
                u("-"),
                3,
                4,
                COMMA,
                PAREN_UNARY,
                b("*"),
            ]
        );
    }

    #[test]
    fn variable() {
        assert_expression!("3*x", [3, var("x"), b("*")]);
    }

    #[test]
    fn dotted_variable() {
        assert_expression!("3*x.y", [3, var("x"), dot("y"), b("*")]);
    }

    #[test]
    fn string() {
        assert_expression!(r#""foobar""#, ["foobar"]);
    }

    #[test]
    fn function_call() {
        assert_expression!(
            "a() + b(1) + c(2, 3)",
            [
                var("a"),
                FN_CALL_UNARY,
                var("b"),
                1,
                FN_CALL,
                b("+"),
                var("c"),
                2,
                3,
                COMMA,
                FN_CALL,
                b("+"),
            ]
        )
    }

    macro_rules! assert_statement {
        ($text:expr, $statement:expr) => {{
            let result = parse_statement(Lexer::new($text)).unwrap().unwrap();
            assert_eq!(result.statement, $statement);
        }};
    }

    #[test]
    fn variable_assignment() {
        assert_statement!(
            "a = b",
            StatementKind::Variable("a".into(), Vec::new(), expression_full![var("b")].into())
        );
    }

    #[test]
    fn dotted_variable_assignment() {
        assert_statement!(
            "a.b = c",
            StatementKind::Variable(
                "a".into(),
                vec!["b".into()],
                expression_full![var("c")].into()
            )
        );
    }

    #[test]
    fn point_single() {
        assert_statement!(
            "point a = b",
            StatementKind::PointSingle("a".into(), expression_full![var("b")].into())
        );
    }

    #[test]
    fn points_spaced() {
        assert_statement!(
            "points from a spaced x: (1/2) b, c, (1/2) d",
            StatementKind::PointSpaced {
                from: "a".into(),
                spaced: expression_full![var("x")].into(),
                points: vec![
                    (
                        Some(expression_full![1, 2, b("/"), PAREN_UNARY].into()),
                        "b".into()
                    ),
                    (None, "c".into()),
                    (
                        Some(expression_full![1, 2, b("/"), PAREN_UNARY].into()),
                        "d".into()
                    ),
                ],
            }
        );
    }

    #[test]
    fn points_between() {
        assert_statement!(
            "points from a to (1/2) d: (1/2) b, c",
            StatementKind::PointExtend {
                from: "a".into(),
                to: (
                    Some(expression_full![1, 2, b("/"), PAREN_UNARY].into()),
                    "d".into()
                ),
                points: vec![
                    (
                        Some(expression_full![1, 2, b("/"), PAREN_UNARY].into()),
                        "b".into()
                    ),
                    (None, "c".into()),
                ],
                is_past: false,
            }
        );
    }

    #[test]
    fn route() {
        assert_statement!(
            "route red: a --(1) b --(1) c",
            StatementKind::Route {
                styles: vec![],
                name: "red".into(),
                segments: vec![
                    segment!("a", "b", [1, PAREN_UNARY]),
                    segment!("b", "c", [1, PAREN_UNARY]),
                ],
            }
        )
    }

    #[test]
    fn route_with_style() {
        assert_statement!(
            "route.narrow red: a --(1) b --(1) c",
            StatementKind::Route {
                styles: vec!["narrow".into()],
                name: "red".into(),
                segments: vec![
                    segment!("a", "b", [1, PAREN_UNARY]),
                    segment!("b", "c", [1, PAREN_UNARY]),
                ],
            }
        )
    }

    #[test]
    fn stop() {
        assert_statement!(
            r#"stop a marker circle r(10)"#,
            StatementKind::Stop(Stop {
                styles: vec![],
                point: expression_full![var("a")].into(),
                marker_type: "circle".into(),
                marker_parameters: [("r".into(), expression_full![10, PAREN_UNARY].into())]
                    .into_iter()
                    .collect(),
            })
        )
    }

    #[test]
    fn stop_with_style() {
        assert_statement!(
            r#"stop.terminus a marker double_tick length(20), angle(45)"#,
            StatementKind::Stop(Stop {
                styles: vec!["terminus".into()],
                point: expression_full![var("a")].into(),
                marker_type: "double_tick".into(),
                marker_parameters: [
                    ("length".into(), expression_full![20, PAREN_UNARY].into()),
                    ("angle".into(), expression_full![45, PAREN_UNARY].into())
                ]
                .into_iter()
                .collect(),
            })
        )
    }

    #[test]
    fn stop_with_text() {
        assert_statement!(
            r#"stop.terminus a marker text text("A station") angle(0)"#,
            StatementKind::Stop(Stop {
                styles: vec!["terminus".into()],
                point: expression_full![var("a")].into(),
                marker_type: "text".into(),
                marker_parameters: [
                    (
                        "text".into(),
                        expression_full!["A station", PAREN_UNARY].into()
                    ),
                    ("angle".into(), expression_full![0, PAREN_UNARY].into())
                ]
                .into_iter()
                .collect(),
            })
        )
    }
}
