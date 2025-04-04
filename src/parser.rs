use std::{
    collections::{hash_map::Entry, HashMap},
    fmt, iter,
};

use expr_parser::parser::ParseState;

use crate::{
    expressions::{Expression, Function, Variable},
    statement::{Segment, Statement, StatementKind, Stop},
};

mod error;
mod expression;
mod lexer;

pub use error::{Error, Errors, LexerError};
use error::{Result, ResultExt};
use lexer::{Lexer, Token, TokenKind};

type TokenResult = Result<Token, LexerError>;

pub fn parse(input: &str) -> Result<Vec<Statement>, Errors<'_>> {
    use itertools::Itertools;
    let mut errors = Vec::new();
    let statements = Lexer::new(input)
        .batching(|tokens| parse_statement(tokens, &mut errors))
        .collect();
    if errors.is_empty() {
        Ok(statements)
    } else {
        Err(Errors::new(input, errors))
    }
}

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

pub fn parse_statement(
    tokens: impl IntoIterator<Item = TokenResult>,
    errors: &mut Vec<Error>,
) -> Option<Statement> {
    let mut tokens = tokens.into_iter();
    loop {
        let initial_token = tokens.by_ref().find_map(|res| match res {
            Ok(token) => Some(token),
            Err(err) => {
                errors.push(err.into());
                None
            }
        })?;
        let mut semicolon = None;
        let tokens = tokens.by_ref().take_while(|res| {
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
        let errors_start = errors.len();
        let maybe_statement = parse_statement_kind(initial_token, tokens, errors);
        if errors.len() > errors_start {
            for err in &mut errors[errors_start..] {
                // handle unexpected end of input caused by semicolons
                if let (Error::EndOfInput, Some(span)) = (&err, semicolon) {
                    *err = Error::Token(TokenKind::Semicolon, span)
                }
            }
            continue;
        }
        if let Some(statement) = maybe_statement {
            return Some(Statement { line, statement });
        }
    }
}

fn parse_statement_kind(
    initial_token: Token,
    mut tokens: impl Iterator<Item = TokenResult>,
    errors: &mut Vec<Error>,
) -> Option<StatementKind> {
    match initial_token.kind {
        // tag; start of an assignment expression or function definition
        TokenKind::Tag(tag) => {
            match tag.as_ref() {
                // function definition
                "fn" => {
                    let (func_name, func) = parse_function_def(tokens, errors)?;
                    Some(StatementKind::Function(func_name, func))
                }
                // single point
                "point" => {
                    let name = expect_get_tag(tokens.next()).or_push(errors);
                    expect_tag(tokens.next(), "=").or_push(errors);
                    let expr = parse_expression(tokens, HashMap::new(), errors);
                    Some(StatementKind::PointSingle(name?, expr?))
                }
                // sequence of points
                "points" => parse_points_statement(tokens, errors),
                // route
                "route" => {
                    let (styles, next) = parse_dot_list(&mut tokens);
                    let name = expect_get_tag(next).or_push(errors);
                    expect_tag(tokens.next(), ":").or_push(errors);
                    let segments = parse_route(tokens, errors);
                    Some(StatementKind::Route {
                        name: name?,
                        styles,
                        segments: segments?,
                    })
                }
                // stop
                "stop" => parse_stop_statement(tokens, errors),
                // stylesheet
                "style" => {
                    let style = expect_get_string(tokens.next()).or_push(errors)?;
                    Some(StatementKind::Style(style))
                }
                // title
                "title" => {
                    let title = expect_get_string(tokens.next()).or_push(errors)?;
                    Some(StatementKind::Title(title))
                }
                // other (variable assignment)
                _ => {
                    let (fields, next) = parse_dot_list(&mut tokens);
                    expect_tag(next, "=").or_push(errors);
                    let expr = parse_expression(tokens, HashMap::new(), errors);
                    Some(StatementKind::Variable(tag, fields, expr?))
                }
            }
        }
        // semicolon; no statement
        TokenKind::Semicolon => None,
        // other token; unexpected
        _ => {
            errors.push(Error::Token(initial_token.kind, initial_token.span));
            None
        }
    }
}

pub fn parse_expression(
    tokens: impl Iterator<Item = TokenResult>,
    args: HashMap<Variable, usize>,
    errors: &mut Vec<Error>,
) -> Option<Expression> {
    let mut state = ParseState::new(expression::Parser::new(args));
    state.extend(tokens);
    state
        .finish()
        .map_err(|es| errors.extend(es.errors.into_iter().map(|e| panic!("{e}"))))
        .ok()
}

fn parse_expression_until(
    tokens: impl IntoIterator<Item = TokenResult>,
    errors: &mut Vec<Error>,
    mut until: impl for<'a> FnMut(&'a TokenKind) -> bool,
) -> Option<Expression> {
    parse_expression(
        tokens
            .into_iter()
            .take_while(|res| res.as_ref().map_or(true, |tok| !until(&tok.kind))),
        HashMap::new(),
        errors,
    )
}

fn parse_delimited_expression(
    tokens: impl IntoIterator<Item = TokenResult>,
    errors: &mut Vec<Error>,
) -> Option<Expression> {
    let mut state = ParseState::new(expression::Parser::new(HashMap::new()));
    for token in tokens {
        state.parse_result(token);
        if state.has_parsed_expression() {
            break;
        }
    }
    state
        .finish()
        .map_err(|es| errors.extend(es.errors.into_iter().map(|e| panic!("{e}"))))
        .ok()
}

/// Parses a function definition.
///
/// On success, returns a tuple of the function name and the function definition.
fn parse_function_def(
    mut tokens: impl Iterator<Item = TokenResult>,
    errors: &mut Vec<Error>,
) -> Option<(Variable, Function)> {
    let name = expect_get_tag(tokens.next()).or_push(errors);
    let start_span = expect_map(tokens.next(), None, |token| match token.kind {
        TokenKind::LeftParen => Some(token.span),
        _ => None,
    })
    .or_push(errors);
    // maps argument names to their index in the function signature
    let mut args = HashMap::new();
    loop {
        let token = expect(tokens.next(), start_span).or_push(errors)?;
        let index = args.len();
        match token.kind {
            // a named argument
            TokenKind::Tag(arg) => {
                // insert the new argument into the hashmap
                match args.entry(arg) {
                    // there's already an argument with this name
                    Entry::Occupied(e) => {
                        errors.push(Error::Argument {
                            argument: e.key().clone(),
                            span: token.span,
                        });
                    }
                    Entry::Vacant(e) => {
                        e.insert(index);
                    }
                };
                // TODO: not sure the best way to handle bad tokens here. ideally we try and
                // parse the whole (invalid) statement, but it's hard to tell what the proper
                // recovery strategy here is.
                let should_break =
                    expect_map(tokens.next(), start_span, |token| match token.kind {
                        TokenKind::Comma => Some(false),
                        TokenKind::RightParen => Some(true),
                        _ => None,
                    })
                    .or_push(errors)
                    .unwrap_or(false);
                if should_break {
                    break;
                }
            }
            // end of the arguments
            TokenKind::RightParen => break,
            _ => {
                errors.push(Error::Token(token.kind, token.span));
                return None;
            }
        }
    }
    expect_tag(tokens.next(), "=").or_push(errors);
    // get the function body, as an expression tree
    let num_args = args.len();
    let expression = parse_expression(tokens, args, errors);
    Some((
        name?,
        Function {
            expression: expression?.into(),
            num_args,
        },
    ))
}

fn parse_point_list(
    mut tokens: impl Iterator<Item = TokenResult>,
    errors: &mut Vec<Error>,
) -> Vec<(Option<Expression>, Variable)> {
    let mut points = Vec::new();
    while let Some(token) = tokens.next().transpose().or_push(errors).flatten() {
        let point = match token.kind {
            TokenKind::LeftParen => {
                let multiplier =
                    parse_delimited_expression(iter::once(Ok(token)).chain(&mut tokens), errors)
                        .map(Some);
                let ident = expect_get_tag(tokens.next()).or_push(errors);
                multiplier.zip(ident)
            }
            TokenKind::Tag(ident) => Some((None, ident)),
            _ => {
                errors.push(Error::Token(token.kind, token.span));
                None
            }
        };
        if let Some(point) = point {
            points.push(point);
        }
        let Some(next) = tokens.next().transpose().or_push(errors).flatten() else {
            break;
        };
        if next.kind != TokenKind::Comma {
            errors.push(Error::Token(next.kind, next.span));
        }
    }
    points
}

fn parse_route(
    mut tokens: impl Iterator<Item = TokenResult>,
    errors: &mut Vec<Error>,
) -> Option<Vec<Segment>> {
    let mut route = Vec::new();
    let mut start = expect_get_tag(tokens.next()).or_push(errors);
    while let Some(token) = tokens.next().or_push(errors) {
        expect_tag(Some(Ok(token)), "--").or_push(errors);
        let token = expect(tokens.next(), None).or_push(errors)?;
        let offset_and_end = match token.kind {
            TokenKind::LeftParen => {
                let offset =
                    parse_delimited_expression(iter::once(Ok(token)).chain(&mut tokens), errors);
                let end = expect_get_tag(tokens.next()).or_push(errors);
                offset.zip(end)
            }
            TokenKind::Tag(tag) => {
                // TODO: zero expression
                Some((vec![], tag))
            }
            _ => {
                errors.push(Error::Token(token.kind, token.span));
                None
            }
        };
        let next = offset_and_end.as_ref().map(|(_, end)| end.clone());
        if let Some((start, (offset, end))) = start.zip(offset_and_end) {
            route.push(Segment { start, end, offset });
        }
        start = next;
    }
    Some(route)
}

fn parse_dot_list(
    mut tokens: impl Iterator<Item = TokenResult>,
) -> (Vec<Variable>, Option<TokenResult>) {
    let mut list = Vec::new();
    loop {
        let token = match tokens.next() {
            Some(Ok(token)) => token,
            Some(Err(e)) => return (list, Some(Err(e))),
            None => return (list, None),
        };
        if let TokenKind::DotTag(tag) = token.kind {
            list.push(tag);
        } else {
            return (list, Some(Ok(token)));
        }
    }
}

fn parse_points_statement(
    mut tokens: impl Iterator<Item = TokenResult>,
    errors: &mut Vec<Error>,
) -> Option<StatementKind> {
    expect_tag(tokens.next(), "from").or_push(errors);
    let from = expect_get_tag(tokens.next()).or_push(errors);
    enum PointsKind {
        Spaced,
        To,
        Past,
    }
    let kind = expect_map(tokens.next(), None, |token| match token.kind.as_tag() {
        Some("spaced") => Some(PointsKind::Spaced),
        Some("to") => Some(PointsKind::To),
        Some("past") => Some(PointsKind::Past),
        _ => None,
    })
    .or_push(errors)?;
    match kind {
        // from ... spaced
        PointsKind::Spaced => {
            let spaced =
                parse_expression_until(&mut tokens, errors, |tok| tok.as_tag() == Some(":"));
            let points = parse_point_list(tokens, errors);
            Some(StatementKind::PointSpaced {
                from: from?,
                spaced: spaced?,
                points,
            })
        }
        // from ... to
        PointsKind::To => parse_points_extend_statement(tokens, from, false, errors),
        // from ... past
        PointsKind::Past => parse_points_extend_statement(tokens, from, true, errors),
    }
}

fn parse_points_extend_statement(
    mut tokens: impl Iterator<Item = TokenResult>,
    from: Option<Variable>,
    is_past: bool,
    errors: &mut Vec<Error>,
) -> Option<StatementKind> {
    let token = expect(tokens.next(), None).or_push(errors)?;
    let to = match token.kind {
        TokenKind::LeftParen => {
            let multiplier =
                parse_delimited_expression(iter::once(Ok(token)).chain(&mut tokens), errors)
                    .map(Some);
            let ident = expect_get_tag(tokens.next()).or_push(errors);
            multiplier.zip(ident)
        }
        TokenKind::Tag(ident) => Some((None, ident)),
        _ => {
            errors.push(Error::Token(token.kind, token.span));
            None
        }
    };
    expect_tag(tokens.next(), ":").or_push(errors);
    let points = parse_point_list(tokens, errors);
    Some(StatementKind::PointExtend {
        from: from?,
        to: to?,
        points,
        is_past,
    })
}

fn parse_stop_statement(
    mut tokens: impl Iterator<Item = TokenResult>,
    errors: &mut Vec<Error>,
) -> Option<StatementKind> {
    let (styles, token) = parse_dot_list(&mut tokens);
    let point = parse_expression_until(token.into_iter().chain(&mut tokens), errors, |tok| {
        tok.as_tag() == Some("marker")
    });
    let marker_type = expect_get_tag(tokens.next()).or_push(errors);
    let marker_parameters = parse_marker_params(tokens, errors);
    Some(StatementKind::Stop(Stop {
        point: point?,
        styles,
        marker_type: marker_type?,
        marker_parameters: marker_parameters?,
    }))
}

fn parse_marker_params(
    mut tokens: impl Iterator<Item = TokenResult>,
    errors: &mut Vec<Error>,
) -> Option<HashMap<Variable, Expression>> {
    let mut params = HashMap::new();
    while let Some(token) = tokens.next().or_push(errors) {
        match token.kind {
            TokenKind::Comma => {}
            TokenKind::Tag(tag) => {
                let expr = parse_delimited_expression(&mut tokens, errors);
                match params.entry(tag) {
                    // there's already an argument with this name
                    Entry::Occupied(e) => {
                        errors.push(Error::MarkerArgument {
                            argument: e.key().clone(),
                            span: token.span,
                        });
                    }
                    Entry::Vacant(e) => {
                        if let Some(expr) = expr {
                            e.insert(expr);
                        }
                    }
                };
            }
            _ => errors.push(Error::Token(token.kind, token.span)),
        }
    }
    Some(params)
}

fn expect(token: Option<TokenResult>, paren_span: Option<Span>) -> Result<Token> {
    token.transpose()?.ok_or({
        if let Some(span) = paren_span {
            Error::Parentheses(span)
        } else {
            Error::EndOfInput
        }
    })
}

fn expect_map<U>(
    token: Option<TokenResult>,
    paren_span: Option<Span>,
    map: impl FnOnce(&Token) -> Option<U>,
) -> Result<U> {
    let token = expect(token, paren_span)?;
    map(&token).ok_or(Error::Token(token.kind, token.span))
}

fn expect_get_tag(token: Option<TokenResult>) -> Result<Variable> {
    let token = expect(token, None)?;
    match token.kind {
        TokenKind::Tag(t) => Ok(t),
        _ => Err(Error::Token(token.kind, token.span)),
    }
}

fn expect_get_string(token: Option<TokenResult>) -> Result<String> {
    let token = expect(token, None)?;
    match token.kind {
        TokenKind::String(s) => Ok(s),
        _ => Err(Error::Token(token.kind, token.span)),
    }
}

fn expect_tag(token: Option<TokenResult>, tag: &str) -> Result<()> {
    let token = expect(token, None)?;
    if token.kind.as_tag() == Some(tag) {
        Ok(())
    } else {
        Err(Error::Token(token.kind, token.span))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use test_case::test_case;

    use super::{lexer::Lexer, parse_expression, parse_statement};
    use crate::{
        expressions::tests::{b, dot, expression_full, t, u, var, Expr},
        operators::{COMMA, COMMA_UNARY, FN_CALL, FN_CALL_UNARY, PAREN_UNARY},
        statement::{Segment, StatementKind, Stop},
    };

    #[test_case("1+2*3+4", [t(1), t(2), t(3), b("*"), b("+"), t(4), b("+")]; "basic arithmetic")]
    #[test_case("1-2*3+4", [t(1), t(2), t(3), b("*"), b("-"), t(4), b("+")]; "basic arithmetic 2")]
    #[test_case("1-3/2*5", [t(1), t(3), t(2), b("/"), t(5), b("*"), b("-")]; "basic arithmetic 3")]
    #[test_case("3++4", [t(3), t(4), b("++")]; "hypot")]
    #[test_case("5+-+3", [t(5), t(3), b("+-+")]; "hypot_sub")]
    #[test_case("3^4^5", [t(3), t(4), t(5), b("^"), b("^")]; "pow")]
    #[test_case("(1+2)*3+4", [t(1), t(2), b("+"), u(PAREN_UNARY), t(3), b("*"), t(4), b("+")]; "parentheses")]
    #[test_case("(1,2) + (3,4)", [t(1), t(2), b(COMMA), u(PAREN_UNARY), t(3), t(4), b(COMMA), u(PAREN_UNARY), b("+")]; "points")]
    #[test_case("(1, 2,)", [t(1), t(2), b(COMMA), u(COMMA_UNARY), u(PAREN_UNARY)]; "trailing comma")]
    #[test_case("angle (3, 3)", [t(3), t(3), b(COMMA), u(PAREN_UNARY), u("angle")]; "angle")]
    #[test_case("3* -2", [t(3), t(2), u("-"), b("*")]; "unary minus")]
    #[test_case("-(1,2)*3", [t(1), t(2), b(COMMA), u(PAREN_UNARY), u("-"), t(3), b("*")]; "unary minus 2")]
    #[test_case("3*x", [t(3), var("x"), b("*")]; "variable")]
    #[test_case("3*x.y", [t(3), var("x"), dot("y"), b("*")]; "dotted variable")]
    #[test_case(r#""foobar""#, [t("foobar")]; "string")]
    #[test_case("a() + b(1) + c(2, 3)", [var("a"), u(FN_CALL_UNARY), var("b"), t(1), b(FN_CALL), b("+"), var("c"), t(2), t(3), b(COMMA), b(FN_CALL), b("+")]; "function call")]
    fn expression<const N: usize>(input: &str, expected: [Expr; N]) {
        let mut errors = Vec::new();
        let result = parse_expression(Lexer::new(input), HashMap::new(), &mut errors)
            .ok_or(errors)
            .unwrap()
            .into_iter()
            .map(|expr| expr.kind)
            .collect::<Vec<_>>();
        assert_eq!(result, expected.map(Into::into));
    }

    fn segment<const N: usize>(start: &str, end: &str, offset: [Expr; N]) -> Segment {
        Segment {
            start: start.into(),
            end: end.into(),
            offset: expression_full(offset).into(),
        }
    }

    #[test_case(
        "a = b",
        StatementKind::Variable(
            "a".into(),
            Vec::new(),
            expression_full([var("b")]).into()
        );
        "variable assignment"
    )]
    #[test_case(
        "a.b = c",
        StatementKind::Variable(
            "a".into(),
            vec!["b".into()],
            expression_full([var("c")]).into()
        );
        "dotted variable assignment"
    )]
    #[test_case(
        "point a = b",
        StatementKind::PointSingle("a".into(), expression_full([var("b")]).into());
        "point single"
    )]
    #[test_case(
        "points from a spaced x: (1/2) b, c, (1/2) d",
        StatementKind::PointSpaced {
            from: "a".into(),
            spaced: expression_full([var("x")]).into(),
            points: vec![
                (
                    Some(expression_full([t(1), t(2), b("/"), u(PAREN_UNARY)]).into()),
                    "b".into()
                ),
                (None, "c".into()),
                (
                    Some(expression_full([t(1), t(2), b("/"), u(PAREN_UNARY)]).into()),
                    "d".into()
                ),
            ],
        };
        "points spaced"
    )]
    #[test_case(
        "points from a to (1/2) d: (1/2) b, c",
        StatementKind::PointExtend {
            from: "a".into(),
            to: (
                Some(expression_full([t(1), t(2), b("/"), u(PAREN_UNARY)]).into()),
                "d".into()
            ),
            points: vec![
                (
                    Some(expression_full([t(1), t(2), b("/"), u(PAREN_UNARY)]).into()),
                    "b".into()
                ),
                (None, "c".into()),
            ],
            is_past: false,
        };
        "points between"
    )]
    #[test_case(
        "route red: a --(1) b --(1) c",
        StatementKind::Route {
            styles: vec![],
            name: "red".into(),
            segments: vec![
                segment("a", "b", [t(1), u(PAREN_UNARY)]),
                segment("b", "c", [t(1), u(PAREN_UNARY)]),
            ],
        };
        "route"
    )]
    #[test_case(
        "route.narrow red: a --(1) b --(1) c",
        StatementKind::Route {
            styles: vec!["narrow".into()],
            name: "red".into(),
            segments: vec![
                segment("a", "b", [t(1), u(PAREN_UNARY)]),
                segment("b", "c", [t(1), u(PAREN_UNARY)]),
            ],
        };
        "route with style"
    )]
    #[test_case(
        r#"stop a marker circle r(10)"#,
        StatementKind::Stop(Stop {
            styles: vec![],
            point: expression_full([var("a")]).into(),
            marker_type: "circle".into(),
            marker_parameters: [("r".into(), expression_full([t(10), u(PAREN_UNARY)]).into())]
                .into_iter()
                .collect(),
        });
        "stop"
    )]
    #[test_case(
        r#"stop.terminus a marker double_tick length(20), angle(45)"#,
        StatementKind::Stop(Stop {
            styles: vec!["terminus".into()],
            point: expression_full([var("a")]).into(),
            marker_type: "double_tick".into(),
            marker_parameters: [
                (
                    "length".into(),
                    expression_full([t(20), u(PAREN_UNARY)]).into()
                ),
                (
                    "angle".into(),
                    expression_full([t(45), u(PAREN_UNARY)]).into()
                )
            ]
            .into_iter()
            .collect(),
        });
        "stop with style"
    )]
    #[test_case(
        r#"stop.terminus a marker text text("A station") angle(0)"#,
        StatementKind::Stop(Stop {
            styles: vec!["terminus".into()],
            point: expression_full([var("a")]).into(),
            marker_type: "text".into(),
            marker_parameters: [
                (
                    "text".into(),
                    expression_full([t("A station"), u(PAREN_UNARY)]).into()
                ),
                (
                    "angle".into(),
                    expression_full([t(0), u(PAREN_UNARY)]).into()
                )
            ]
            .into_iter()
            .collect(),
        });
        "stop with text"
    )]
    fn statement(input: &str, expected: StatementKind) {
        let result = parse_statement(Lexer::new(input), &mut Vec::new()).unwrap();
        assert_eq!(result.statement, expected);
    }
}
