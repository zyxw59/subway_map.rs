use std::collections::{hash_map::Entry, HashMap};

use expr_parser::{parser::Parser as _, token::IterTokenizer};

use crate::{
    error::{ParserError, Result},
    expressions::{Expression2 as Expression, Function, Variable},
    lexer::{Token, TokenKind},
    statement::{Segment, Statement, StatementKind, Stop},
};

mod expression;

pub trait LexerExt: Iterator<Item = Result<Token>> {
    fn line(&self) -> usize;

    fn into_parser(self) -> Parser<Self>
    where
        Self: Sized,
    {
        Parser {
            tokens: self,
            peek: None,
        }
    }
}

impl<'a, T> LexerExt for &'a mut T
where
    T: LexerExt,
{
    fn line(&self) -> usize {
        (**self).line()
    }
}

macro_rules! expect {
    ($self:ident, $token:pat $(if $guard:expr)?) => {
        expect!($self, $token $(if $guard)? => ())
    };
    ($self:ident, $($token:pat $(if $guard:expr)? => $capture:expr),*$(,)*) => {
        match $self.next().transpose()? {
            $(Some($token) $(if $guard)? => $capture),*,
            #[allow(unreachable_patterns)] // to allow for expect!(self, tok, tok)
            Some(tok) => return Err(ParserError::Token(tok, $self.line()).into()),
            None => return Err(ParserError::EndOfInput($self.line()).into()),
        }
    };
}

macro_rules! expect_peek {
    ($self:ident, $token:pat $(if $guard:expr)?) => {
        expect_peek!($self, $token $(if $guard)? => ())
    };
    ($self:ident, $($token:pat $(if $guard:expr)? => $capture:expr),*$(,)*) => {
        match $self.peek()? {
            $(Some($token) $(if $guard)? => $capture),*,
            #[allow(unreachable_patterns)] // to allow for expect!(self, tok, tok)
            Some(_) => return Err(ParserError::Token($self.take_peek().unwrap(), $self.line()).into()),
            None => return Err(ParserError::EndOfInput($self.line()).into()),
        }
    };
}

pub struct Parser<T> {
    tokens: T,
    peek: Option<Token>,
}

impl<T> Parser<T>
where
    T: LexerExt,
{
    fn next(&mut self) -> Option<Result<TokenKind>> {
        self.next_token().map(|res| res.map(|tok| tok.kind))
    }

    fn next_token(&mut self) -> Option<Result<Token>> {
        self.peek.take().map(Ok).or_else(|| self.tokens.next())
    }

    fn peek(&mut self) -> Result<Option<&TokenKind>> {
        self.peek = self.next_token().transpose()?;
        Ok(self.peek.as_ref().map(|tok| &tok.kind))
    }

    fn take_peek(&mut self) -> Option<TokenKind> {
        self.peek.take().map(|tok| tok.kind)
    }

    fn line(&self) -> usize {
        self.tokens.line()
    }

    fn tokens_until<F: FnMut(&TokenKind) -> bool>(&mut self, until: F) -> TokensUntil<'_, T, F> {
        TokensUntil {
            parser: self,
            until,
        }
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        expression::Parser
            .parse(IterTokenizer(
                self.tokens_until(|t| t == &TokenKind::Semicolon),
            ))
            .map_err(|_| todo!())
    }

    fn parse_delimited_expression(&mut self) -> Result<Expression> {
        expression::Parser
            .parse_one_term(IterTokenizer(
                self.tokens_until(|t| t == &TokenKind::Semicolon),
            ))
            .map_err(|_| todo!())
    }

    /// Parses a function definition.
    ///
    /// On success, returns a tuple of the function name and the function definition.
    fn parse_function_def(&mut self) -> Result<(Variable, Function)> {
        let name = expect!(self, TokenKind::Tag(name) => name);
        expect!(self, TokenKind::LeftParen);
        // maps argument names to their index in the function signature
        let mut args = HashMap::new();
        let mut index = 0;
        let start_line = self.line();
        loop {
            match self.next().transpose()? {
                // a named argument
                Some(TokenKind::Tag(arg)) => {
                    // insert the new argument into the hashmap
                    match args.entry(arg) {
                        // there's already an argument with this name
                        Entry::Occupied(e) => {
                            return Err(ParserError::Argument {
                                argument: e.remove_entry().0,
                                function: name,
                                line: self.line(),
                            }
                            .into());
                        }
                        Entry::Vacant(e) => e.insert(index),
                    };
                    index += 1;
                    match self.next().transpose()? {
                        Some(TokenKind::Comma) => {}
                        Some(TokenKind::RightParen) => break,
                        Some(tok) => return Err(ParserError::Token(tok, self.line()).into()),
                        None => return Err(ParserError::Parentheses(start_line).into()),
                    }
                }
                // end of the arguments
                Some(TokenKind::RightParen) => break,
                Some(tok) => return Err(ParserError::Token(tok, self.line()).into()),
                None => return Err(ParserError::Parentheses(start_line).into()),
            }
        }
        expect!(self, TokenKind::Equal);
        // get the function body, as an expression tree
        let expression = self.parse_expression()?;
        Ok((
            name.clone(),
            Function {
                name,
                args,
                expression,
            },
        ))
    }

    fn parse_comma_point_list(&mut self) -> Result<Vec<(Option<Expression>, Variable)>> {
        let mut points = Vec::new();
        while let Some(tok) = self.peek()? {
            let point = match tok {
                TokenKind::Comma => {
                    self.take_peek();
                    continue;
                }
                TokenKind::LeftParen => {
                    let multiplier = self.parse_delimited_expression()?;
                    let ident = expect!(self, TokenKind::Tag(tag) => tag);
                    (Some(multiplier), ident)
                }
                TokenKind::Tag(_) => {
                    let Some(TokenKind::Tag(ident)) = self.take_peek() else {
                        unreachable!()
                    };
                    (None, ident)
                }
                _ => return Err(ParserError::Token(self.take_peek().unwrap(), self.line()).into()),
            };
            points.push(point);
            match self.peek()? {
                None => break,
                Some(TokenKind::Comma) => {
                    self.take_peek();
                }
                Some(TokenKind::Semicolon) => {
                    break;
                }
                Some(_) => {
                    return Err(ParserError::Token(self.take_peek().unwrap(), self.line()).into())
                }
            }
        }
        Ok(points)
    }

    fn parse_route(&mut self) -> Result<Vec<Segment>> {
        let mut route = Vec::new();
        let mut start = expect!(self, TokenKind::Tag(tag) => tag);
        while self.peek()?.is_some() {
            expect_peek! { self,
                TokenKind::Tag(ref tag) if tag == "--" => {},
                TokenKind::Semicolon => break,
            };
            expect_peek!(self, TokenKind::LeftParen);
            let offset = self.parse_delimited_expression()?;
            let end = expect!(self, TokenKind::Tag(tag) => tag);
            let next = end.clone();
            route.push(Segment { start, end, offset });
            start = next;
        }
        Ok(route)
    }

    fn parse_statement(&mut self) -> Result<Option<StatementKind>> {
        match self.peek()? {
            // tag; start of an assignment expression or function definition
            Some(TokenKind::Tag(tag)) => {
                match tag.as_ref() {
                    // function definition
                    "fn" => {
                        let (func_name, func) = self.parse_function_def()?;
                        Ok(Some(StatementKind::Function(func_name, func)))
                    }
                    // single point
                    "point" => {
                        let name = expect!(self, TokenKind::Tag(tag) => tag);
                        expect!(self, TokenKind::Equal);
                        let expr = self.parse_expression()?;
                        Ok(Some(StatementKind::PointSingle(name, expr)))
                    }
                    // sequence of points
                    "points" => self.parse_points_statement(),
                    // route
                    "route" => {
                        let styles = self.parse_dot_list()?;
                        let name = expect!(self, TokenKind::Tag(name) => name);
                        expect!(self, TokenKind::Tag(ref tag) if tag == ":");
                        let segments = self.parse_route()?;
                        Ok(Some(StatementKind::Route {
                            name,
                            styles,
                            segments,
                        }))
                    }
                    // stop
                    "stop" => self.parse_stop_statement(),
                    // stylesheet
                    "style" => {
                        expect! { self,
                            TokenKind::String(style) => Ok(Some(StatementKind::Style(style)))
                        }
                    }
                    // title
                    "title" => {
                        expect! { self,
                            TokenKind::String(title) => Ok(Some(StatementKind::Title(title)))
                        }
                    }
                    // other (variable assignment)
                    _ => {
                        let Some(TokenKind::Tag(tag)) = self.take_peek() else {
                            unreachable!()
                        };
                        expect!(self, TokenKind::Equal);
                        let expr = self.parse_expression()?;
                        Ok(Some(StatementKind::Variable(tag, expr)))
                    }
                }
            }
            // semicolon; null statement
            Some(TokenKind::Semicolon) => Ok(Some(StatementKind::Null)),
            // other token; unexpected
            Some(_) => Err(ParserError::Token(self.take_peek().unwrap(), self.line()).into()),
            // empty statement, end of input
            None => Ok(None),
        }
    }

    fn parse_dot_list(&mut self) -> Result<Vec<Variable>> {
        todo!();
        // let mut list = Vec::new();
        // loop {
        //     match self.peek()? {
        //         Some(TokenKind::Dot) => {
        //             self.take_peek();
        //             expect!(self, TokenKind::Tag(tag) => list.push(tag));
        //         }
        //         Some(_) => {
        //             break;
        //         }
        //         _ => break,
        //     }
        // }
        // Ok(list)
    }

    fn parse_points_statement(&mut self) -> Result<Option<StatementKind>> {
        expect!(self, TokenKind::Tag(ref tag) if tag == "from");
        let from = expect!(self, TokenKind::Tag(tag) => tag);
        let kind = expect!(self, TokenKind::Tag(tag) => tag);
        match kind.as_ref() {
            // from ... spaced
            "spaced" => {
                let spaced = self.parse_expression()?;
                expect!(self, TokenKind::Tag(ref tag) if tag == ":");
                let points = self.parse_comma_point_list()?;
                Ok(Some(StatementKind::PointSpaced {
                    from,
                    spaced,
                    points,
                }))
            }
            // from ... to
            "to" => self.parse_points_extend_statement(from, false),
            // from ... past
            "past" => self.parse_points_extend_statement(from, true),
            _ => Err(ParserError::Token(TokenKind::Tag(kind), self.line()).into()),
        }
    }

    fn parse_points_extend_statement(
        &mut self,
        from: Variable,
        is_past: bool,
    ) -> Result<Option<StatementKind>> {
        let to = expect_peek! { self,
            TokenKind::LeftParen => {
                let multiplier = self.parse_delimited_expression()?;
                let ident = expect!(self, TokenKind::Tag(tag) => tag);
                (Some(multiplier), ident)
            },
            TokenKind::Tag(_) => {
                let Some(TokenKind::Tag(ident)) = self.take_peek() else { unreachable!() };
                (None, ident)
            },
        };
        expect!(self, TokenKind::Tag(ref tag) if tag == ":");
        let points = self.parse_comma_point_list()?;
        Ok(Some(StatementKind::PointExtend {
            from,
            to,
            points,
            is_past,
        }))
    }

    fn parse_stop_statement(&mut self) -> Result<Option<StatementKind>> {
        let styles = self.parse_dot_list()?;
        let point = self.parse_expression()?;
        expect!(self, TokenKind::Tag(ref tag) if tag == "marker");
        let marker_type = expect!(self, TokenKind::Tag(tag) => tag);
        let marker_parameters = self.parse_marker_params()?;
        Ok(Some(StatementKind::Stop(Stop {
            point,
            styles,
            marker_type,
            marker_parameters,
        })))
    }

    fn parse_marker_params(&mut self) -> Result<HashMap<String, Expression>> {
        let mut params = HashMap::new();
        while let Some(tok) = self.peek()? {
            match tok {
                TokenKind::Comma => {
                    self.take_peek();
                }
                TokenKind::Semicolon => break,
                TokenKind::Tag(_) => {
                    let Some(TokenKind::Tag(tag)) = self.take_peek() else {
                        unreachable!()
                    };
                    expect!(self, TokenKind::Equal);
                    let expr = self.parse_expression()?;
                    match params.entry(tag) {
                        // there's already an argument with this name
                        Entry::Occupied(e) => {
                            return Err(ParserError::Argument {
                                argument: e.remove_entry().0,
                                function: "marker".to_owned(),
                                line: self.line(),
                            }
                            .into());
                        }
                        Entry::Vacant(e) => e.insert(expr),
                    };
                }
                _ => return Err(ParserError::Token(self.take_peek().unwrap(), self.line()).into()),
            }
        }
        Ok(params)
    }
}

impl<T> Iterator for Parser<T>
where
    T: LexerExt,
{
    type Item = Result<Statement>;

    fn next(&mut self) -> Option<Result<Statement>> {
        self.parse_statement().transpose().map(|res| {
            res.and_then(|statement| {
                expect!(self, TokenKind::Semicolon);
                Ok(Statement {
                    statement,
                    line: self.line(),
                })
            })
        })
    }
}

struct TokensUntil<'a, T, F> {
    parser: &'a mut Parser<T>,
    until: F,
}

impl<T, F> Iterator for TokensUntil<'_, T, F>
where
    T: LexerExt,
    F: FnMut(&TokenKind) -> bool,
{
    type Item = Result<Token>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.parser.peek() {
            Ok(Some(tok)) if (self.until)(tok) => None,
            Err(err) => Some(Err(err)),
            _ => self.parser.next_token(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        expressions::tests::{b, u, var},
        lexer::Lexer,
        // statement::{StatementKind, Stop},
    };

    macro_rules! assert_expression {
        ($text:expr, [$($expr:tt)*]) => {{
            let result = Lexer::new($text.as_bytes())
                .into_parser()
                .parse_expression()
                .unwrap()
                .into_iter()
                .map(|expr| expr.kind)
                .collect::<Vec<_>>();
            assert_eq!(result, crate::expressions::tests::expression!($($expr)*));
        }};
    }

    use super::LexerExt;

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
        assert_expression!("3^4", [3, 4, b("^")]);
    }

    #[test]
    fn parentheses() {
        // TODO: paren operator
        // assert_expression!("(1+2)*3+4", ("+", ("*", ("+", 1, 2), 3), 4));
    }

    #[test]
    fn points() {
        // TODO: paren operator
        // assert_expression!("(1,2) + (3,4)", ("+", (@1, 2), (@3, 4)));
    }

    #[test]
    fn dot_product() {
        // TODO: paren operator
        // assert_expression!("(1,2) * (3,4)", ("*", (@1, 2), (@3, 4)));
    }

    #[test]
    fn scalar_product() {
        // TODO: paren operator
        // assert_expression!("3 * (1,2)", ("*", 3, (@1, 2)));
    }

    #[test]
    fn angle() {
        // TODO: paren operator
        // assert_expression!("angle (3, 3)", ("angle", (@3, 3)));
    }

    #[test]
    fn unary_minus() {
        assert_expression!("3* -2", [3, 2, u("-"), b("*")]);
    }

    #[test]
    fn unary_minus_3() {
        // TODO: paren operator
        // assert_expression!("-(1,2)*(3,4)", ("-", ("*", (@1, 2), (@3, 4))));
    }

    #[test]
    fn variable() {
        assert_expression!("3*x", [3, var("x"), b("*")]);
    }

    #[test]
    fn dotted_variable() {
        // TODO: dot operator
        // assert_expression!("3*x.y", ("*", 3, (#"x.y")));
    }

    #[test]
    fn string() {
        assert_expression!(r#""foobar""#, ["foobar"]);
    }

    // macro_rules! assert_statement {
    //     ($text:expr, $statement:expr) => {{
    //         let result = Lexer::new($text.as_bytes())
    //             .into_parser()
    //             .parse_statement()
    //             .unwrap()
    //             .unwrap();
    //         assert_eq!(result, $statement);
    //     }};
    // }

    // #[test]
    // fn variable_assignment() {
    //     assert_statement!(
    //         "a = b",
    //         StatementKind::Variable("a".into(), expression!(#"b"))
    //     );
    // }

    // #[test]
    // fn dotted_variable_assignment() {
    //     assert_statement!(
    //         "a.b = c",
    //         StatementKind::Variable("a.b".into(), expression!(#"c"))
    //     );
    // }

    // #[test]
    // fn point_single() {
    //     assert_statement!(
    //         "point a = b",
    //         StatementKind::PointSingle("a".into(), expression!(#"b"))
    //     );
    // }

    // #[test]
    // fn points_spaced() {
    //     assert_statement!(
    //         "points from a spaced x: (1/2) b, c, (1/2) d",
    //         StatementKind::PointSpaced {
    //             from: "a".into(),
    //             spaced: expression!(#"x"),
    //             points: vec![
    //                 (Some(expression!("/", 1, 2)), "b".into()),
    //                 (None, "c".into()),
    //                 (Some(expression!("/", 1, 2)), "d".into()),
    //             ],
    //         }
    //     );
    // }

    // #[test]
    // fn points_between() {
    //     assert_statement!(
    //         "points from a to (1/2) d: (1/2) b, c",
    //         StatementKind::PointExtend {
    //             from: "a".into(),
    //             to: (Some(expression!("/", 1, 2)), "d".into()),
    //             points: vec![
    //                 (Some(expression!("/", 1, 2)), "b".into()),
    //                 (None, "c".into()),
    //             ],
    //             is_past: false,
    //         }
    //     );
    // }

    // #[test]
    // fn route() {
    //     assert_statement!(
    //         "route red: a --(1) b --(1) c",
    //         StatementKind::Route {
    //             styles: vec![],
    //             name: "red".into(),
    //             segments: vec![segment!("a", "b", 1), segment!("b", "c", 1),],
    //         }
    //     )
    // }

    // #[test]
    // fn route_with_style() {
    //     assert_statement!(
    //         "route.narrow red: a --(1) b --(1) c",
    //         StatementKind::Route {
    //             styles: vec!["narrow".into()],
    //             name: "red".into(),
    //             segments: vec![segment!("a", "b", 1), segment!("b", "c", 1),],
    //         }
    //     )
    // }

    // #[test]
    // fn stop() {
    //     assert_statement!(
    //         r#"stop a marker circle r=10"#,
    //         StatementKind::Stop(Stop {
    //             styles: vec![],
    //             point: expression!(#"a"),
    //             marker_type: "circle".into(),
    //             marker_parameters: [("r".to_owned(), expression!(10))].into_iter().collect(),
    //         })
    //     )
    // }

    // #[test]
    // fn stop_with_style() {
    //     assert_statement!(
    //         r#"stop.terminus a marker double_tick length=20, angle=45"#,
    //         StatementKind::Stop(Stop {
    //             styles: vec!["terminus".into()],
    //             point: expression!(#"a"),
    //             marker_type: "double_tick".into(),
    //             marker_parameters: [
    //                 ("length".to_owned(), expression!(20)),
    //                 ("angle".to_owned(), expression!(45))
    //             ]
    //             .into_iter()
    //             .collect(),
    //         })
    //     )
    // }

    // #[test]
    // fn stop_with_text() {
    //     assert_statement!(
    //         r#"stop.terminus a marker text text="A station", angle=0"#,
    //         StatementKind::Stop(Stop {
    //             styles: vec!["terminus".into()],
    //             point: expression!(#"a"),
    //             marker_type: "text".into(),
    //             marker_parameters: [
    //                 ("text".to_owned(), expression!(@"A station")),
    //                 ("angle".to_owned(), expression!(0))
    //             ]
    //             .into_iter()
    //             .collect(),
    //         })
    //     )
    // }
}
