use std::collections::{hash_map::Entry, HashMap};

use crate::error::{ParserError, Result as EResult};
use crate::expressions::{Expression, Function, Variable};
use crate::lexer::Token;
use crate::operators::{BinaryBuiltins, UnaryBuiltins};
use crate::statement::{Segment, Statement, StatementKind, Stop};
use crate::values::Value;

pub trait LexerExt: Iterator<Item = EResult<Token>> {
    fn put_back(&mut self, put_back: Token);

    fn line(&self) -> usize;

    fn into_parser(self) -> Parser<Self>
    where
        Self: Sized,
    {
        Parser { tokens: self }
    }
}

impl<'a, T> LexerExt for &'a mut T
where
    T: LexerExt,
{
    fn put_back(&mut self, put_back: Token) {
        (**self).put_back(put_back);
    }

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

pub struct Parser<T> {
    tokens: T,
}

impl<T> Parser<T>
where
    T: LexerExt,
{
    fn next(&mut self) -> Option<EResult<Token>> {
        self.tokens.next()
    }

    fn put_back(&mut self, put_back: Token) {
        self.tokens.put_back(put_back);
    }

    fn line(&self) -> usize {
        self.tokens.line()
    }

    fn parse_expression(&mut self, min_precedence: usize) -> EResult<Expression> {
        // initial left-hand side
        let mut lhs = self.parse_primary()?;
        // as long as we encounter operators with precedence >= min_precedence, we can accumulate
        // them into `lhs`.
        while let Some(tok) = self.next().transpose()? {
            if let Some(op) = tok
                .as_tag()
                .and_then(|tag| BinaryBuiltins.get(tag))
                .filter(|op| op.precedence >= min_precedence)
            {
                // we have an operator; now to get the right hand side, accumulating operators with
                // greater precedence than the current one
                let rhs = self.parse_expression(op.precedence + 1)?;
                lhs = op.expression(lhs, rhs);
            } else {
                // the token wasn't an operator, so put it back on the stack
                self.put_back(tok);
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_primary(&mut self) -> EResult<Expression> {
        match self.next().transpose()? {
            None => Err(ParserError::EndOfInput(self.line()).into()),
            Some(Token::LeftParen) => self.parse_parentheses(),
            Some(Token::Number(num)) => Ok(Expression::Value(Value::Number(num))),
            Some(Token::Tag(tag)) => match UnaryBuiltins.get(&tag) {
                Some(op) => Ok(op.expression(self.parse_expression(op.precedence)?)),
                None => {
                    let tag = self.parse_dotted_ident(tag)?;
                    let next_tok = self.next().transpose()?;
                    if let Some(Token::LeftParen) = next_tok {
                        // function call
                        let start_line = self.line();
                        let args = self.parse_comma_list()?;
                        match self.next().transpose()? {
                            Some(Token::RightParen) => {}
                            Some(tok) => return Err(ParserError::Token(tok, self.line()).into()),
                            None => return Err(ParserError::Parentheses(start_line).into()),
                        };
                        Ok(Expression::Function(tag, args))
                    } else {
                        if let Some(tok) = next_tok {
                            self.put_back(tok);
                        }
                        Ok(Expression::Variable(tag))
                    }
                }
            },
            Some(Token::String(string)) => Ok(Expression::Value(Value::String(string))),
            Some(tok) => Err(ParserError::Token(tok, self.line()).into()),
        }
    }

    fn parse_dotted_ident(&mut self, tag: String) -> EResult<String> {
        let mut arr = vec![tag];
        while let Some(tok) = self.next().transpose()? {
            match tok {
                Token::Dot => arr.push(expect!(self, Token::Tag(tag) => tag)),
                _ => {
                    self.put_back(tok);
                    break;
                }
            }
        }
        Ok(arr.join("."))
    }

    fn parse_parentheses(&mut self) -> EResult<Expression> {
        let start_line = self.line();
        let mut list = self.parse_comma_list()?;
        let exp = match list.len() {
            1 => list.pop().unwrap(),
            2 => {
                let y = list.pop().unwrap();
                let x = list.pop().unwrap();
                Expression::Point(Box::new((x, y)))
            }
            n => return Err(ParserError::ParenList(n, start_line).into()),
        };
        match self.next().transpose()? {
            Some(Token::RightParen) => Ok(exp),
            Some(tok) => Err(ParserError::Token(tok, self.line()).into()),
            None => Err(ParserError::Parentheses(start_line).into()),
        }
    }

    fn parse_comma_list(&mut self) -> EResult<Vec<Expression>> {
        let mut list = Vec::new();
        while let Some(tok) = self.next().transpose()? {
            match tok {
                Token::Comma => {}
                Token::RightParen | Token::Semicolon => {
                    self.put_back(tok);
                    break;
                }
                _ => self.put_back(tok),
            }
            list.push(self.parse_expression(0)?);
        }
        Ok(list)
    }

    /// Parses a function definition.
    ///
    /// On success, returns a tuple of the function name and the function definition.
    fn parse_function_def(&mut self) -> EResult<(Variable, Function)> {
        let name = expect!(self, Token::Tag(name) => name);
        expect!(self, Token::LeftParen);
        // maps argument names to their index in the function signature
        let mut args = HashMap::new();
        let mut index = 0;
        let start_line = self.line();
        loop {
            match self.next().transpose()? {
                // a named argument
                Some(Token::Tag(arg)) => {
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
                        Some(Token::Comma) => {}
                        Some(Token::RightParen) => break,
                        Some(tok) => return Err(ParserError::Token(tok, self.line()).into()),
                        None => return Err(ParserError::Parentheses(start_line).into()),
                    }
                }
                // end of the arguments
                Some(Token::RightParen) => break,
                Some(tok) => return Err(ParserError::Token(tok, self.line()).into()),
                None => return Err(ParserError::Parentheses(start_line).into()),
            }
        }
        expect!(self, Token::Equal);
        // get the function body, as an expression tree
        let expression = self.parse_expression(0)?;
        Ok((
            name.clone(),
            Function {
                name,
                args,
                expression,
            },
        ))
    }

    fn parse_comma_point_list(&mut self) -> EResult<Vec<(Option<Expression>, Variable)>> {
        let mut points = Vec::new();
        while let Some(tok) = self.next().transpose()? {
            let point = match tok {
                Token::Comma => continue,
                Token::LeftParen => {
                    let multiplier = self.parse_expression(0)?;
                    expect!(self, Token::RightParen);
                    let ident = expect!(self, Token::Tag(tag) => tag);
                    (Some(multiplier), ident)
                }
                Token::Tag(ident) => (None, ident),
                _ => return Err(ParserError::Token(tok, self.line()).into()),
            };
            points.push(point);
            match self.next().transpose()? {
                None => break,
                Some(Token::Comma) => continue,
                Some(Token::Semicolon) => {
                    self.put_back(Token::Semicolon);
                    break;
                }
                Some(tok) => return Err(ParserError::Token(tok, self.line()).into()),
            }
        }
        Ok(points)
    }

    fn parse_route(&mut self) -> EResult<Vec<Segment>> {
        let mut route = Vec::new();
        let mut start = expect!(self, Token::Tag(tag) => tag);
        while let Some(tok) = self.next().transpose()? {
            self.put_back(tok);
            expect! { self,
                Token::Tag(ref tag) if tag == "--" => {},
                Token::Semicolon => {
                    self.put_back(Token::Semicolon);
                    break
                },
            };
            expect!(self, Token::LeftParen);
            let offset = self.parse_expression(0)?;
            expect!(self, Token::RightParen);
            let end = expect!(self, Token::Tag(tag) => tag);
            let next = end.clone();
            route.push(Segment { start, end, offset });
            start = next;
        }
        Ok(route)
    }

    fn parse_statement(&mut self) -> EResult<Option<StatementKind>> {
        match self.next().transpose()? {
            // tag; start of an assignment expression or function definition
            Some(Token::Tag(tag)) => {
                match tag.as_ref() {
                    // function definition
                    "fn" => {
                        let (func_name, func) = self.parse_function_def()?;
                        Ok(Some(StatementKind::Function(func_name, func)))
                    }
                    // single point
                    "point" => {
                        let name = expect!(self, Token::Tag(tag) => tag);
                        expect!(self, Token::Equal);
                        let expr = self.parse_expression(0)?;
                        Ok(Some(StatementKind::PointSingle(name, expr)))
                    }
                    // sequence of points
                    "points" => self.parse_points_statement(),
                    // route
                    "route" => {
                        let styles = self.parse_dot_list()?;
                        let name = expect!(self, Token::Tag(name) => name);
                        expect!(self, Token::Tag(ref tag) if tag == ":");
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
                            Token::String(style) => Ok(Some(StatementKind::Style(style)))
                        }
                    }
                    // title
                    "title" => {
                        expect! { self,
                            Token::String(title) => Ok(Some(StatementKind::Title(title)))
                        }
                    }
                    // other (variable assignment)
                    _ => {
                        let tag = self.parse_dotted_ident(tag)?;
                        expect!(self, Token::Equal);
                        let expr = self.parse_expression(0)?;
                        Ok(Some(StatementKind::Variable(tag, expr)))
                    }
                }
            }
            // semicolon; null statement
            Some(Token::Semicolon) => {
                self.put_back(Token::Semicolon);
                Ok(Some(StatementKind::Null))
            }
            // other token; unexpected
            Some(tok) => Err(ParserError::Token(tok, self.line()).into()),
            // empty statement, end of input
            None => Ok(None),
        }
    }

    fn parse_dot_list(&mut self) -> EResult<Vec<Variable>> {
        let mut list = Vec::new();
        loop {
            match self.next().transpose()? {
                Some(Token::Dot) => expect!(self, Token::Tag(tag) => list.push(tag)),
                Some(tok) => {
                    self.put_back(tok);
                    break;
                }
                _ => break,
            }
        }
        Ok(list)
    }

    fn parse_points_statement(&mut self) -> EResult<Option<StatementKind>> {
        expect!(self, Token::Tag(ref tag) if tag == "from");
        let from = expect!(self, Token::Tag(tag) => tag);
        let kind = expect!(self, Token::Tag(tag) => tag);
        match kind.as_ref() {
            // from ... spaced
            "spaced" => {
                let spaced = self.parse_expression(0)?;
                expect!(self, Token::Tag(ref tag) if tag == ":");
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
            _ => Err(ParserError::Token(Token::Tag(kind), self.line()).into()),
        }
    }

    fn parse_points_extend_statement(
        &mut self,
        from: Variable,
        is_past: bool,
    ) -> EResult<Option<StatementKind>> {
        let to = expect! { self,
            Token::LeftParen => {
                let multiplier = self.parse_expression(0)?;
                expect!(self, Token::RightParen);
                let ident = expect!(self, Token::Tag(tag) => tag);
                (Some(multiplier), ident)
            },
            Token::Tag(ident) => (None, ident),
        };
        expect!(self, Token::Tag(ref tag) if tag == ":");
        let points = self.parse_comma_point_list()?;
        Ok(Some(StatementKind::PointExtend {
            from,
            to,
            points,
            is_past,
        }))
    }

    fn parse_stop_statement(&mut self) -> EResult<Option<StatementKind>> {
        let styles = self.parse_dot_list()?;
        let point = self.parse_expression(0)?;
        expect!(self, Token::Tag(ref tag) if tag == "marker");
        let marker_type = expect!(self, Token::Tag(tag) => tag);
        let marker_parameters = self.parse_marker_params()?;
        Ok(Some(StatementKind::Stop(Stop {
            point,
            styles,
            marker_type,
            marker_parameters,
        })))
    }

    fn parse_marker_params(&mut self) -> EResult<HashMap<String, Expression>> {
        let mut params = HashMap::new();
        while let Some(tok) = self.next().transpose()? {
            match tok {
                Token::Comma => continue,
                Token::Semicolon => {
                    self.put_back(tok);
                    break;
                }
                Token::Tag(tag) => {
                    expect!(self, Token::Equal);
                    let expr = self.parse_expression(0)?;
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
                tok => return Err(ParserError::Token(tok, self.line()).into()),
            }
        }
        Ok(params)
    }
}

impl<T> Iterator for Parser<T>
where
    T: LexerExt,
{
    type Item = EResult<Statement>;

    fn next(&mut self) -> Option<EResult<Statement>> {
        self.parse_statement().transpose().map(|res| {
            res.and_then(|statement| {
                expect!(self, Token::Semicolon);
                Ok(Statement {
                    statement,
                    line: self.line(),
                })
            })
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::statement::{StatementKind, Stop};

    use super::LexerExt;

    macro_rules! assert_expression {
        ($text:expr, ($($expr:tt)+)) => {{
            let result = Lexer::new($text.as_bytes())
                .into_parser()
                .parse_expression(0)
                .unwrap();
            assert_eq!(result, expression!($($expr)+));
        }};
    }

    #[test]
    fn basic_arithmetic() {
        assert_expression!("1+2*3+4", ("+", ("+", 1, ("*", 2, 3)), 4));
    }

    #[test]
    fn basic_arithmetic_2() {
        assert_expression!("1-2*3+4", ("+", ("-", 1, ("*", 2, 3)), 4));
    }

    #[test]
    fn basic_arithmetic_3() {
        assert_expression!("1-3/2*5", ("-", 1, ("*", ("/", 3, 2), 5)));
    }

    #[test]
    fn hypot() {
        assert_expression!("3++4", ("++", 3, 4));
    }

    #[test]
    fn hypot_sub() {
        assert_expression!("5+-+3", ("+-+", 5, 3));
    }

    #[test]
    fn pow() {
        assert_expression!("3^4", ("^", 3, 4));
    }

    #[test]
    fn parentheses() {
        assert_expression!("(1+2)*3+4", ("+", ("*", ("+", 1, 2), 3), 4));
    }

    #[test]
    fn points() {
        assert_expression!("(1,2) + (3,4)", ("+", (@1, 2), (@3, 4)));
    }

    #[test]
    fn dot_product() {
        assert_expression!("(1,2) * (3,4)", ("*", (@1, 2), (@3, 4)));
    }

    #[test]
    fn scalar_product() {
        assert_expression!("3 * (1,2)", ("*", 3, (@1, 2)));
    }

    #[test]
    fn angle() {
        assert_expression!("angle (3, 3)", ("angle", (@3, 3)));
    }

    #[test]
    fn unary_minus() {
        assert_expression!("3* -2", ("*", 3, ("-", 2)));
    }

    #[test]
    fn unary_minus_3() {
        assert_expression!("-(1,2)*(3,4)", ("-", ("*", (@1, 2), (@3, 4))));
    }

    #[test]
    fn variable() {
        assert_expression!("3*x", ("*", 3, (#"x")));
    }

    #[test]
    fn dotted_variable() {
        assert_expression!("3*x.y", ("*", 3, (#"x.y")));
    }

    #[test]
    fn string() {
        assert_expression!(r#""foobar""#, (@"foobar"));
    }

    macro_rules! assert_statement {
        ($text:expr, $statement:expr) => {{
            let result = Lexer::new($text.as_bytes())
                .into_parser()
                .parse_statement()
                .unwrap()
                .unwrap();
            assert_eq!(result, $statement);
        }};
    }

    #[test]
    fn variable_assignment() {
        assert_statement!(
            "a = b",
            StatementKind::Variable("a".into(), expression!(#"b"))
        );
    }

    #[test]
    fn dotted_variable_assignment() {
        assert_statement!(
            "a.b = c",
            StatementKind::Variable("a.b".into(), expression!(#"c"))
        );
    }

    #[test]
    fn point_single() {
        assert_statement!(
            "point a = b",
            StatementKind::PointSingle("a".into(), expression!(#"b"))
        );
    }

    #[test]
    fn points_spaced() {
        assert_statement!(
            "points from a spaced x: (1/2) b, c, (1/2) d",
            StatementKind::PointSpaced {
                from: "a".into(),
                spaced: expression!(#"x"),
                points: vec![
                    (Some(expression!("/", 1, 2)), "b".into()),
                    (None, "c".into()),
                    (Some(expression!("/", 1, 2)), "d".into()),
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
                to: (Some(expression!("/", 1, 2)), "d".into()),
                points: vec![
                    (Some(expression!("/", 1, 2)), "b".into()),
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
                segments: vec![segment!("a", "b", 1), segment!("b", "c", 1),],
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
                segments: vec![segment!("a", "b", 1), segment!("b", "c", 1),],
            }
        )
    }

    #[test]
    fn stop() {
        assert_statement!(
            r#"stop a marker circle r: 10"#,
            StatementKind::Stop(Stop {
                styles: vec![],
                point: expression!(#"a"),
                marker_type: "circle".into(),
                marker_parameters: [("r".to_owned(), expression!(10))].into_iter().collect(),
            })
        )
    }

    #[test]
    fn stop_with_style() {
        assert_statement!(
            r#"stop.terminus a marker double_tick length: 20, angle: 45"#,
            StatementKind::Stop(Stop {
                styles: vec!["terminus".into()],
                point: expression!(#"a"),
                marker_type: "double_tick".into(),
                marker_parameters: [
                    ("length".to_owned(), expression!(20)),
                    ("angle".to_owned(), expression!(45))
                ]
                .into_iter()
                .collect(),
            })
        )
    }

    #[test]
    fn stop_with_text() {
        assert_statement!(
            r#"stop.terminus a marker text text: "A station", angle: 0"#,
            StatementKind::Stop(Stop {
                styles: vec!["terminus".into()],
                point: expression!(#"a"),
                marker_type: "text".into(),
                marker_parameters: [
                    ("text".to_owned(), expression!(@"A station")),
                    ("angle".to_owned(), expression!(0))
                ]
                .into_iter()
                .collect(),
            })
        )
    }
}
