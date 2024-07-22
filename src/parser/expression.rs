#![allow(unused)]

use expr_parser::{
    operator::Fixity,
    parser::{self, Element, ParserElement, Postfix, Prefix},
};

use crate::{
    expressions::Term,
    lexer::TokenKind,
    operators::{self, BinaryOperator, Precedence, UnaryOperator},
};

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Delimiter {
    Paren,
}

impl expr_parser::parser::Delimiter for Delimiter {
    fn matches(&self, other: &Self) -> bool {
        self == other
    }
}

pub enum Error {
    UnexpectedToken(TokenKind),
}

pub struct Parser;

impl parser::Parser<TokenKind> for Parser {
    type Precedence = Precedence;
    type Delimiter = Delimiter;
    type BinaryOperator = BinaryOperator;
    type UnaryOperator = UnaryOperator;
    type Term = Term;
    type Error = Error;

    fn parse_token(&self, token: TokenKind) -> Result<ParserElement<Self, TokenKind>, Self::Error> {
        Ok(match token {
            TokenKind::Tag(tag) => {
                let postfix = if let Some((precedence, op)) = BinaryOperator::get(&tag) {
                    Postfix::BinaryOperator {
                        fixity: Fixity::Left(precedence), // TODO: allow right-associative ops?
                        operator: op,
                        no_rhs: None,
                    }
                } else {
                    Postfix::None
                };
                let prefix = if let Some((precedence, op)) = UnaryOperator::get(&tag) {
                    Prefix::UnaryOperator {
                        precedence,
                        operator: op,
                        no_rhs: None,
                    }
                } else {
                    Prefix::Term {
                        term: Term::Variable(tag),
                    }
                };
                Element { prefix, postfix }
            }
            TokenKind::Comma => Element {
                prefix: Prefix::None,
                postfix: Postfix::BinaryOperator {
                    fixity: Fixity::Left(Precedence::Comma),
                    operator: operators::COMMA,
                    no_rhs: Some(operators::COMMA_UNARY),
                },
            },
            TokenKind::Number(number) => Element {
                prefix: Prefix::Term {
                    term: Term::Number(number),
                },
                postfix: Postfix::None,
            },
            TokenKind::String(string) => Element {
                prefix: Prefix::Term {
                    term: Term::String(string),
                },
                postfix: Postfix::None,
            },
            TokenKind::LeftParen => Element {
                prefix: Prefix::LeftDelimiter {
                    delimiter: Delimiter::Paren,
                    operator: Some(operators::PAREN_UNARY),
                    empty: None,
                },
                postfix: Postfix::LeftDelimiter {
                    delimiter: Delimiter::Paren,
                    operator: todo!(), // function call
                    empty: None,
                },
            },
            TokenKind::RightParen => Element {
                prefix: Prefix::RightDelimiter {
                    delimiter: Delimiter::Paren,
                },
                postfix: Postfix::RightDelimiter {
                    delimiter: Delimiter::Paren,
                },
            },
            TokenKind::Semicolon | TokenKind::Equal | TokenKind::Dot => {
                return Err(Error::UnexpectedToken(token));
            }
        })
    }
}
