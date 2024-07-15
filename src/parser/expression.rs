use expr_parser::{
    operator::Fixity,
    parser::{self, Element, ParserElement, Postfix, Prefix},
};

use crate::{
    expressions::Term,
    lexer::TokenKind,
    operators::{
        builtins, BinaryBuiltins, BinaryOperator, Precedence, UnaryBuiltins, UnaryOperator,
    },
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
    type BinaryOperator = &'static BinaryOperator;
    type UnaryOperator = &'static UnaryOperator;
    type Term = Term;
    type Error = Error;

    fn parse_token(&self, token: TokenKind) -> Result<ParserElement<Self, TokenKind>, Self::Error> {
        Ok(match token {
            TokenKind::Tag(tag) => {
                let postfix = if let Some(op) = BinaryBuiltins.get(&tag) {
                    Postfix::BinaryOperator {
                        fixity: Fixity::Left(op.precedence),
                        operator: op,
                        no_rhs: None,
                    }
                } else {
                    Postfix::None
                };
                let prefix = if let Some(op) = UnaryBuiltins.get(&tag) {
                    Prefix::UnaryOperator {
                        precedence: op.precedence,
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
            TokenKind::Dot => Element {
                prefix: Prefix::None,
                postfix: Postfix::BinaryOperator {
                    fixity: Fixity::Left(Precedence::Dot),
                    operator: todo!(), // field access
                    no_rhs: None,
                },
            },
            TokenKind::Comma => Element {
                prefix: Prefix::None,
                postfix: Postfix::BinaryOperator {
                    fixity: Fixity::Left(Precedence::Comma),
                    operator: &builtins::COMMA,
                    no_rhs: Some(&builtins::COMMA_UNARY),
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
                    operator: Some(&builtins::PAREN_UNARY),
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
            TokenKind::Semicolon | TokenKind::Equal => {
                return Err(Error::UnexpectedToken(token));
            }
        })
    }
}