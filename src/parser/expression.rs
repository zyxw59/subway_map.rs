use std::collections::HashMap;

use expr_parser::{
    operator::Fixity,
    parser::{self, Element, ParserElement, Postfix, Prefix},
    ParseError,
};

use super::{error::LexerError, lexer::TokenKind, Position};
use crate::{
    expressions::{Term, Variable},
    operators::{self, BinaryOperator, Precedence, UnaryOperator},
};

pub type ExpressionError = ParseError<UnexpectedToken, LexerError, Position>;

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Delimiter {
    Paren,
}

impl expr_parser::parser::Delimiter for Delimiter {
    fn matches(&self, other: &Self) -> bool {
        self == other
    }
}

#[derive(Debug)]
pub struct UnexpectedToken;

#[derive(Clone, Default)]
pub struct Parser {
    fn_args: HashMap<Variable, usize>,
}

impl Parser {
    pub fn new(fn_args: HashMap<Variable, usize>) -> Self {
        Self { fn_args }
    }
}

impl parser::Parser<TokenKind> for Parser {
    type Precedence = Precedence;
    type Delimiter = Delimiter;
    type BinaryOperator = BinaryOperator;
    type UnaryOperator = UnaryOperator;
    type Term = Term;
    type Error = UnexpectedToken;

    fn parse_token(&self, token: TokenKind) -> Result<ParserElement<Self, TokenKind>, Self::Error> {
        Ok(match token {
            TokenKind::Tag(tag) => {
                let postfix = if let Some((fixity, op)) = BinaryOperator::get(&tag) {
                    Postfix::BinaryOperator {
                        fixity,
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
                } else if let Some(&idx) = self.fn_args.get(&tag) {
                    Prefix::Term {
                        term: Term::FnArg(idx),
                    }
                } else {
                    Prefix::Term {
                        term: Term::Variable(tag),
                    }
                };
                Element { prefix, postfix }
            }
            TokenKind::DotTag(tag) => Element {
                prefix: Prefix::None,
                postfix: Postfix::PostfixOperator {
                    precedence: Precedence::Field,
                    operator: UnaryOperator::FieldAccess(tag),
                },
            },
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
                    operator: operators::FN_CALL,
                    empty: Some(operators::FN_CALL_UNARY),
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
            TokenKind::Semicolon => {
                return Err(UnexpectedToken);
            }
        })
    }
}
