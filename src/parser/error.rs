use std::fmt::{self, Display, Formatter};

use crate::{lexer::error::LexerError, span::Span, token::Token};

#[derive(Debug, PartialEq)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum ParserErrorKind {
    Lexer(LexerError),
    UnexpectedEof,
    Unexpected { expected: Token, actual: Token },
    UnexpectedToken(Token),
    InvalidNumber(String),
}

pub type ParserResult<T> = Result<T, ParserError>;

impl ParserError {
    pub fn new(kind: ParserErrorKind, span: Span) -> Self {
        Self { span, kind }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ParserErrorKind::Lexer(err) => write!(f, "lexer error: {}", err),
            kind => write!(f, "{}: {:?}", self.span, kind),
        }
    }
}

macro_rules! error_kind_variants {
    ($(($variant:ident, $type:ty)),* $(,)?) => {
        $(
            impl From<$type> for ParserErrorKind {
                fn from(error: $type) -> Self {
                    ParserErrorKind::$variant(error)
                }
            }

            impl From<$type> for ParserError {
                fn from(error: $type) -> Self {
                    let span = error.span.clone();
                    Self::new(error.into(), span)
                }
            }
        )*
    };
}

error_kind_variants![(Lexer, LexerError)];

impl Display for ParserErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ParserErrorKind::Lexer(err) => write!(f, "lexer error: {}", err),
            ParserErrorKind::UnexpectedEof => write!(f, "unexpected EOF"),
            ParserErrorKind::Unexpected { expected, actual } => {
                write!(
                    f,
                    "unexpected token: expected {:?}, got {:?}",
                    expected, actual
                )
            }
            ParserErrorKind::UnexpectedToken(token) => {
                write!(f, "unexpected token: {:?}", token)
            }
            ParserErrorKind::InvalidNumber(lexeme) => {
                write!(f, "invalid number: '{}'", lexeme)
            }
        }
    }
}
