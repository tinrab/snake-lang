use std::fmt::{self, Display, Formatter};

use crate::{lexer::error::LexerError, parser::error::ParserError};

#[derive(Debug, PartialEq)]
pub enum LangError {
    Lexer(LexerError),
    Parser(ParserError),
}

pub type LangResult<T> = Result<T, LangError>;

impl Display for LangError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LangError::Lexer(err) => write!(f, "lexer error: {}", err),
            LangError::Parser(err) => write!(f, "parser error: {}", err),
        }
    }
}

macro_rules! error_kind_variants {
    ($(($variant:ident, $type:ty)),* $(,)?) => {
        $(
            impl From<$type> for LangError {
                fn from(error: $type) -> Self {
                    LangError::$variant(error)
                }
            }
        )*
    };
}

error_kind_variants![(Lexer, LexerError), (Parser, ParserError)];
