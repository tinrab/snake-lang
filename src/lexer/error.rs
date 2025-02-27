use std::{
    fmt::{self, Display, Formatter},
    io,
    str::Utf8Error,
};

use crate::span::Span;

#[derive(Debug, PartialEq)]
pub struct LexerError {
    pub kind: LexerErrorKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum LexerErrorKind {
    Io(io::Error),
    Utf8(Utf8Error),
    UnexpectedEof,
    UnexpectedCharacter(char),
}

pub type LexerResult<T> = Result<T, LexerError>;

impl LexerError {
    pub fn new(kind: LexerErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.span, self.kind)
    }
}

macro_rules! error_kind_variants {
    ($(($variant:ident, $type:ty)),* $(,)?) => {
        $(
            impl From<$type> for LexerErrorKind {
                fn from(error: $type) -> Self {
                    LexerErrorKind::$variant(error)
                }
            }
        )*
    };
}

error_kind_variants![(Io, io::Error), (Utf8, Utf8Error)];

impl Display for LexerErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LexerErrorKind::Io(err) => write!(f, "IO error: {}", err),
            LexerErrorKind::Utf8(err) => write!(f, "utf-8 error: {}", err),
            LexerErrorKind::UnexpectedEof => write!(f, "unexpected EOF"),
            LexerErrorKind::UnexpectedCharacter(ch) => write!(f, "unexpected character `{}`", ch),
        }
    }
}

impl PartialEq for LexerErrorKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LexerErrorKind::Io(a), LexerErrorKind::Io(b)) => a.kind() == b.kind(),
            (LexerErrorKind::Utf8(a), LexerErrorKind::Utf8(b)) => a == b,
            (LexerErrorKind::UnexpectedEof, LexerErrorKind::UnexpectedEof) => true,
            (LexerErrorKind::UnexpectedCharacter(a), LexerErrorKind::UnexpectedCharacter(b)) => {
                a == b
            }
            _ => false,
        }
    }
}
