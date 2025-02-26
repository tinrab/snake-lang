use std::fmt::{self, Display, Formatter};

/// Token kinds.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    Eof,
    LiteralNumber,
    LiteralString,
    LiteralBoolean,

    Plus,
    Minus,
    Asterisk,
    Slash,

    OpenParenthesis,
    CloseParenthesis,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,

    Comma,
    Semicolon,

    Equal,

    LogicalAnd,
    LogicalOr,
    LogicalNot,
    LogicalEqual,
    LogicalNotEqual,
    LogicalLess,
    LogicalLessOrEqual,
    LogicalGreater,
    LogicalGreaterOrEqual,

    Identifier,

    KeywordNull,
    KeywordIf,
    KeywordElse,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
