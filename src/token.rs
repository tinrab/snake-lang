use std::fmt::{self, Display, Formatter};

/// Token kinds.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    Eof, // End of File

    LiteralNumber,  // e.g., 42, 3.14
    LiteralString,  // e.g., "hello", "world"
    LiteralBoolean, // e.g., true, false

    Plus,     // +
    Minus,    // -
    Asterisk, // *
    Slash,    // /

    OpenParenthesis,  // (
    CloseParenthesis, // )
    OpenBracket,      // [
    CloseBracket,     // ]
    OpenBrace,        // {
    CloseBrace,       // }

    Comma,     // ,
    Semicolon, // ;

    Equal, // =
    Bang,  // !

    LogicalAnd,            // &&
    LogicalOr,             // ||
    LogicalEqual,          // ==
    LogicalNotEqual,       // !=
    LogicalLess,           // <
    LogicalLessOrEqual,    // <=
    LogicalGreater,        // >
    LogicalGreaterOrEqual, // >=

    Identifier, // e.g., x, y, request_count, 🐍

    KeywordNull,   // null
    KeywordLet,    // let
    KeywordFn,     // fn
    KeywordIf,     // if
    KeywordElse,   // else
    KeywordReturn, // return
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::Eof => write!(f, "EOF"),
            Token::LiteralNumber => write!(f, "number"),
            Token::LiteralString => write!(f, "string"),
            Token::LiteralBoolean => write!(f, "boolean"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::OpenParenthesis => write!(f, "("),
            Token::CloseParenthesis => write!(f, ")"),
            Token::OpenBracket => write!(f, "["),
            Token::CloseBracket => write!(f, "]"),
            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::Equal => write!(f, "="),
            Token::Bang => write!(f, "!"),
            Token::LogicalAnd => write!(f, "&&"),
            Token::LogicalOr => write!(f, "||"),
            Token::LogicalEqual => write!(f, "=="),
            Token::LogicalNotEqual => write!(f, "!="),
            Token::LogicalLess => write!(f, "<"),
            Token::LogicalLessOrEqual => write!(f, "<="),
            Token::LogicalGreater => write!(f, ">"),
            Token::LogicalGreaterOrEqual => write!(f, ">="),
            Token::Identifier => write!(f, "identifier"),
            Token::KeywordNull => write!(f, "null"),
            Token::KeywordLet => write!(f, "let"),
            Token::KeywordFn => write!(f, "fn"),
            Token::KeywordIf => write!(f, "if"),
            Token::KeywordElse => write!(f, "else"),
            Token::KeywordReturn => write!(f, "return"),
        }
    }
}
