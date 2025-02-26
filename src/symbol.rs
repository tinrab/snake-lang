use crate::{span::Span, token::Token};

/// A symbol representing a lexical token in the source code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub lexeme: String,
    pub token: Token,
    pub span: Span,
}
