pub mod error;

use std::{
    io::{self, Read},
    str,
};

use crate::{
    lexer::error::{LexerError, LexerErrorKind, LexerResult},
    span::Span,
    symbol::Symbol,
    token::Token,
};

/// The lexer.
///
/// It reads a stream of characters from a [`Read`] type and produces a stream of [`Symbol`]s.
pub struct Lexer<R> {
    reader: R,
    line: usize,
    column: usize,
    position: usize,
    span_start: Span,
    current_char: Option<char>,
    eof: bool,
}

const PUNCTUATION: &[char] = &[
    '{', '}', '(', ')', '[', ']', ';', ':', ',', '.', '+', '-', '*', '/', '%', '=', '!', '<', '>',
    '&', '|', '^', '?', ':',
];

impl<R: Read> Lexer<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            line: 0,
            column: 0,
            position: 0,
            span_start: Span::default(),
            current_char: None,
            eof: false,
        }
    }

    /// Scan a single symbol.
    pub fn scan(&mut self) -> LexerResult<Option<Symbol>> {
        // Initialize the current character.
        if !self.eof && self.current_char.is_none() {
            self.advance()?;
        }

        self.skip_whitespace()?;

        // If we've reached EOF, return None.
        let Some(ch) = self.current_char else {
            return Ok(None);
        };

        // Start a new span.
        // This is used for error reporting.
        self.start_span();

        let symbol = match ch {
            ch if ch.is_ascii_digit() => self.scan_number(),

            '"' => self.scan_string(),

            '=' => {
                self.advance()?;
                if self.current_char == Some('=') {
                    self.advance()?;
                    Ok(Symbol {
                        lexeme: "==".to_string(),
                        token: Token::LogicalEqual,
                        span: self.get_current_span(),
                    })
                } else {
                    Ok(Symbol {
                        lexeme: "=".to_string(),
                        token: Token::Equal,
                        span: self.get_current_span(),
                    })
                }
            }
            '&' => {
                self.advance()?;
                match self.current_char {
                    Some('&') => {
                        self.advance()?;
                        Ok(Symbol {
                            lexeme: "&&".to_string(),
                            token: Token::LogicalAnd,
                            span: self.get_current_span(),
                        })
                    }
                    None => Err(LexerError::new(
                        LexerErrorKind::UnexpectedEof,
                        self.get_current_eof_span(),
                    )),
                    Some(ch) => Err(LexerError::new(
                        LexerErrorKind::UnexpectedCharacter(ch),
                        self.get_current_span(),
                    )),
                }
            }
            '|' => {
                self.advance()?;
                match self.current_char {
                    Some('|') => {
                        self.advance()?;
                        Ok(Symbol {
                            lexeme: "||".to_string(),
                            token: Token::LogicalOr,
                            span: self.get_current_span(),
                        })
                    }
                    None => Err(LexerError::new(
                        LexerErrorKind::UnexpectedEof,
                        self.get_current_eof_span(),
                    )),
                    Some(ch) => Err(LexerError::new(
                        LexerErrorKind::UnexpectedCharacter(ch),
                        self.get_current_span(),
                    )),
                }
            }
            '!' => {
                self.advance()?;
                if self.current_char == Some('=') {
                    self.advance()?;
                    Ok(Symbol {
                        lexeme: "!=".to_string(),
                        token: Token::LogicalNotEqual,
                        span: self.get_current_span(),
                    })
                } else {
                    Ok(Symbol {
                        lexeme: "!".to_string(),
                        token: Token::Bang,
                        span: self.get_current_span(),
                    })
                }
            }
            '<' => {
                self.advance()?;
                if self.current_char == Some('=') {
                    self.advance()?;
                    Ok(Symbol {
                        lexeme: "<=".to_string(),
                        token: Token::LogicalLessOrEqual,
                        span: self.get_current_span(),
                    })
                } else {
                    Ok(Symbol {
                        lexeme: "<".to_string(),
                        token: Token::LogicalLess,
                        span: self.get_current_span(),
                    })
                }
            }
            '>' => {
                self.advance()?;
                if self.current_char == Some('=') {
                    self.advance()?;
                    Ok(Symbol {
                        lexeme: ">=".to_string(),
                        token: Token::LogicalGreaterOrEqual,
                        span: self.get_current_span(),
                    })
                } else {
                    Ok(Symbol {
                        lexeme: ">".to_string(),
                        token: Token::LogicalGreater,
                        span: self.get_current_span(),
                    })
                }
            }

            '/' => {
                self.advance()?;
                match self.current_char {
                    // Single line comment.
                    Some('/') => {
                        self.advance()?;
                        while self.current_char != Some('\n') && self.current_char.is_some() {
                            self.advance()?;
                        }
                        return self.scan();
                    }
                    // Multi line comment.
                    Some('*') => {
                        self.advance()?;
                        while self.current_char != Some('*') && self.current_char.is_some() {
                            self.advance()?;
                        }
                        if self.current_char == Some('*') {
                            self.advance()?;
                            if self.current_char == Some('/') {
                                self.advance()?;
                                return self.scan();
                            }
                        }
                        return Err(LexerError::new(
                            LexerErrorKind::UnexpectedCharacter('*'),
                            self.get_current_span(),
                        ));
                    }
                    _ => Ok(Symbol {
                        lexeme: ch.to_string(),
                        token: Token::Slash,
                        span: self.get_current_span(),
                    }),
                }
            }

            '+' => self.scan_single_char(Token::Plus, ch),
            '-' => self.scan_single_char(Token::Minus, ch),
            '*' => self.scan_single_char(Token::Asterisk, ch),
            '(' => self.scan_single_char(Token::OpenParenthesis, ch),
            ')' => self.scan_single_char(Token::CloseParenthesis, ch),
            '{' => self.scan_single_char(Token::OpenBrace, ch),
            '}' => self.scan_single_char(Token::CloseBrace, ch),
            '[' => self.scan_single_char(Token::OpenBracket, ch),
            ']' => self.scan_single_char(Token::CloseBracket, ch),
            ',' => self.scan_single_char(Token::Comma, ch),
            ';' => self.scan_single_char(Token::Semicolon, ch),

            // Else, fallback to identifier.
            // These could contain unicode characters.
            ch if is_valid_identifier_char(ch) => self.scan_identifier(),

            ch => Err(LexerError::new(
                LexerErrorKind::UnexpectedCharacter(ch),
                self.get_current_span(),
            )),
        }?;

        Ok(Some(symbol))
    }

    /// Scan a single character token and advance the lexer.
    fn scan_single_char(&mut self, token: Token, ch: char) -> LexerResult<Symbol> {
        self.advance()?;
        Ok(Symbol {
            lexeme: ch.to_string(),
            token,
            span: self.get_current_span(),
        })
    }

    /// Scan all symbols until EOF.
    pub fn scan_all(&mut self) -> LexerResult<Vec<Symbol>> {
        let mut symbols = Vec::new();
        while let Some(symbol) = self.scan()? {
            symbols.push(symbol);
        }
        Ok(symbols)
    }

    fn scan_number(&mut self) -> LexerResult<Symbol> {
        let mut lexeme = String::new();

        // Read integer part.
        while let Some(ch) = self.current_char {
            if ch.is_ascii_digit() {
                lexeme.push(ch);
                self.advance()?;
            } else {
                break;
            }
        }

        // Check for decimal part.
        if self.current_char == Some('.') {
            lexeme.push('.');
            self.advance()?;

            while let Some(ch) = self.current_char {
                if ch.is_ascii_digit() {
                    lexeme.push(ch);
                    self.advance()?;
                } else {
                    break;
                }
            }
        }

        // Check for exponent part.
        if let Some('e' | 'E') = self.current_char {
            lexeme.push(self.current_char.unwrap());
            self.advance()?;

            // Optional sign
            if let Some('+' | '-') = self.current_char {
                lexeme.push(self.current_char.unwrap());
                self.advance()?;
            }

            // Exponent digits.
            while let Some(ch) = self.current_char {
                if ch.is_ascii_digit() {
                    lexeme.push(ch);
                    self.advance()?;
                } else {
                    break;
                }
            }
        }

        Ok(Symbol {
            lexeme,
            token: Token::LiteralNumber,
            span: self.get_current_span(),
        })
    }

    /// Scan an identifier.
    fn scan_identifier(&mut self) -> LexerResult<Symbol> {
        let mut lexeme = String::new();

        while let Some(ch) = self.current_char {
            if !is_valid_identifier_char(ch) {
                break;
            }
            lexeme.push(ch);
            self.advance()?;
        }

        // Check if it's a keyword
        let token = match lexeme.as_str() {
            "true" | "false" => Token::LiteralBoolean,
            "null" => Token::KeywordNull,
            "let" => Token::KeywordLet,
            "fn" => Token::KeywordFn,
            "if" => Token::KeywordIf,
            "else" => Token::KeywordElse,
            "return" => Token::KeywordReturn,
            _ => Token::Identifier,
        };

        Ok(Symbol {
            lexeme,
            token,
            span: self.get_current_span(),
        })
    }

    /// Scan a string literal.
    fn scan_string(&mut self) -> LexerResult<Symbol> {
        let mut lexeme = String::new();

        if let Some(ch) = self.current_char {
            lexeme.push(ch);
        }

        // Skip the opening quote.
        self.advance()?;

        while let Some(ch) = self.current_char {
            if ch == '"' {
                if let Some(ch) = self.current_char {
                    lexeme.push(ch);
                }

                // Skip the closing quote.
                self.advance()?;

                break;
            } else if ch == '\\' {
                // Handle escape sequences
                self.advance()?;

                if let Some(esc_ch) = self.current_char {
                    let escaped = match esc_ch {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '\\' => '\\',
                        '"' => '"',
                        _ => esc_ch, // Invalid escape sequence
                    };

                    lexeme.push(escaped);
                }
            } else {
                lexeme.push(ch);
            }

            self.advance()?;
        }

        Ok(Symbol {
            lexeme,
            token: Token::LiteralString,
            span: self.get_current_span(),
        })
    }

    /// Advances the lexer to the next character.
    /// The next character is stored in `self.current_char`.
    fn advance(&mut self) -> LexerResult<()> {
        if self.eof {
            self.current_char = None;
            return Ok(());
        }

        let mut buf = [0; 4];
        let mut bytes_read;

        // Read the first byte to determine UTF-8 sequence length.
        match self.reader.read(&mut buf[0..1]) {
            Ok(0) => {
                // EOF reached
                self.eof = true;
                self.current_char = None;
                return Ok(());
            }
            Ok(1) => {
                bytes_read = 1;

                // Determine how many additional bytes we need based on the first byte.
                let additional_bytes = if (buf[0] & 0x80) == 0 {
                    // ASCII character (0xxxxxxx)
                    0
                } else if (buf[0] & 0xE0) == 0xC0 {
                    // 2-byte sequence (110xxxxx)
                    1
                } else if (buf[0] & 0xF0) == 0xE0 {
                    // 3-byte sequence (1110xxxx)
                    2
                } else if (buf[0] & 0xF8) == 0xF0 {
                    // 4-byte sequence (11110xxx)
                    3
                } else {
                    // Invalid UTF-8 first byte.
                    return Err(LexerError::new(
                        io::Error::new(io::ErrorKind::InvalidData, "invalid UTF-8 first byte")
                            .into(),
                        self.get_char_span(),
                    ));
                };

                // Read any additional bytes needed.
                if additional_bytes > 0 {
                    match self.reader.read(&mut buf[1..1 + additional_bytes]) {
                        Ok(n) if n == additional_bytes => {
                            bytes_read += n;
                        }
                        Ok(_) => {
                            return Err(LexerError::new(
                                io::Error::new(
                                    io::ErrorKind::UnexpectedEof,
                                    "unexpected EOF in the middle of a UTF-8 character",
                                )
                                .into(),
                                self.get_char_span(),
                            ));
                        }
                        Err(err) => {
                            return Err(LexerError::new(err.into(), self.get_char_span()));
                        }
                    }
                }
            }
            Err(err) => {
                return Err(LexerError::new(err.into(), self.get_char_span()));
            }
            _ => unreachable!(),
        }

        // Convert the bytes to a character.
        match str::from_utf8(&buf[0..bytes_read]) {
            Ok(s) => {
                let ch = s.chars().next().unwrap();
                if ch == '\n' {
                    self.line += 1;
                    self.column = 0;
                } else {
                    self.column += 1;
                }

                self.position += bytes_read;
                self.current_char = Some(ch);
                Ok(())
            }
            Err(err) => Err(LexerError::new(
                LexerErrorKind::Utf8(err),
                self.get_char_span(),
            )),
        }
    }

    /// Skips whitespace characters.
    fn skip_whitespace(&mut self) -> LexerResult<()> {
        while let Some(ch) = self.current_char {
            if !ch.is_whitespace() {
                break;
            }
            self.advance()?;
        }
        Ok(())
    }

    /// Updates span tracker to current location.
    fn start_span(&mut self) {
        self.span_start.line_start = self.line;
        if self.current_char.is_some() {
            self.span_start.column_start = self.column.saturating_sub(1);
        } else {
            self.span_start.column_start = self.column;
        }
    }

    fn get_current_span(&self) -> Span {
        Span::new(
            self.span_start.line_start,
            self.line,
            self.span_start.column_start,
            if self.eof {
                self.column
            } else {
                self.column.saturating_sub(1)
            },
        )
    }

    fn get_current_eof_span(&self) -> Span {
        Span::new(self.line, self.line, self.column, self.column)
    }

    fn get_char_span(&self) -> Span {
        Span::new(
            self.line,
            self.line,
            self.column,
            self.column.saturating_add(1),
        )
    }
}

fn is_valid_identifier_char(ch: char) -> bool {
    !ch.is_whitespace() && !PUNCTUATION.contains(&ch)
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::*;

    macro_rules! check_tokens {
        ($input:expr, [ $(($lexeme:expr, $token:ident)),* $(,)* ] $(,)*) => {{
            let cursor = Cursor::new($input);
            let mut lexer = Lexer::new(cursor);
            $(
                let result = lexer.scan().unwrap().unwrap();
                assert_eq!(result.lexeme, $lexeme);
                assert_eq!(result.token, Token::$token);
            )*
        }};
    }

    macro_rules! check_spans {
        ($input:expr, [ $($span:expr),* $(,)* ] $(,)*) => {{
            let cursor = Cursor::new($input);
            let mut lexer = Lexer::new(cursor);
            $(
                let result = lexer.scan().unwrap().unwrap();
                assert_eq!(result.span, $span);
            )*
        }}
    }

    #[test]
    fn it_works() {
        assert!(Lexer::new(Cursor::new("")).scan().unwrap().is_none());

        // Numbers
        check_tokens!(
            "42 3.14 1E6",
            [
                ("42", LiteralNumber),
                ("3.14", LiteralNumber),
                ("1E6", LiteralNumber),
            ],
        );

        // Strings
        check_tokens!(
            r#" "hello" "a\nb\nc" "#,
            [("\"hello\"", LiteralString), ("\"a\nb\nc\"", LiteralString)],
        );

        // Punctuation
        check_tokens!(
            "()[]{},;",
            [
                ("(", OpenParenthesis),
                (")", CloseParenthesis),
                ("[", OpenBracket),
                ("]", CloseBracket),
                ("{", OpenBrace),
                ("}", CloseBrace),
                (",", Comma),
                (";", Semicolon),
            ]
        );

        // Operators
        check_tokens!(
            "= && || ! == != < <= > >=",
            [
                ("=", Equal),
                ("&&", LogicalAnd),
                ("||", LogicalOr),
                ("!", Bang),
                ("==", LogicalEqual),
                ("!=", LogicalNotEqual),
                ("<", LogicalLess),
                ("<=", LogicalLessOrEqual),
                (">", LogicalGreater),
                (">=", LogicalGreaterOrEqual),
            ]
        );

        // Identifiers
        check_tokens!(
            "foo bar true false null if else return let fn",
            [
                ("foo", Identifier),
                ("bar", Identifier),
                ("true", LiteralBoolean),
                ("false", LiteralBoolean),
                ("null", KeywordNull),
                ("if", KeywordIf),
                ("else", KeywordElse),
                ("return", KeywordReturn),
                ("let", KeywordLet),
                ("fn", KeywordFn),
            ]
        );

        // Unicode
        check_tokens!("🐍", [("🐍", Identifier)]);

        // Edge cases
        check_tokens!(
            "(true)",
            [
                ("(", OpenParenthesis),
                ("true", LiteralBoolean),
                (")", CloseParenthesis),
            ]
        );
    }

    #[test]
    fn check_spans() {
        check_spans!("1", [Span::new(0, 0, 0, 1)]);
        check_spans!(" 1", [Span::new(0, 0, 1, 2)]);
        check_spans!("1337", [Span::new(0, 0, 0, 4)]);
        check_spans!("1 1", [Span::new(0, 0, 0, 1), Span::new(0, 0, 2, 3)]);
        check_spans!(
            " 42   3.14",
            [Span::new(0, 0, 1, 3), Span::new(0, 0, 6, 10)]
        );

        check_spans!(r#""abc""#, [Span::new(0, 0, 0, 5)]);
    }

    #[test]
    fn check_errors() {
        assert_eq!(
            Lexer::new(Cursor::new("&")).scan().unwrap_err(),
            LexerError::new(LexerErrorKind::UnexpectedEof, Span::new(0, 0, 1, 1))
        );
        assert_eq!(
            Lexer::new(Cursor::new("|")).scan().unwrap_err(),
            LexerError::new(LexerErrorKind::UnexpectedEof, Span::new(0, 0, 1, 1))
        );
    }

    #[test]
    fn comments() {
        check_tokens!(
            r#"
            // hi
            42
            /* Is this true? */
            true
            /*
                Multiline comment.
            */
            "test"
            // fin
        "#,
            [
                ("42", LiteralNumber),
                ("true", LiteralBoolean),
                ("\"test\"", LiteralString),
            ]
        );
    }
}
