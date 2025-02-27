pub mod error;

use std::io::Read;

use crate::{
    ast::{
        Ast, BinaryExpressionAst, CallExpressionAst, ExpressionAst, FunctionDeclarationAst,
        IfStatementAst, StatementAst, UnaryExpressionAst, VariableDeclarationAst,
    },
    lexer::Lexer,
    parser::error::{ParserError, ParserErrorKind, ParserResult},
    span::Span,
    symbol::Symbol,
    token::Token,
};

/// The parser.
///
/// This is a recursive descent parser that uses Pratt parsing [1].
///
/// [1]: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
pub struct Parser<R> {
    lexer: Lexer<R>,
    span_start: Span,
    current: Option<Symbol>,
    peek: Option<Symbol>,
}

impl<R: Read> Parser<R> {
    pub fn new(lexer: Lexer<R>) -> Self {
        Self {
            lexer,
            span_start: Span::default(),
            current: None,
            peek: None,
        }
    }

    /// Parse the input into an abstract syntax tree.
    pub fn parse(mut self) -> ParserResult<Ast> {
        self.advance()?;

        let mut statements = Vec::new();
        while self.current.is_some() {
            statements.push(self.parse_statement()?);
        }

        Ok(Ast { statements })
    }

    fn parse_statement(&mut self) -> ParserResult<StatementAst> {
        if let Some(ref symbol) = self.current {
            match symbol.token {
                Token::KeywordLet => self
                    .parse_variable_declaration()
                    .map(|decl| Ok(decl.into()))?,
                Token::KeywordFn => self
                    .parse_function_declaration()
                    .map(|decl| Ok(decl.into()))?,
                Token::KeywordIf => self.parse_if_statement().map(|stmt| Ok(stmt.into()))?,
                Token::KeywordReturn => self
                    .parse_return_statement()
                    .map(|stmt| Ok(StatementAst::Return(stmt)))?,
                Token::OpenBrace => self.parse_block_statement(),
                _ => self.parse_expression_statement(),
            }
        } else {
            self.parse_expression_statement()
        }
    }

    fn parse_block_statement(&mut self) -> ParserResult<StatementAst> {
        let mut statements = Vec::new();

        self.expect(Token::OpenBrace)?;

        while let Some(ref symbol) = self.current {
            if symbol.token == Token::CloseBrace {
                break;
            }
            statements.push(self.parse_statement()?);
        }

        self.expect(Token::CloseBrace)?;

        Ok(StatementAst::Block(statements))
    }

    fn parse_expression_statement(&mut self) -> ParserResult<StatementAst> {
        let expr = self.parse_expression(0)?;
        // TODO: remove
        self.accept(Token::Semicolon);
        Ok(expr.into())
    }

    fn parse_if_statement(&mut self) -> ParserResult<IfStatementAst> {
        //  skip `if`
        self.advance()?;

        // condition expression
        let condition = self.parse_expression(0)?;

        // then branch
        let then_branch = self.parse_statement()?;

        // optional else branch
        if self
            .current
            .as_ref()
            .map(|symbol| symbol.token == Token::KeywordElse)
            .unwrap_or(false)
        {
            self.advance()?;
            let else_branch = self.parse_statement()?;
            return Ok(IfStatementAst {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Some(Box::new(else_branch)),
            });
        }

        Ok(IfStatementAst {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: None,
        })
    }

    fn parse_return_statement(&mut self) -> ParserResult<Option<ExpressionAst>> {
        // skip `return`
        self.advance()?;

        // optional return value
        let expr = if let Some(ref sym) = self.current {
            if sym.token != Token::Semicolon {
                Some(self.parse_expression(0)?)
            } else {
                None
            }
        } else {
            None
        };

        // statement terminator
        self.expect(Token::Semicolon)?;

        Ok(expr)
    }

    fn parse_variable_declaration(&mut self) -> ParserResult<VariableDeclarationAst> {
        // skip `let`
        self.advance()?;

        // variable name
        let name = self.expect(Token::Identifier)?;
        // =
        self.expect(Token::Equal)?;
        // initializer
        let initializer = self.parse_expression(0)?;
        // statement terminator
        self.expect(Token::Semicolon)?;

        Ok(VariableDeclarationAst {
            name,
            initializer: Box::new(initializer),
        })
    }

    fn parse_function_declaration(&mut self) -> ParserResult<FunctionDeclarationAst> {
        // skip `fn`
        self.advance()?;

        // function name
        let name = self.expect(Token::Identifier)?;

        // parameters
        let mut parameters = Vec::new();
        self.expect(Token::OpenParenthesis)?;
        if let Some(ref current) = self.current {
            if current.token != Token::CloseParenthesis {
                loop {
                    let param = self.expect(Token::Identifier)?;
                    parameters.push(param);
                    if let Some(ref symbol) = self.current {
                        if symbol.token == Token::Comma {
                            self.advance()?;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
            }
        }
        self.expect(Token::CloseParenthesis)?;

        // function body
        let body = self.parse_block_statement()?;

        Ok(FunctionDeclarationAst {
            name,
            parameters,
            body: Box::new(body),
        })
    }

    fn parse_expression(&mut self, min_precedence: u8) -> ParserResult<ExpressionAst> {
        // parse the prefix expression
        let mut left = self.parse_prefix()?;

        loop {
            // handle postfix function calls
            if let Some(ref symbol) = self.current {
                if symbol.token == Token::OpenParenthesis {
                    // skip `(`
                    self.advance()?;
                    let mut arguments = Vec::new();
                    if self
                        .current
                        .as_ref()
                        .map(|current| current.token != Token::CloseParenthesis)
                        .unwrap_or(false)
                    {
                        loop {
                            let arg = self.parse_expression(0)?;
                            arguments.push(arg);
                            if self
                                .current
                                .as_ref()
                                .map(|arg_separator| arg_separator.token == Token::Comma)
                                .unwrap_or(false)
                            {
                                // skip `,`
                                self.advance()?;
                            } else {
                                break;
                            }
                        }
                    }
                    // skip `)`
                    self.expect(Token::CloseParenthesis)?;
                    left = CallExpressionAst {
                        callee: Box::new(left),
                        arguments,
                    }
                    .into();
                    // Allow chained calls.
                    // For example: `make_adder(5)(3)`.
                    continue;
                }
            }

            // handle infix operators
            if let Some(ref symbol) = self.current {
                if let Some((left_bp, right_bp)) = Self::infix_binding_power(symbol.token) {
                    if left_bp < min_precedence {
                        break;
                    }
                    let operator = symbol.token;
                    self.advance()?;
                    let right = self.parse_expression(right_bp)?;
                    left = BinaryExpressionAst {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                    .into();
                    continue;
                }
            }

            break;
        }

        Ok(left)
    }

    fn parse_prefix(&mut self) -> ParserResult<ExpressionAst> {
        // consume the current operator
        let symbol = self.current.take().ok_or_else(|| {
            ParserError::new(ParserErrorKind::UnexpectedEof, self.span_start.clone())
        })?;
        self.advance()?;

        match symbol.token {
            Token::LiteralNumber => {
                let value = symbol.lexeme.parse::<f64>().map_err(|_| {
                    ParserError::new(
                        ParserErrorKind::InvalidNumber(symbol.lexeme),
                        symbol.span.clone(),
                    )
                })?;
                Ok(ExpressionAst::Literal(value.into()))
            }
            Token::LiteralString => Ok(ExpressionAst::Literal(symbol.lexeme.into())),
            Token::LiteralBoolean => Ok(ExpressionAst::Literal((symbol.lexeme == "true").into())),
            Token::Identifier => Ok(ExpressionAst::Identifier(symbol)),
            // Handle prefix operators using their binding power.
            operator @ Token::Plus | operator @ Token::Minus | operator @ Token::Bang => {
                let bp = Self::prefix_binding_power(operator).unwrap();
                let right = self.parse_expression(bp)?;
                Ok(UnaryExpressionAst {
                    operator,
                    operand: Box::new(right),
                }
                .into())
            }
            Token::OpenParenthesis => {
                let expr = self.parse_expression(0)?;
                self.expect(Token::CloseParenthesis)?;
                Ok(expr)
            }
            _ => Err(ParserError::new(
                ParserErrorKind::UnexpectedToken(symbol.token),
                symbol.span,
            )),
        }
    }

    /// Determines the prefix binding power of a given token.
    ///
    /// The prefix binding power is used to determine the precedence of prefix operators
    /// like unary `+`, `-`, and `!`. The returned `u8` value represents the binding power,
    /// where higher values indicate higher precedence.
    ///
    /// If the given token is not a valid prefix operator, `None` is returned.
    fn prefix_binding_power(token: Token) -> Option<u8> {
        match token {
            Token::Plus | Token::Minus | Token::Bang => Some(9),
            _ => None,
        }
    }

    // fn postfix_binding_power(token: Token) -> Option<(u8, ())> {
    //     let res = match op {
    //         '!' => (11, ()),
    //         '[' => (11, ()),
    //         _ => return None,
    //     };
    //     Some(res)
    // }

    /// Determines the infix binding power of a given token.
    ///
    /// The infix binding power is used to determine the precedence of infix operators
    /// like `+`, `-`, `*`, and `/`. The returned `(u8, u8)` tuple represents the left and
    /// right binding power, where higher values indicate higher precedence.
    ///
    /// If the given token is not a valid infix operator, `None` is returned.
    fn infix_binding_power(token: Token) -> Option<(u8, u8)> {
        match token {
            Token::Plus | Token::Minus => Some((5, 6)),
            Token::Asterisk | Token::Slash => Some((7, 8)),
            _ => None,
        }
    }

    /// Expects the current token to match the given `expected` token.
    fn expect(&mut self, expected: Token) -> ParserResult<Symbol> {
        match self.current.take() {
            Some(symbol) if symbol.token == expected => {
                self.advance()?;
                Ok(symbol)
            }
            Some(symbol) => Err(ParserError::new(
                ParserErrorKind::UnexpectedToken(symbol.token),
                symbol.span,
            )),
            None => Err(ParserError::new(
                ParserErrorKind::UnexpectedEof,
                self.get_current_span(),
            )),
        }
    }

    /// Accepts the current token if it matches the given token.
    fn accept(&mut self, token: Token) {
        if matches!(self.current, Some(ref symbol) if symbol.token == token) {
            self.advance().unwrap();
        }
    }

    /// Advances the parser to the next token.
    fn advance(&mut self) -> ParserResult<()> {
        self.current = self.peek.take();
        if let Some(next) = self.lexer.scan()? {
            if self.current.is_none() {
                self.current = Some(next);
                self.peek = self.lexer.scan()?;
            } else {
                self.peek = Some(next);
            }
        } else {
            self.peek = None;
        }
        Ok(())
    }

    fn get_current_span(&self) -> Span {
        self.span_start.clone()
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use std::io::Cursor;

    use super::*;
    use crate::ast::LiteralAst;

    macro_rules! assert_ast {
        ($input:expr, $expected:expr) => {
            let parser = Parser::new(Lexer::new(Cursor::new($input)));
            let ast = parser.parse().unwrap();
            assert_eq!(ast, Ast::from($expected));
        };
    }

    #[test]
    fn expressions() {
        assert_ast!("42", LiteralAst::Number(42.0f64));
        assert_ast!("\"hello\"", LiteralAst::String("\"hello\"".into()));
        assert_ast!("true", LiteralAst::Boolean(true));

        assert_ast!(
            "(5 + 3) * 2",
            BinaryExpressionAst {
                operator: Token::Asterisk,
                left: Box::new(
                    BinaryExpressionAst {
                        operator: Token::Plus,
                        left: Box::new(5.0f64.into()),
                        right: Box::new(3.0f64.into()),
                    }
                    .into()
                ),
                right: Box::new(2.0f64.into()),
            }
        );
    }

    #[test]
    fn if_statements() {
        assert_ast!(
            r#"if (true) {}"#,
            IfStatementAst {
                condition: Box::new(true.into()),
                then_branch: Box::new(StatementAst::Block(vec![])),
                else_branch: None,
            }
        );
        assert_ast!(
            r#"if true {} else {}"#,
            IfStatementAst {
                condition: Box::new(true.into()),
                then_branch: Box::new(StatementAst::Block(vec![])),
                else_branch: Some(Box::new(StatementAst::Block(vec![]))),
            }
        );

        assert_ast!(
            r#"if (false) { return 42; }"#,
            IfStatementAst {
                condition: Box::new(false.into()),
                then_branch: Box::new(StatementAst::Block(vec![StatementAst::Return(Some(
                    42.0f64.into()
                ))])),
                else_branch: None,
            }
        );
    }

    #[test]
    fn variable_declarations() {
        assert_ast!(
            r#"let x = 5; let y = 3;"#,
            vec![
                VariableDeclarationAst {
                    name: Symbol::new_identifier("x".into(), Span::new(0, 0, 4, 5)),
                    initializer: Box::new(5.0f64.into()),
                },
                VariableDeclarationAst {
                    name: Symbol::new_identifier("y".into(), Span::new(0, 0, 15, 16)),
                    initializer: Box::new(3.0f64.into()),
                },
            ]
        );
    }

    #[test]
    fn function_declarations() {
        assert_ast!(
            r#"fn print(msg) {}"#,
            vec![FunctionDeclarationAst {
                name: Symbol::new_identifier("print".into(), Span::new(0, 0, 3, 8)),
                parameters: vec![Symbol::new_identifier("msg".into(), Span::new(0, 0, 9, 12))],
                body: Box::new(StatementAst::Block(vec![])),
            }]
        );

        assert_ast!(
            r#"fn add(a, b) {return a + b;}"#,
            vec![FunctionDeclarationAst {
                name: Symbol::new_identifier("add".into(), Span::new(0, 0, 3, 6)),
                parameters: vec![
                    Symbol::new_identifier("a".into(), Span::new(0, 0, 7, 8)),
                    Symbol::new_identifier("b".into(), Span::new(0, 0, 10, 11)),
                ],
                body: Box::new(StatementAst::Block(vec![StatementAst::Return(Some(
                    BinaryExpressionAst {
                        operator: Token::Plus,
                        left: Box::new(
                            Symbol::new_identifier("a".into(), Span::new(0, 0, 21, 22)).into()
                        ),
                        right: Box::new(
                            Symbol::new_identifier("b".into(), Span::new(0, 0, 25, 26)).into()
                        ),
                    }
                    .into()
                ))]))
            }]
        );
    }

    #[test]
    fn function_calls() {
        assert_ast!(
            r#"print(42)"#,
            CallExpressionAst {
                callee: Box::new(
                    Symbol::new_identifier("print".into(), Span::new(0, 0, 0, 5)).into()
                ),
                arguments: vec![42.0f64.into()],
            }
        );

        assert_ast!(
            r#"add(3, 5) * 2"#,
            BinaryExpressionAst {
                operator: Token::Asterisk,
                left: Box::new(
                    CallExpressionAst {
                        callee: Box::new(
                            Symbol::new_identifier("add".into(), Span::new(0, 0, 0, 3)).into()
                        ),
                        arguments: vec![3.0f64.into(), 5.0f64.into()],
                    }
                    .into()
                ),
                right: Box::new(2.0f64.into()),
            }
        );
    }
}
