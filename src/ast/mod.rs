use std::fmt::{self, Display, Formatter};

use crate::{symbol::Symbol, token::Token};

#[derive(Debug, Clone, PartialEq)]
pub struct Ast {
    pub statements: Vec<StatementAst>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementAst {
    Block(Vec<StatementAst>),
    Declaration(DeclarationAst),
    Return(Option<ExpressionAst>),
    If(IfStatementAst),
    Expression(ExpressionAst),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionAst {
    Literal(LiteralAst),
    BinaryExpression(BinaryExpressionAst),
    UnaryExpression(UnaryExpressionAst),
    Identifier(Symbol),
    Call(CallExpressionAst),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclarationAst {
    Function(FunctionDeclarationAst),
    Variable(VariableDeclarationAst),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclarationAst {
    pub name: Symbol,
    pub parameters: Vec<Symbol>,
    pub body: Box<StatementAst>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclarationAst {
    pub name: Symbol,
    pub initializer: Box<ExpressionAst>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatementAst {
    pub condition: Box<ExpressionAst>,
    pub then_branch: Box<StatementAst>,
    pub else_branch: Option<Box<StatementAst>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpressionAst {
    pub callee: Box<ExpressionAst>,
    pub arguments: Vec<ExpressionAst>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpressionAst {
    pub operator: Token,
    pub left: Box<ExpressionAst>,
    pub right: Box<ExpressionAst>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpressionAst {
    pub operator: Token,
    pub operand: Box<ExpressionAst>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralAst {
    Number(f64),
    String(String),
    Boolean(bool),
}

impl Display for Ast {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?;
        }
        Ok(())
    }
}

impl Display for ExpressionAst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ExpressionAst::Literal(literal) => write!(f, "{}", literal),
            ExpressionAst::BinaryExpression(binary) => write!(f, "{}", binary),
            ExpressionAst::UnaryExpression(unary) => write!(f, "{}", unary),
            ExpressionAst::Identifier(ident) => write!(f, "{}", ident.lexeme),
            ExpressionAst::Call(call) => write!(f, "{}", call),
        }
    }
}

impl Display for StatementAst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            StatementAst::Block(stmts) => {
                writeln!(f, "{{")?;
                for stmt in stmts {
                    write!(f, "    {}", stmt)?;
                }
                write!(f, "\n}}\n")
            }
            StatementAst::Declaration(decl) => write!(f, "{}", decl),
            StatementAst::Return(expr) => {
                if let Some(expr) = expr {
                    write!(f, "return {};", expr)
                } else {
                    write!(f, "return")
                }
            }
            StatementAst::If(if_stmt) => write!(f, "{}", if_stmt),
            StatementAst::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

impl Display for DeclarationAst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DeclarationAst::Function(func) => write!(f, "{}", func),
            DeclarationAst::Variable(var) => write!(f, "{}", var),
        }
    }
}

impl Display for FunctionDeclarationAst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let params =
            self.parameters
                .iter()
                .enumerate()
                .fold(String::new(), |mut acc, (i, param)| {
                    if i > 0 {
                        acc.push_str(", ");
                    }
                    acc.push_str(&param.lexeme);
                    acc
                });
        write!(f, "fn {}({}) {}", &self.name.lexeme, params, self.body)
    }
}

impl Display for VariableDeclarationAst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", &self.name.lexeme, self.initializer)
    }
}

impl Display for IfStatementAst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "if ({}) {}", self.condition, self.then_branch)
    }
}

impl Display for CallExpressionAst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let args = self
            .arguments
            .iter()
            .map(|arg| arg.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{}({})", self.callee, args)
    }
}

impl Display for BinaryExpressionAst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

impl Display for UnaryExpressionAst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.operator, self.operand)
    }
}

impl Display for LiteralAst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LiteralAst::Number(value) => write!(f, "{}", value),
            LiteralAst::String(value) => write!(f, "\"{}\"", value),
            LiteralAst::Boolean(value) => write!(f, "{}", value),
        }
    }
}

impl<I, T> From<I> for Ast
where
    I: IntoIterator<Item = T>,
    T: Into<StatementAst>,
{
    fn from(iter: I) -> Self {
        Ast {
            statements: iter.into_iter().map(Into::into).collect(),
        }
    }
}

impl From<IfStatementAst> for StatementAst {
    fn from(stmt: IfStatementAst) -> Self {
        StatementAst::If(stmt)
    }
}

impl From<IfStatementAst> for Ast {
    fn from(stmt: IfStatementAst) -> Self {
        Ast {
            statements: vec![StatementAst::If(stmt)],
        }
    }
}

impl From<DeclarationAst> for StatementAst {
    fn from(decl: DeclarationAst) -> Self {
        StatementAst::Declaration(decl)
    }
}

macro_rules! convert_declaration {
    ($kind:ident, $type:ty) => {
        impl From<$type> for DeclarationAst {
            fn from(decl: $type) -> Self {
                DeclarationAst::$kind(decl)
            }
        }

        impl From<$type> for StatementAst {
            fn from(decl: $type) -> Self {
                StatementAst::Declaration(decl.into())
            }
        }

        impl From<$type> for Ast {
            fn from(decl: $type) -> Self {
                Ast {
                    statements: vec![StatementAst::Declaration(decl.into())],
                }
            }
        }
    };
}

convert_declaration!(Function, FunctionDeclarationAst);
convert_declaration!(Variable, VariableDeclarationAst);

impl From<ExpressionAst> for StatementAst {
    fn from(expr: ExpressionAst) -> Self {
        StatementAst::Expression(expr)
    }
}

macro_rules! convert_expression {
    ($kind:ident, $type:ty) => {
        impl From<$type> for ExpressionAst {
            fn from(expr: $type) -> Self {
                ExpressionAst::$kind(expr)
            }
        }

        impl From<$type> for StatementAst {
            fn from(expr: $type) -> Self {
                StatementAst::Expression(expr.into())
            }
        }

        impl From<$type> for Ast {
            fn from(expr: $type) -> Self {
                Ast {
                    statements: vec![StatementAst::Expression(expr.into())],
                }
            }
        }
    };
}

convert_expression!(Literal, LiteralAst);
convert_expression!(BinaryExpression, BinaryExpressionAst);
convert_expression!(UnaryExpression, UnaryExpressionAst);
convert_expression!(Identifier, Symbol);
convert_expression!(Call, CallExpressionAst);

macro_rules! convert_literal {
    ($kind:ident, $type:ty) => {
        impl From<$type> for LiteralAst {
            fn from(value: $type) -> Self {
                LiteralAst::$kind(value)
            }
        }

        impl From<$type> for ExpressionAst {
            fn from(value: $type) -> Self {
                ExpressionAst::Literal(value.into())
            }
        }

        impl From<$type> for StatementAst {
            fn from(value: $type) -> Self {
                StatementAst::Expression(value.into())
            }
        }
    };
}

convert_literal!(Number, f64);
convert_literal!(String, String);
convert_literal!(Boolean, bool);
