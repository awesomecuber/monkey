use std::fmt::Display;

use eyre::eyre;

use crate::token::Token;

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Let { name: Box<[u8]>, val: Expression },
    Return(Expression),
    Expression(Expression),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let { name, val } => {
                write!(f, "let {} = {};", std::str::from_utf8(name).unwrap(), val)
            }
            Statement::Return(expr) => write!(f, "return {expr};"),
            Statement::Expression(expr) => write!(f, "{expr};"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Identifier(Box<[u8]>),
    Integer(i64),
    Boolean(bool),
    Prefix(PrefixOperator, Box<Expression>),
    Infix(Box<Expression>, InfixOperator, Box<Expression>),
    If {
        condition: Box<Expression>,
        consequence: Vec<Statement>,
        alternative: Option<Vec<Statement>>,
    },
    Function {
        arguments: Vec<Expression>,
        body: Vec<Statement>,
    },
    Call(Box<Expression>, Vec<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{}", std::str::from_utf8(ident).unwrap()),
            Expression::Integer(int) => write!(f, "{int}"),
            Expression::Boolean(bool) => write!(f, "{bool}"),
            Expression::Prefix(op, exp) => write!(f, "({op}{exp})"),
            Expression::Infix(a, op, b) => write!(f, "({a} {op} {b})"),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => match alternative {
                Some(alternative) => write!(
                    f,
                    "if ({}) {{ {} }} else {{ {} }}",
                    condition,
                    print_multiple_statements(consequence),
                    print_multiple_statements(alternative),
                ),
                None => write!(
                    f,
                    "if ({}) {{ {} }}",
                    condition,
                    print_multiple_statements(consequence),
                ),
            },
            Expression::Function { arguments, body } => write!(
                f,
                "fn({}) {{ {} }}",
                print_multiple_expressions(arguments),
                print_multiple_statements(body),
            ),
            Expression::Call(name, args) => {
                write!(f, "{}({})", name, print_multiple_expressions(args))
            }
        }
    }
}

fn print_multiple_statements(statements: &[Statement]) -> String {
    statements
        .iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join(" ")
}

fn print_multiple_expressions(expressions: &[Expression]) -> String {
    expressions
        .iter()
        .map(|expr| expr.to_string())
        .collect::<Vec<String>>()
        .join(", ")
}

#[derive(Debug, PartialEq, Eq)]
pub enum PrefixOperator {
    Negate,
    Not,
}

impl Display for PrefixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixOperator::Negate => write!(f, "-"),
            PrefixOperator::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum InfixOperator {
    Plus,
    Minus,
    Times,
    DividedBy,
    GreaterThan,
    LessThan,
    Equals,
    NotEquals,
}

impl Display for InfixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixOperator::Plus => write!(f, "+"),
            InfixOperator::Minus => write!(f, "-"),
            InfixOperator::Times => write!(f, "*"),
            InfixOperator::DividedBy => write!(f, "/"),
            InfixOperator::GreaterThan => write!(f, ">"),
            InfixOperator::LessThan => write!(f, "<"),
            InfixOperator::Equals => write!(f, "=="),
            InfixOperator::NotEquals => write!(f, "!="),
        }
    }
}

impl<'a> TryFrom<Token> for InfixOperator {
    type Error = eyre::Report;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        use InfixOperator::*;
        Ok(match value {
            Token::Plus => Plus,
            Token::Minus => Minus,
            Token::Asterisk => Times,
            Token::Slash => DividedBy,
            Token::GT => GreaterThan,
            Token::LT => LessThan,
            Token::Eq => Equals,
            Token::NotEq => NotEquals,
            _ => return Err(eyre!("[{value}] is not an infix operator")),
        })
    }
}
