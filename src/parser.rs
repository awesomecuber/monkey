use std::iter::Peekable;
use std::str;

use crate::{
    ast::{Expression, InfixOperator, PrefixOperator, Statement},
    lexer::Lexer,
    token::{Token, TokenType},
};

use color_eyre::Result;
use eyre::{eyre, Context};

pub fn get_ast(l: Lexer) -> (Vec<Statement>, Vec<color_eyre::Report>) {
    let mut parser = Parser { l: l.peekable() };
    let mut statements = vec![];
    let mut errors = vec![];
    while let Some(&first_token) = parser.l.peek() {
        match parser.parse_statement(first_token) {
            Ok(s) => statements.push(s),
            Err(e) => {
                errors.push(e);
                parser.skip_past_semicolon();
            }
        }
    }
    (statements, errors)
}

struct Parser<'a> {
    l: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    fn parse_statement(&mut self, first: Token<'a>) -> Result<Statement> {
        match first.token_type {
            TokenType::Let => self
                .parse_let_statement()
                .context("Failed to parse let statement"),
            TokenType::Return => self
                .parse_return_statement()
                .context("Failed to parse return statement"),
            _ => self
                .parse_expression_statement()
                .context("Failed to parse expression statement"),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        self.expect(TokenType::Let)?;
        let name = self.expect(TokenType::Ident)?.into();
        self.expect(TokenType::Assign)?;
        let val = self.parse_expr(Precedence::Lowest)?;
        let _ = self.expect(TokenType::Semicolon);
        Ok(Statement::Let { name, val })
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        self.expect(TokenType::Return)?;
        let val = self.parse_expr(Precedence::Lowest)?;
        let _ = self.expect(TokenType::Semicolon);
        Ok(Statement::Return(val))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let val = self.parse_expr(Precedence::Lowest)?;
        let _ = self.expect(TokenType::Semicolon);
        Ok(Statement::Expression(val))
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Result<Expression> {
        let mut expr = match self.l.peek() {
            Some(&t) => match t.token_type {
                TokenType::Ident => self.parse_identifier(),
                TokenType::Int => self.parse_integer(),
                TokenType::True | TokenType::False => self.parse_boolean(),
                TokenType::LParen => self.parse_grouped_expression(),
                TokenType::If => self.parse_if_expression(),
                TokenType::Function => self.parse_function_expression(),
                TokenType::Minus | TokenType::Bang => self.parse_prefix_expr(),
                _ => Err(eyre!("No prefix parse function for [{t}] found")),
            },
            None => Err(eyre!("Wanted expression, found EOF")),
        }?;

        loop {
            let Some(&infix_token) = self.l.peek() else {
                break;
            };

            if infix_token.token_type == TokenType::LParen {
                expr = self.parse_call_expression(expr)?;
                continue;
            }

            let Ok(op) = infix_token.try_into() else {
                break;
            };
            if Precedence::from_infix_operator(&op) <= precedence {
                break;
            }
            self.l.next();
            expr = self.parse_infix_expr(expr, op)?;
        }
        Ok(expr)
    }

    fn parse_identifier(&mut self) -> Result<Expression> {
        let ident = self.expect(TokenType::Ident)?;
        Ok(Expression::Identifier(ident.into()))
    }

    fn parse_integer(&mut self) -> Result<Expression> {
        let num = self.expect(TokenType::Int)?;
        Ok(Expression::Integer(
            str::from_utf8(num).unwrap().parse().unwrap(),
        ))
    }

    fn parse_boolean(&mut self) -> Result<Expression> {
        match self
            .l
            .next()
            .ok_or(eyre!("Wanted boolean, found EOF"))?
            .token_type
        {
            TokenType::True => Ok(Expression::Boolean(true)),
            TokenType::False => Ok(Expression::Boolean(false)),
            t => return Err(eyre!("Wanted boolean, found {t:?}")),
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.expect(TokenType::LParen)?;
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.expect(TokenType::RParen)?;
        Ok(expr)
    }

    fn parse_if_expression(&mut self) -> Result<Expression> {
        self.expect(TokenType::If)?;
        self.expect(TokenType::LParen)?;
        let condition = self.parse_expr(Precedence::Lowest)?;
        self.expect(TokenType::RParen)?;

        let consequence = self.parse_blocked_statements()?;
        let alternative = match self.expect(TokenType::Else) {
            Ok(_) => Some(self.parse_blocked_statements()?),
            Err(_) => None,
        };
        Ok(Expression::If {
            condition: condition.into(),
            consequence,
            alternative,
        })
    }

    fn parse_function_expression(&mut self) -> Result<Expression> {
        self.expect(TokenType::Function)?;
        let arguments = self.parse_function_arguments()?;
        let body = self.parse_blocked_statements()?;
        Ok(Expression::Function { arguments, body })
    }

    fn parse_blocked_statements(&mut self) -> Result<Vec<Statement>> {
        self.expect(TokenType::LBrace)?;

        let mut statements = vec![];
        loop {
            let token = *self
                .l
                .peek()
                .ok_or(eyre!("Wanted statement or }}, found EOF"))?;
            if token.token_type == TokenType::RBrace {
                break;
            }
            let statement = self.parse_statement(token)?;
            statements.push(statement);
        }
        self.expect(TokenType::RBrace)?;
        Ok(statements)
    }

    fn parse_function_arguments(&mut self) -> Result<Vec<Expression>> {
        self.expect(TokenType::LParen)?;
        if self.expect(TokenType::RParen).is_ok() {
            return Ok(vec![]);
        }

        let mut arguments = vec![];
        let ident = self.parse_identifier()?;
        arguments.push(ident);
        while self.expect(TokenType::Comma).is_ok() {
            let ident = self.parse_identifier()?;
            arguments.push(ident);
        }
        self.expect(TokenType::RParen)?;
        Ok(arguments)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression> {
        let arguments = self.parse_call_arguments()?;
        Ok(Expression::Call(function.into(), arguments))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>> {
        self.expect(TokenType::LParen)?;
        if self.expect(TokenType::RParen).is_ok() {
            return Ok(vec![]);
        }

        let mut arguments = vec![];
        let expr = self.parse_expr(Precedence::Lowest)?;
        arguments.push(expr);
        while self.expect(TokenType::Comma).is_ok() {
            let expr = self.parse_expr(Precedence::Lowest)?;
            arguments.push(expr);
        }
        self.expect(TokenType::RParen)?;
        Ok(arguments)
    }

    fn parse_prefix_expr(&mut self) -> Result<Expression> {
        let op = match self.l.next().unwrap().token_type {
            TokenType::Minus => PrefixOperator::Negate,
            TokenType::Bang => PrefixOperator::Not,
            _ => unreachable!(),
        };
        let expr = self.parse_expr(Precedence::Prefix)?;
        Ok(Expression::Prefix(op, expr.into()))
    }

    fn parse_infix_expr(&mut self, left: Expression, op: InfixOperator) -> Result<Expression> {
        let precedence = Precedence::from_infix_operator(&op);
        let right = self.parse_expr(precedence)?;
        Ok(Expression::Infix(left.into(), op, right.into()))
    }

    fn skip_past_semicolon(&mut self) {
        for t in self.l.by_ref() {
            if t.token_type == TokenType::Semicolon {
                return;
            }
        }
    }

    fn expect(&mut self, want: TokenType) -> Result<&'a [u8]> {
        match self.l.peek() {
            Some(&t) if t.token_type == want => {
                self.l.next();
                Ok(t.literal)
            }
            Some(&t) => Err(eyre!("Wanted {want:?}, found {:?}", t.token_type)),
            None => Err(eyre!("Wanted {want:?}, found EOF")),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
}

impl Precedence {
    fn from_infix_operator(op: &InfixOperator) -> Precedence {
        use Precedence::*;

        match op {
            InfixOperator::Plus => Sum,
            InfixOperator::Minus => Sum,
            InfixOperator::Times => Product,
            InfixOperator::DividedBy => Product,
            InfixOperator::GreaterThan => LessGreater,
            InfixOperator::LessThan => LessGreater,
            InfixOperator::Equals => Equals,
            InfixOperator::NotEquals => Equals,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ast::{Expression, InfixOperator, PrefixOperator, Statement},
        lexer::Lexer,
        parser::{get_ast, Precedence},
    };

    use super::Parser;

    #[test]
    fn test_let_statements() {
        let input = b"
let x = 5;
let y = 10;
let foobar = 838383;";

        let (program, errors) = get_ast(Lexer::new(input));
        check_errors(errors);

        let expected = vec![
            Statement::Let {
                name: b"x".as_slice().into(),
                val: Expression::Integer(5),
            },
            Statement::Let {
                name: b"y".as_slice().into(),
                val: Expression::Integer(10),
            },
            Statement::Let {
                name: b"foobar".as_slice().into(),
                val: Expression::Integer(838383),
            },
        ];
        assert_eq!(program, expected);
    }

    #[test]
    fn test_return_statements() {
        let input = b"
return 5;
return 10;
return 993322;";

        let (program, errors) = get_ast(Lexer::new(input));
        check_errors(errors);

        let expected = vec![
            Statement::Return(Expression::Integer(5)),
            Statement::Return(Expression::Integer(10)),
            Statement::Return(Expression::Integer(993322)),
        ];
        assert_eq!(program, expected);
    }

    #[test]
    fn test_expression_statements() {
        let input = b"
a;
2;
ab;";

        let (program, errors) = get_ast(Lexer::new(input));
        check_errors(errors);

        let expected = vec![
            Statement::Expression(Expression::Identifier(b"a".as_slice().into())),
            Statement::Expression(Expression::Integer(2)),
            Statement::Expression(Expression::Identifier(b"ab".as_slice().into())),
        ];
        assert_eq!(program, expected);
    }

    fn check_errors(errors: Vec<color_eyre::Report>) {
        if !errors.is_empty() {
            for error in errors {
                println!("{error:#}");
            }
            panic!();
        }
    }

    #[test]
    fn test_expression_literals() {
        use Expression::*;

        let tests: &[(&[u8], Expression)] = &[
            (b"3", Integer(3)),
            (b"a", Identifier(b"a".as_slice().into())),
            (b"true", Boolean(true)),
            (b"false", Boolean(false)),
        ];
        run_expression_tests(tests);
    }

    #[test]
    fn test_prefix_expressions() {
        use Expression::*;
        use PrefixOperator::*;

        let tests: &[(&[u8], Expression)] = &[
            (b"!5", Prefix(Not, Integer(5).into())),
            (b"-15", Prefix(Negate, Integer(15).into())),
            (
                b"--5",
                Prefix(Negate, Prefix(Negate, Integer(5).into()).into()),
            ),
            (b"!true", Prefix(Not, Boolean(true).into())),
            (b"!false", Prefix(Not, Boolean(false).into())),
        ];
        run_expression_tests(tests);
    }

    #[test]
    fn test_if_expression() {
        use Expression::*;
        use InfixOperator::*;

        let tests: &[(&[u8], Expression)] = &[
            (
                b"if (1 < 2) {}",
                If {
                    condition: Infix(Integer(1).into(), LessThan, Integer(2).into()).into(),
                    consequence: vec![],
                    alternative: None,
                },
            ),
            (
                b"if (1 < 2) { 3 }",
                If {
                    condition: Infix(Integer(1).into(), LessThan, Integer(2).into()).into(),
                    consequence: vec![Statement::Expression(Integer(3))],
                    alternative: None,
                },
            ),
            (
                b"if (1 < 2) { 3 } else { 4 }",
                If {
                    condition: Infix(Integer(1).into(), LessThan, Integer(2).into()).into(),
                    consequence: vec![Statement::Expression(Integer(3))],
                    alternative: Some(vec![Statement::Expression(Integer(4))]),
                },
            ),
        ];
        run_expression_tests(tests);
    }

    #[test]
    fn test_fn_expression() {
        use Expression::*;
        use InfixOperator::*;

        let tests: &[(&[u8], Expression)] = &[
            (
                b"fn() {3; 4;}",
                Function {
                    arguments: vec![],
                    body: vec![
                        Statement::Expression(Integer(3)),
                        Statement::Expression(Integer(4)),
                    ],
                },
            ),
            (
                b"fn(x) { x; }",
                Function {
                    arguments: vec![Identifier(b"x".as_slice().into())],
                    body: vec![Statement::Expression(Identifier(b"x".as_slice().into()))],
                },
            ),
            (
                b"fn(x, y) { x + y; }",
                Function {
                    arguments: vec![
                        Identifier(b"x".as_slice().into()),
                        Identifier(b"y".as_slice().into()),
                    ],
                    body: vec![Statement::Expression(Infix(
                        Identifier(b"x".as_slice().into()).into(),
                        Plus,
                        Identifier(b"y".as_slice().into()).into(),
                    ))],
                },
            ),
        ];
        run_expression_tests(tests);
    }

    #[test]
    fn test_call_expression() {
        use Expression::*;
        use InfixOperator::*;
        let tests: &[(&[u8], Expression)] = &[
            (
                b"add(1, 2 * 3, 4 + 5)",
                Call(
                    Identifier(b"add".as_slice().into()).into(),
                    vec![
                        Integer(1),
                        Infix(Integer(2).into(), Times, Integer(3).into()),
                        Infix(Integer(4).into(), Plus, Integer(5).into()),
                    ],
                ),
            ),
            (
                b"fn(x) {x}(3)",
                Call(
                    Function {
                        arguments: vec![Identifier(b"x".as_slice().into())],
                        body: vec![Statement::Expression(Identifier(b"x".as_slice().into()))],
                    }
                    .into(),
                    vec![Integer(3)],
                ),
            ),
        ];
        run_expression_tests(tests)
    }

    #[test]
    fn test_infix_expressions() {
        use Expression::*;
        use InfixOperator::*;

        let tests: &[(&[u8], Expression)] = &[
            (b"5 + 5", Infix(Integer(5).into(), Plus, Integer(5).into())),
            (b"5 - 5", Infix(Integer(5).into(), Minus, Integer(5).into())),
            (b"5 * 5", Infix(Integer(5).into(), Times, Integer(5).into())),
            (
                b"5 / 5",
                Infix(Integer(5).into(), DividedBy, Integer(5).into()),
            ),
            (
                b"5 > 5",
                Infix(Integer(5).into(), GreaterThan, Integer(5).into()),
            ),
            (
                b"5 < 5",
                Infix(Integer(5).into(), LessThan, Integer(5).into()),
            ),
            (
                b"5 == 5",
                Infix(Integer(5).into(), Equals, Integer(5).into()),
            ),
            (
                b"5 != 5",
                Infix(Integer(5).into(), NotEquals, Integer(5).into()),
            ),
            (
                b"true == true",
                Infix(Boolean(true).into(), Equals, Boolean(true).into()),
            ),
            (
                b"true != false",
                Infix(Boolean(true).into(), NotEquals, Boolean(false).into()),
            ),
            (
                b"false == false",
                Infix(Boolean(false).into(), Equals, Boolean(false).into()),
            ),
        ];
        run_expression_tests(tests);
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests: &[(&[u8], &str)] = &[
            (b"-a * b", "((-a) * b)"),
            (b"!-a", "(!(-a))"),
            (b"a + b + c", "((a + b) + c)"),
            (b"a + b - c", "((a + b) - c)"),
            (b"a * b * c", "((a * b) * c)"),
            (b"a * b / c", "((a * b) / c)"),
            (b"a + b / c", "(a + (b / c))"),
            (b"a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            (b"5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            (b"5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                b"3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (b"3 > 5 == false", "((3 > 5) == false)"),
            (b"3 < 5 == true", "((3 < 5) == true)"),
            (b"1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            (b"(5 + 5) * 2", "((5 + 5) * 2)"),
            (b"2 / (5 + 5)", "(2 / (5 + 5))"),
            (b"-(5 + 5)", "(-(5 + 5))"),
            (b"!(true == true)", "(!(true == true))"),
            (b"a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                b"add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                b"add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];
        run_expression_tests_pretty_print(tests);
    }

    fn run_expression_tests(tests: &[(&[u8], Expression)]) {
        for (input, expect) in tests {
            let mut p = Parser {
                l: Lexer::new(input).peekable(),
            };

            let actual = p.parse_expr(Precedence::Lowest).unwrap();
            assert_eq!(&actual, expect);
        }
    }

    fn run_expression_tests_pretty_print(tests: &[(&[u8], &str)]) {
        for (input, expect) in tests {
            let mut p = Parser {
                l: Lexer::new(input).peekable(),
            };

            let actual = p.parse_expr(Precedence::Lowest).unwrap().to_string();
            assert_eq!(&actual, expect);
        }
    }
}
