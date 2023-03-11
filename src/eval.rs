use color_eyre::Result;
use eyre::eyre;

use crate::{
    ast::{Expression, InfixOperator, PrefixOperator, Statement},
    object::Object,
};

pub fn eval_statements(statements: Vec<Statement>) -> Result<Object> {
    let mut evaluator = Evaluator {};
    let mut last_obj = None;
    for statement in statements {
        let obj = evaluator.eval_statement(statement)?;
        last_obj = Some(obj);
    }
    match last_obj {
        Some(obj) => Ok(obj),
        None => Ok(Object::Null),
    }
}

struct Evaluator {}

impl Evaluator {
    fn eval_statement(&mut self, statement: Statement) -> Result<Object> {
        match statement {
            Statement::Let { name, val } => todo!(),
            Statement::Return(_) => todo!(),
            Statement::Expression(expr) => self.eval_expression(expr),
        }
    }

    fn eval_expression(&mut self, expr: Expression) -> Result<Object> {
        match expr {
            Expression::Identifier(_) => todo!(),
            Expression::Integer(int) => Ok(Object::Integer(int)),
            Expression::Boolean(bool) => Ok(Object::Boolean(bool)),
            Expression::Prefix(op, expr) => self.eval_prefix(op, *expr),
            Expression::Infix(left, op, right) => self.eval_infix(*left, op, *right),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => todo!(),
            Expression::Function { arguments, body } => todo!(),
            Expression::Call(_, _) => todo!(),
        }
    }

    fn eval_prefix(&mut self, op: PrefixOperator, expr: Expression) -> Result<Object> {
        match self.eval_expression(expr)? {
            Object::Integer(int) => match op {
                PrefixOperator::Negate => Ok(Object::Integer(-int)),
                PrefixOperator::Not => Ok(Object::Boolean(false)),
            },
            Object::Boolean(bool) => match op {
                PrefixOperator::Negate => Ok(Object::Null),
                PrefixOperator::Not => Ok(Object::Boolean(!bool)),
            },
            Object::Null => match op {
                PrefixOperator::Negate => Err(eyre!("Can't negate a null")),
                PrefixOperator::Not => Ok(Object::Null),
            },
        }
    }

    fn eval_infix(
        &mut self,
        left: Expression,
        op: InfixOperator,
        right: Expression,
    ) -> Result<Object> {
        let left_obj = self.eval_expression(left)?;
        let right_obj = self.eval_expression(right)?;

        match (left_obj, right_obj) {
            (Object::Integer(left_int), Object::Integer(right_int)) => match op {
                InfixOperator::Plus => Ok(Object::Integer(left_int + right_int)),
                InfixOperator::Minus => Ok(Object::Integer(left_int - right_int)),
                InfixOperator::Times => Ok(Object::Integer(left_int * right_int)),
                InfixOperator::DividedBy => Ok(Object::Integer(left_int / right_int)),
                InfixOperator::GreaterThan => Ok(Object::Boolean(left_int > right_int)),
                InfixOperator::LessThan => Ok(Object::Boolean(left_int < right_int)),
                InfixOperator::Equals => Ok(Object::Boolean(left_int == right_int)),
                InfixOperator::NotEquals => Ok(Object::Boolean(left_int != right_int)),
            },
            (Object::Boolean(left_bool), Object::Boolean(right_bool)) => match op {
                InfixOperator::Equals => Ok(Object::Boolean(left_bool == right_bool)),
                InfixOperator::NotEquals => Ok(Object::Boolean(left_bool != right_bool)),
                _ => Err(eyre!("Unsupported operation {op:?} between two booleans")),
            },
            (left_obj, right_obj) => Err(eyre!(
                "Unsupported operation {op:?} between {left_obj:?} and {right_obj:?}"
            )),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{eval::eval_statements, lexer::Lexer, object::Object, parser::get_ast};

    #[test]
    fn test_eval_integer_expression() {
        use Object::*;
        let tests: &[(&[u8], Object)] = &[
            (b"5", Integer(5)),
            (b"10", Integer(10)),
            (b"-5", Integer(-5)),
            (b"-10", Integer(-10)),
            (b"5 + 5 + 5 + 5 - 10", Integer(10)),
            (b"2 * 2 * 2 * 2 * 2", Integer(32)),
            (b"-50 + 100 + -50", Integer(0)),
            (b"5 * 2 + 10", Integer(20)),
            (b"5 + 2 * 10", Integer(25)),
            (b"20 + 2 * -10", Integer(0)),
            (b"50 / 2 * 2 + 10", Integer(60)),
            (b"2 * (5 + 10)", Integer(30)),
            (b"3 * 3 * 3 + 10", Integer(37)),
            (b"3 * (3 * 3) + 10", Integer(37)),
            (b"(5 + 10 * 2 + 15 / 3) * 2 + -10", Integer(50)),
        ];
        run_tests(tests);
    }

    #[test]
    fn test_eval_boolean_expression() {
        use Object::*;
        let tests: &[(&[u8], Object)] = &[
            (b"true", Boolean(true)),
            (b"false", Boolean(false)),
            (b"1 < 2", Boolean(true)),
            (b"1 > 2", Boolean(false)),
            (b"1 < 1", Boolean(false)),
            (b"1 > 1", Boolean(false)),
            (b"1 == 1", Boolean(true)),
            (b"1 != 1", Boolean(false)),
            (b"1 == 2", Boolean(false)),
            (b"1 != 2", Boolean(true)),
            (b"true == true", Boolean(true)),
            (b"false == false", Boolean(true)),
            (b"true == false", Boolean(false)),
            (b"true != false", Boolean(true)),
            (b"false != true", Boolean(true)),
            (b"(1 < 2) == true", Boolean(true)),
            (b"(1 < 2) == false", Boolean(false)),
            (b"(1 > 2) == true", Boolean(false)),
            (b"(1 > 2) == false", Boolean(true)),
        ];
        run_tests(tests);
    }

    #[test]
    fn test_bang_operator() {
        use Object::*;
        let tests: &[(&[u8], Object)] = &[
            (b"!true", Boolean(false)),
            (b"!false", Boolean(true)),
            (b"!5", Boolean(false)),
            (b"!!true", Boolean(true)),
            (b"!!false", Boolean(false)),
            (b"!!5", Boolean(true)),
        ];
        run_tests(tests);
    }

    fn run_tests(tests: &[(&[u8], Object)]) {
        for (input, expect) in tests {
            let (statements, errors) = get_ast(Lexer::new(input));
            assert!(errors.is_empty());
            let actual = eval_statements(statements).unwrap();
            assert_eq!(&actual, expect, "{}", std::str::from_utf8(input).unwrap());
        }
    }
}
