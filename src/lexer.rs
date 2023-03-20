use crate::{
    token::Token,
    util::{char_one_of, digit, ident, one_of, text, Parser},
};

pub struct Lexer<'a> {
    input: &'a [u8],
    anchor: usize,
    position: usize,
}

fn token<'a>() -> impl Parser<'a, Token<'a>> {
    text(b"==")
        .or(text(b"!="))
        .or(char_one_of(b"+-*/<>,;(){}=!").map(|c| ))
        .or(ident())
        .or(digit())
        .map(|l| Token {
            token_type: l.into(),
            literal: l,
        })
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let cur_char = self.peak()?;
        self.position += 1;

        match cur_char {
            b'+' | b'-' | b'*' | b'/' | b'<' | b'>' | b',' | b';' | b'(' | b')' | b'{' | b'}' => {}
            b'=' => {
                if self.peak() == Some(b'=') {
                    self.position += 1
                }
            }
            b'!' => {
                if self.peak() == Some(b'=') {
                    self.position += 1
                }
            }
            _ if is_letter(cur_char) => self.seak_rest_of_identifier(),
            _ if is_digit(cur_char) => self.seak_rest_of_digit(),
            _ => {}
        };
        Some(self.consume())
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [u8]) -> Lexer<'a> {
        Lexer {
            input,
            anchor: 0,
            position: 0,
        }
    }

    fn seak_rest_of_identifier(&mut self) {
        while matches!(self.peak(), Some(c) if is_letter(c)) {
            self.position += 1;
        }
    }

    fn seak_rest_of_digit(&mut self) {
        while matches!(self.peak(), Some(c) if is_digit(c)) {
            self.position += 1;
        }
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peak(), Some(c) if is_whitespace(c)) {
            self.position += 1;
        }
        self.anchor = self.position;
    }

    fn peak(&self) -> Option<u8> {
        self.input.get(self.position).copied()
    }

    fn consume(&mut self) -> Token<'a> {
        let literal = &self.input[self.anchor..self.position];
        let token = Token {
            token_type: literal.into(),
            literal,
        };
        self.anchor = self.position;
        token
    }
}

pub fn is_letter(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_'
}

fn is_whitespace(c: u8) -> bool {
    c == b' ' || c == b'\t' || c == b'\n' || c == b'\r'
}

pub fn is_digit(c: u8) -> bool {
    c.is_ascii_digit()
}

#[cfg(test)]
mod test {
    use crate::token::TokenType::{self, *};

    use super::Lexer;

    #[test]
    fn test_next_token() {
        let input = b"let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;";

        let expected: &[(TokenType, &[u8])] = &[
            (Let, b"let"),
            (Ident, b"five"),
            (Assign, b"="),
            (Int, b"5"),
            (Semicolon, b";"),
            (Let, b"let"),
            (Ident, b"ten"),
            (Assign, b"="),
            (Int, b"10"),
            (Semicolon, b";"),
            (Let, b"let"),
            (Ident, b"add"),
            (Assign, b"="),
            (Function, b"fn"),
            (LParen, b"("),
            (Ident, b"x"),
            (Comma, b","),
            (Ident, b"y"),
            (RParen, b")"),
            (LBrace, b"{"),
            (Ident, b"x"),
            (Plus, b"+"),
            (Ident, b"y"),
            (Semicolon, b";"),
            (RBrace, b"}"),
            (Semicolon, b";"),
            (Let, b"let"),
            (Ident, b"result"),
            (Assign, b"="),
            (Ident, b"add"),
            (LParen, b"("),
            (Ident, b"five"),
            (Comma, b","),
            (Ident, b"ten"),
            (RParen, b")"),
            (Semicolon, b";"),
            (Bang, b"!"),
            (Minus, b"-"),
            (Slash, b"/"),
            (Asterisk, b"*"),
            (Int, b"5"),
            (Semicolon, b";"),
            (Int, b"5"),
            (LT, b"<"),
            (Int, b"10"),
            (GT, b">"),
            (Int, b"5"),
            (Semicolon, b";"),
            (If, b"if"),
            (LParen, b"("),
            (Int, b"5"),
            (LT, b"<"),
            (Int, b"10"),
            (RParen, b")"),
            (LBrace, b"{"),
            (Return, b"return"),
            (True, b"true"),
            (Semicolon, b";"),
            (RBrace, b"}"),
            (Else, b"else"),
            (LBrace, b"{"),
            (Return, b"return"),
            (False, b"false"),
            (Semicolon, b";"),
            (RBrace, b"}"),
            (Int, b"10"),
            (Eq, b"=="),
            (Int, b"10"),
            (Semicolon, b";"),
            (Int, b"10"),
            (NotEq, b"!="),
            (Int, b"9"),
            (Semicolon, b";"),
        ];
        let expected = expected.iter();
        let mut l = Lexer::new(input.as_slice());

        for (expected_token_type, expected_literal) in expected {
            let actual_token = l.next().unwrap();
            println!("{actual_token:?}");
            assert_eq!(&actual_token.token_type, expected_token_type);
            assert_eq!(&actual_token.literal, expected_literal);
        }

        assert!(l.next().is_none());
    }
}
