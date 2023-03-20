use crate::util::{any_char, chr, eof, text, Parser};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Illegal(u8),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    LT,
    GT,

    Eq,
    NotEq,

    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,

    Ident(Vec<u8>),
    Int(u64),
}

pub fn get_tokens(input: &[u8]) -> Vec<Token> {
    let (tokens, _) = all_tokens().parse(input).unwrap();
    tokens
}

fn all_tokens<'a>() -> impl Parser<'a, Vec<Token>> {
    token()
        .repeated()
        .then_ignore(whitespace().optional())
        .then_ignore(eof())
}

fn token<'a>() -> impl Parser<'a, Token> {
    use Token::*;

    whitespace().optional().ignore_then(
        text(b"==")
            .to(Eq)
            .or(text(b"!=").to(NotEq))
            .or(chr(b'=').to(Assign))
            .or(chr(b'+').to(Plus))
            .or(chr(b'-').to(Minus))
            .or(chr(b'!').to(Bang))
            .or(chr(b'*').to(Asterisk))
            .or(chr(b'/').to(Slash))
            .or(chr(b'<').to(LT))
            .or(chr(b'>').to(GT))
            .or(chr(b',').to(Comma))
            .or(chr(b';').to(Semicolon))
            .or(chr(b'(').to(LParen))
            .or(chr(b')').to(RParen))
            .or(chr(b'{').to(LBrace))
            .or(chr(b'}').to(RBrace))
            .or(text(b"fn").to(Function))
            .or(text(b"let").to(Let))
            .or(text(b"true").to(True))
            .or(text(b"false").to(False))
            .or(text(b"if").to(If))
            .or(text(b"else").to(Else))
            .or(text(b"return").to(Return))
            .or(ident().map(Ident))
            .or(digit().map(Int))
            .or(any_char().map(Illegal)),
    )
}

fn ident<'a>() -> impl Parser<'a, Vec<u8>> {
    any_char()
        .pred(|&c| c.is_ascii_alphabetic() || c == b'_')
        .repeated()
}

fn digit<'a>() -> impl Parser<'a, u64> {
    any_char()
        .pred(|c| c.is_ascii_digit())
        .repeated()
        .map(|d| std::str::from_utf8(&d).unwrap().parse().unwrap())
}

fn whitespace<'a>() -> impl Parser<'a, Vec<u8>> {
    any_char()
        .pred(|&c| c == b' ' || c == b'\t' || c == b'\n' || c == b'\r')
        .repeated()
}

#[cfg(test)]
mod test {
    use std::iter::zip;

    use crate::{token::Token, util::Parser};

    use super::all_tokens;

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

        use Token::*;
        let expected: &[Token] = &[
            Let,
            Ident(b"five".to_vec()),
            Assign,
            Int(5),
            Semicolon,
            Let,
            Ident(b"ten".to_vec()),
            Assign,
            Int(10),
            Semicolon,
            Let,
            Ident(b"add".to_vec()),
            Assign,
            Function,
            LParen,
            Ident(b"x".to_vec()),
            Comma,
            Ident(b"y".to_vec()),
            RParen,
            LBrace,
            Ident(b"x".to_vec()),
            Plus,
            Ident(b"y".to_vec()),
            Semicolon,
            RBrace,
            Semicolon,
            Let,
            Ident(b"result".to_vec()),
            Assign,
            Ident(b"add".to_vec()),
            LParen,
            Ident(b"five".to_vec()),
            Comma,
            Ident(b"ten".to_vec()),
            RParen,
            Semicolon,
            Bang,
            Minus,
            Slash,
            Asterisk,
            Int(5),
            Semicolon,
            Int(5),
            LT,
            Int(10),
            GT,
            Int(5),
            Semicolon,
            If,
            LParen,
            Int(5),
            LT,
            Int(10),
            RParen,
            LBrace,
            Return,
            True,
            Semicolon,
            RBrace,
            Else,
            LBrace,
            Return,
            False,
            Semicolon,
            RBrace,
            Int(10),
            Eq,
            Int(10),
            Semicolon,
            Int(10),
            NotEq,
            Int(9),
            Semicolon,
        ];
        let expected = expected.iter();
        let (actual, _) = all_tokens().parse(input).unwrap();
        assert_eq!(expected.len(), actual.len());

        for (expected_token, actual_token) in zip(expected, actual) {
            assert_eq!(expected_token, &actual_token);
        }
    }
}
