use std::fmt::Display;

use crate::lexer::{is_digit, is_letter};

#[derive(Clone, Copy, Debug)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub literal: &'a [u8],
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Type: {:?} | Literal: '{}'",
            self.token_type,
            String::from_utf8_lossy(self.literal)
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    Illegal,

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

    Ident,
    Int,
}

impl From<&[u8]> for TokenType {
    fn from(value: &[u8]) -> Self {
        match value {
            b"" => TokenType::Illegal,

            b"=" => TokenType::Assign,
            b"+" => TokenType::Plus,
            b"-" => TokenType::Minus,
            b"!" => TokenType::Bang,
            b"*" => TokenType::Asterisk,
            b"/" => TokenType::Slash,

            b"<" => TokenType::LT,
            b">" => TokenType::GT,

            b"==" => TokenType::Eq,
            b"!=" => TokenType::NotEq,

            b"," => TokenType::Comma,
            b";" => TokenType::Semicolon,

            b"(" => TokenType::LParen,
            b")" => TokenType::RParen,
            b"{" => TokenType::LBrace,
            b"}" => TokenType::RBrace,

            b"fn" => TokenType::Function,
            b"let" => TokenType::Let,
            b"true" => TokenType::True,
            b"false" => TokenType::False,
            b"if" => TokenType::If,
            b"else" => TokenType::Else,
            b"return" => TokenType::Return,

            _ if is_letter(value[0]) => TokenType::Ident,
            _ if is_digit(value[0]) => TokenType::Int,

            _ => TokenType::Illegal,
        }
    }
}
