use std::fs;
use chumsky::prelude::*;
use logos::Logos;

#[derive(Logos, Clone, PartialEq, Debug)]
#[logos(skip r"[ \t\r\n\f]+")]
enum Token {
    Error,

    #[token("false", |_| false)]
    #[token("true", |_| true)]
    Boolean(bool),

    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("simultaneous")]
    Simultaneous,
    #[token("free")]
    Free,
    #[token("function")]
    Function,
    #[token("var")]
    Var,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_owned())]
    Identifier(String),

    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,
    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,

    #[token("==")]
    DoubleEqual,
    #[token("!=")]
    BangEqual,
    #[token("<=")]
    LessEqual,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,

    #[token("=")]
    Equal,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("!")]
    Bang,
    #[token("&&")]
    And,

    #[token("null")]
    Null,

    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", |lex| lex.slice().parse::<f64>().unwrap())]
    Number(f64),

    #[regex(r#""([^"\\\x00-\x1F]|\\(["\\bnfrt/]|u[a-fA-F0-9]{4}))*""#, |lex| lex.slice().to_owned())]
    String(String),
}

#[test]
fn test_lexer() {
    let filename = ".\\output\\day_zero.mist.txt";
    let src = fs::read_to_string(&filename).expect("Failed to read file");

    let mut lexer = Token::lexer(src.as_str());

    while let Some(token) = lexer.next() {
        println!("{:?}", token);
    }
}

fn parser<'src>() -> impl Parser<'src, &'src str, ()> {
    end()
}

#[test]
fn test_parser() {
    assert_eq!(parser().parse("").into_result(), Ok(()));

    assert!(parser().parse("123").has_errors());
}