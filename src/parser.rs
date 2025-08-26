use crate::expressions::{NewExpression};
use crate::tokens::NewToken;
use chumsky::prelude::*;
use logos::Logos;
use std::fs;

#[derive(Logos, Clone, PartialEq, Debug)]
#[logos(skip r"[ \t\r\n\f]+")]
enum LogosToken {
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
    #[token("return")]
    Return,

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
    let input = r#"
function hello_word(a, b) {
    return a + b;
}
    "#;

    let mut lexer = LogosToken::lexer(input);

    assert_eq!(lexer.next(), Some(Ok(LogosToken::Function)));
    assert_eq!(
        lexer.next(),
        Some(Ok(LogosToken::Identifier("hello_word".to_owned())))
    );
    assert_eq!(lexer.next(), Some(Ok(LogosToken::ParenOpen)));
    assert_eq!(
        lexer.next(),
        Some(Ok(LogosToken::Identifier("a".to_owned())))
    );
    assert_eq!(lexer.next(), Some(Ok(LogosToken::Comma)));
    assert_eq!(
        lexer.next(),
        Some(Ok(LogosToken::Identifier("b".to_owned())))
    );
    assert_eq!(lexer.next(), Some(Ok(LogosToken::ParenClose)));

    assert_eq!(lexer.next(), Some(Ok(LogosToken::BraceOpen)));

    assert_eq!(lexer.next(), Some(Ok(LogosToken::Return)));
    assert_eq!(
        lexer.next(),
        Some(Ok(LogosToken::Identifier("a".to_owned())))
    );
    assert_eq!(lexer.next(), Some(Ok(LogosToken::Plus)));
    assert_eq!(
        lexer.next(),
        Some(Ok(LogosToken::Identifier("b".to_owned())))
    );
    assert_eq!(lexer.next(), Some(Ok(LogosToken::Semicolon)));

    assert_eq!(lexer.next(), Some(Ok(LogosToken::BraceClose)));
}

#[test]
fn test_lexer_with_file() {
    let filename = ".\\output\\day_zero.mist.txt";
    let input = std::fs::read_to_string(filename).unwrap();
    let mut lexer = LogosToken::lexer(input.as_str());
    while let Some(Ok(token)) = lexer.next() {
        println!("{:?}", token);
    }
}

fn parser<'src>() -> impl Parser<'src, &'src str, ()> {
    // let ast = recursive::<_, _, extra::Err<Simple<LogosToken>>, _, _>(|ast| {
    //     let literal = select! {
    //         LogosToken::Number(x) => Expression::new_number_literal(x),
    //         LogosToken::Boolean(x) => Expression::new_boolean_literal(x)
    //     };
    // });

    // recursive(|expr| {
    //     let ident = select_ref! { LogosToken::Identifier(x) => x.clone() };
    //     let atom = choice((
    //         select_ref! {
    //             LogosToken::Number(x) => Expression::new_number_literal(*x) },
    //         select_ref! { LogosToken::Boolean(x) => Expression::new_boolean_literal(*x) },
    //     ));
    // })
    end()
}

#[test]
fn test_parser() {
    assert_eq!(parser().parse("").into_result(), Ok(()));

    assert!(parser().parse("123").has_errors());
}
