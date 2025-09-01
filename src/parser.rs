use crate::expressions::Expression;
use crate::statements::Statement;
use crate::tokens::{BinaryOperator, Token};
use chumsky::input::{Stream, ValueInput};
use chumsky::prelude::*;
use clap::builder::TypedValueParser;
use logos::Logos;

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

#[derive(Logos, Clone, PartialEq, Debug)]
#[logos(skip r"[ \t\r\n\f]+")]
enum LogosToken<'a> {
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

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier(&'a str),

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
    #[token(">=")]
    GreaterEqual,

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

    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?")]
    Number(&'a str),

    #[regex(r#""([^"\\\x00-\x1F]|\\(["\\bnfrt/]|u[a-fA-F0-9]{4}))*""#)]
    String(&'a str),
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
    assert_eq!(lexer.next(), Some(Ok(LogosToken::Identifier("hello_word"))));
    assert_eq!(lexer.next(), Some(Ok(LogosToken::ParenOpen)));
    assert_eq!(lexer.next(), Some(Ok(LogosToken::Identifier("a"))));
    assert_eq!(lexer.next(), Some(Ok(LogosToken::Comma)));
    assert_eq!(lexer.next(), Some(Ok(LogosToken::Identifier("b"))));
    assert_eq!(lexer.next(), Some(Ok(LogosToken::ParenClose)));

    assert_eq!(lexer.next(), Some(Ok(LogosToken::BraceOpen)));

    assert_eq!(lexer.next(), Some(Ok(LogosToken::Return)));
    assert_eq!(lexer.next(), Some(Ok(LogosToken::Identifier("a"))));
    assert_eq!(lexer.next(), Some(Ok(LogosToken::Plus)));
    assert_eq!(lexer.next(), Some(Ok(LogosToken::Identifier("b"))));
    assert_eq!(lexer.next(), Some(Ok(LogosToken::Semicolon)));

    assert_eq!(lexer.next(), Some(Ok(LogosToken::BraceClose)));
}

#[test]
fn test_lexer_with_file() {
    let filename = ".\\output\\day_zero.mist.txt";
    let input = std::fs::read_to_string(filename).unwrap();
    let mut lexer = LogosToken::lexer(input.as_str());

    let mut count = 0;
    while let Some(Ok(token)) = lexer.next() {
        println!("{:?}", token);
        count += 1;

        if (count > 10) {
            break;
        }
    }
    assert!(false);
}
fn bin_expr_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Expression, extra::Err<Rich<'tokens, LogosToken<'src>>>>
where
    I: ValueInput<'tokens, Token = LogosToken<'src>, Span = SimpleSpan>,
{
    // TODO: Somehow add Unary expression, Assign, Call and Grouping.
    recursive(|expr| {
        let inline_expr = recursive(|inline_expr| {
            let val = select! {
                LogosToken::Boolean(x) => match x {
                    true => Expression::Literal { value: Token::True },
                    false => Expression::Literal { value: Token::False },
                },
                LogosToken::Number(x) => Expression::Literal { value: Token::Number { value: x.parse::<f64>().unwrap() }},
                LogosToken::String(x) => Expression::Literal { value: Token::String { value: x.to_string() }}
            };

            let ident = select! { LogosToken::Identifier(ident) => ident };

            let items = inline_expr
                .clone()
                .separated_by(just(LogosToken::Comma))
                .collect::<Vec<_>>();

            let atom = val
                .or(ident.map(|x| Expression::Named {
                    name: Token::Identifier {
                        value: x.to_string(),
                        default_value: None,
                    },
                }))
                .boxed();

            let call = atom.foldl_with(
                items
                    .delimited_by(just(LogosToken::ParenOpen), just(LogosToken::ParenClose))
                    .repeated(),
                |f, args, e| Expression::Call {
                    call: Box::new(f),
                    args: args
                }
            );

            let op = just(LogosToken::Star)
                .to(BinaryOperator::Star)
                .or(just(LogosToken::Slash).to(BinaryOperator::Slash));
            let product = call
                .clone()
                .foldl_with(op.then(call).repeated(), |a, (op, b), e| {
                    Expression::Binary {
                        operator: op,
                        left: Box::new(a),
                        right: Box::new(b),
                    }
                });

            let op = just(LogosToken::Plus)
                .to(BinaryOperator::Plus)
                .or(just(LogosToken::Minus).to(BinaryOperator::Minus));
            let sum = product
                .clone()
                .foldl_with(op.then(product).repeated(), |a, (op, b), e| {
                    Expression::Binary {
                        operator: op,
                        left: Box::new(a),
                        right: Box::new(b),
                    }
                });

            let op = just(LogosToken::DoubleEqual)
                .to(BinaryOperator::DoubleEqual)
                .or(just(LogosToken::BangEqual).to(BinaryOperator::BangEqual))
                .or(just(LogosToken::Less).to(BinaryOperator::Less))
                .or(just(LogosToken::LessEqual).to(BinaryOperator::LessEqual))
                .or(just(LogosToken::Greater).to(BinaryOperator::Greater))
                .or(just(LogosToken::GreaterEqual).to(BinaryOperator::GreaterEqual));
            let compare = sum
                .clone()
                .foldl_with(op.then(sum).repeated(), |a, (op, b), e| {
                    Expression::Binary {
                        operator: op,
                        left: Box::new(a),
                        right: Box::new(b),
                    }
                });

            let op = just(LogosToken::And).to(BinaryOperator::And);
            let logical = compare
                .clone()
                .foldl_with(op.then(compare).repeated(), |a, (op, b), e| {
                    Expression::Binary {
                        operator: op,
                        left: Box::new(a),
                        right: Box::new(b),
                    }
                });

            logical
        });

        inline_expr
    })
}

#[test]
fn test_bin_expr_parser() {
    let input = "1.0 + 5.0 == 70.3";
    let token_iter = LogosToken::lexer(input)
        .spanned()
        .map(|(token, span)| match token {
            Ok(t) => (t, span.into()),
            Err(()) => (LogosToken::Error, span.into()),
        });

    let token_stream = Stream::from_iter(token_iter).map((0..input.len()).into(), |(t, s)| (t, s));

    match bin_expr_parser().parse(token_stream).into_result() {
        Ok(ast) => {
            println!("{:#?}", ast);
            // TODO: Actually check the AST.
        }
        Err(parse_errs) => parse_errs
            .into_iter()
            .for_each(|err| println!("Parse error: {:#?}", err)),
    }
}

fn var_stmt_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Statement, extra::Err<Rich<'tokens, LogosToken<'src>>>>
where
    I: ValueInput<'tokens, Token = LogosToken<'src>, Span = SimpleSpan>,
{
    let ident = select! { LogosToken::Identifier(ident) => ident };

    any()
        .filter(|token| match token {
            LogosToken::Var => true,
            _ => false,
        })
        .ignore_then(ident)
        .then_ignore(select!(LogosToken::Equal))
        .then(ident)
        .then_ignore(select!(LogosToken::Semicolon))
        .map(|(a, b)| Statement::Var {
            name: Token::Identifier {
                value: a.to_string(),
                default_value: None,
            },
            initializer: Expression::Named {
                name: Token::Identifier {
                    value: b.to_string(),
                    default_value: None,
                },
            },
        })
}

#[test]
fn test_var_stmt_parser() {
    let input = "var x = ari_x;";
    let token_iter = LogosToken::lexer(input)
        .spanned()
        .map(|(token, span)| match token {
            Ok(t) => (t, span.into()),
            Err(()) => (LogosToken::Error, span.into()),
        });

    let token_stream = Stream::from_iter(token_iter).map((0..input.len()).into(), |(t, s)| (t, s));

    match var_stmt_parser().parse(token_stream).into_result() {
        Ok(ast) => {
            println!("{:#?}", ast);
            // TODO: Actually check the AST.
            println!("--------");
            let json_string = serde_json::to_string_pretty(&ast).unwrap();
            println!("{}", json_string);
        }
        Err(parse_errs) => parse_errs
            .into_iter()
            .for_each(|err| println!("Parse error: {:#?}", err)),
    }
}
