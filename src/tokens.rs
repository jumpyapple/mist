use crate::expressions::Expression;
use crate::statements::Optional;
use crate::statements::Statement::Expr;
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag = "token_type")]
pub(crate) enum Token {
    True,
    False,
    Number {
        #[serde(rename = "Value")]
        value: f64,
    },
    String {
        value: String,
    },
    Identifier {
        value: String,
        default_value: Option<Optional>,
    },

    // Operators.
    DoubleEqual,
    BangEqual,
    LessEqual,
    Less,
    GreaterEqual,
    Greater,
    Plus,
    Minus,
    Star,
    Slash,
    And,

    // Unary Operators.
    Bang,
}

impl Token {
    pub(crate) fn fmt_indented(&self, f: &mut fmt::Formatter<'_>, _indent: usize) -> fmt::Result {
        match self {
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Number { value: Value } => write!(f, "{}", Value),
            Token::String { value } => write!(f, "\"{}\"", value),
            Token::Identifier {
                value,
                default_value,
            } => {
                write!(f, "{}", value)?;
                if let Some(v) = default_value {
                    match v {
                        Optional::NullString(_) => write!(f, " = null")?,
                        Optional::OptionalExpression(exp) => {
                            write!(f, " = ")?;
                            exp.fmt_indented(f, 0)?;
                        }
                        _ => write!(f, "([!!] Did not expect OptionalStatement.)")?,
                    };
                }
                Ok(())
            }
            Token::DoubleEqual => write!(f, "=="),
            Token::BangEqual => write!(f, "!="),
            Token::LessEqual => write!(f, "<="),
            Token::Less => write!(f, "<"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Greater => write!(f, ">"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::And => write!(f, "&&"),
            Token::Bang => write!(f, "!"),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_indented(f, 0)
    }
}

#[test]
fn test_tokens_de() {
    let input = r#"{ "token_type": "Number", "Value": 0.99 }"#;
    let result: Token = serde_json::from_str(input).expect("failed to deserialize Number");
    assert_eq!(result, Token::Number { value: 0.99 });

    let input = r#"{ "token_type": "String", "value": "Music/Jingles/GoToSleep"}"#;
    let result: Token = serde_json::from_str(input).expect("failed to deserialize String");
    assert_eq!(
        result,
        Token::String {
            value: "Music/Jingles/GoToSleep".to_string()
        }
    );

    let input = r#"{"token_type": "Identifier", "value": "__get_new_day_spawn_x" }"#;
    let result: Token = serde_json::from_str(input).expect("failed to deserialize Identifier");
    assert_eq!(
        result,
        Token::Identifier {
            value: "__get_new_day_spawn_x".to_string(),
            default_value: None
        }
    );

    let input = r#"{ "token_type": "Identifier", "value": "dir", "default_value": "null" }"#;
    let result: Token = serde_json::from_str(input)
        .expect("failed to deserialize Identifier (with default value = \"null\")");
    assert_eq!(
        result,
        Token::Identifier {
            value: "dir".to_string(),
            default_value: Some(Optional::NullString("null".to_string()))
        }
    );

    let input = r#"{"token_type": "DoubleEqual"}"#;
    let result: Token = serde_json::from_str(input).expect("failed to deserialize DoubleEqual");
    assert_eq!(result, Token::DoubleEqual);
}

#[test]
fn test_identifier_de_with_default_value_null() {
    let input = r#"{"token_type": "Identifier", "value": "__get_new_day_spawn_x", "default_value": "null" }"#;
    let result: Token = serde_json::from_str(input).expect("failed to deserialize Identifier");
    assert_eq!(
        result,
        Token::Identifier {
            value: "__get_new_day_spawn_x".to_string(),
            default_value: Some(Optional::NullString("null".to_string()))
        }
    );
}

#[test]
fn test_identifier_de_with_default_value_expr() {
    let input = r#"
{
    "token_type": "Identifier",
    "value": "__get_new_day_spawn_x",
    "default_value": {
        "expr_type": "Literal",
        "value": { "token_type": "Number", "Value": 0.0}
    }
}
    "#;
    let result: Token = serde_json::from_str(input).expect("failed to deserialize Identifier");
    assert_eq!(
        result,
        Token::Identifier {
            value: "__get_new_day_spawn_x".to_string(),
            default_value: Some(Optional::OptionalExpression(Box::from(
                Expression::Literal {
                    value: Token::Number { value: 0.0 }
                },
            )))
        }
    );
}

#[test]
fn test_token_format_human() {
    let input = Token::String {
        value: "hello".to_string(),
    };
    assert_eq!(format!("{}", input), "\"hello\"");

    let input = Token::Identifier {
        value: "hello".to_string(),
        default_value: Some(Optional::NullString("null".to_string())),
    };
    assert_eq!(format!("{}", input), "hello = null");

    let input = Token::Less;
    assert_eq!(format!("{}", input), "<");
    let input = Token::And;
    assert_eq!(format!("{}", input), "&&");
}
