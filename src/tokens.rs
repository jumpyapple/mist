use crate::expressions::NewExpression;
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum NewDefaultValue {
    #[serde(rename = "null")]
    // The "null". The String is needed for serde to capture the "null".
    NullStringDefault(String),
    // Has to be Expression because the default value can be an Expression::Literal.
    ExpressionDefault(Box<NewExpression>),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag = "token_type")]
pub(crate) enum NewToken {
    True,
    False,
    Number {
        Value: f64,
    },
    String {
        value: String,
    },
    Identifier {
        value: String,
        default_value: Option<NewDefaultValue>,
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

impl NewToken {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            NewToken::True => write!(f, "true"),
            NewToken::False => write!(f, "false"),
            NewToken::Number { Value } => write!(f, "{}", Value),
            NewToken::String { value } => write!(f, "\"{}\"", value),
            NewToken::Identifier {
                value,
                default_value,
            } => {
                write!(f, "{}", value)?;
                if let Some(v) = default_value {
                    match v {
                        NewDefaultValue::NullStringDefault(_) => write!(f, " = null")?,
                        NewDefaultValue::ExpressionDefault(exp) => {
                            write!(f, " = ")?;
                            exp.fmt_indented(f, 0)?;
                        }
                    };
                }
                Ok(())
            }
            NewToken::DoubleEqual => write!(f, "=="),
            NewToken::BangEqual => write!(f, "!="),
            NewToken::LessEqual => write!(f, "<="),
            NewToken::Less => write!(f, "<"),
            NewToken::GreaterEqual => write!(f, ">="),
            NewToken::Greater => write!(f, ">"),
            NewToken::Plus => write!(f, "+"),
            NewToken::Minus => write!(f, "-"),
            NewToken::Star => write!(f, "*"),
            NewToken::Slash => write!(f, "/"),
            NewToken::And => write!(f, "&&"),
            NewToken::Bang => write!(f, "!"),
        }
    }
}

impl fmt::Display for NewToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_indented(f, 0)
    }
}

#[test]
fn test_tokens_de() {
    let input = r#"{ "token_type": "Number", "Value": 0.99 }"#;
    let result: NewToken = serde_json::from_str(input).expect("failed to deserialize Number");
    assert_eq!(result, NewToken::Number { Value: 0.99 });

    let input = r#"{ "token_type": "String", "value": "Music/Jingles/GoToSleep"}"#;
    let result: NewToken = serde_json::from_str(input).expect("failed to deserialize String");
    assert_eq!(
        result,
        NewToken::String {
            value: "Music/Jingles/GoToSleep".to_string()
        }
    );

    let input = r#"{"token_type": "Identifier", "value": "__get_new_day_spawn_x" }"#;
    let result: NewToken = serde_json::from_str(input).expect("failed to deserialize Identifier");
    assert_eq!(
        result,
        NewToken::Identifier {
            value: "__get_new_day_spawn_x".to_string(),
            default_value: None
        }
    );

    let input = r#"{ "token_type": "Identifier", "value": "dir", "default_value": "null" }"#;
    let result: NewToken = serde_json::from_str(input)
        .expect("failed to deserialize Identifier (with default value = \"null\")");
    assert_eq!(
        result,
        NewToken::Identifier {
            value: "dir".to_string(),
            default_value: Some(NewDefaultValue::NullStringDefault("null".to_string()))
        }
    );

    let input = r#"{"token_type": "DoubleEqual"}"#;
    let result: NewToken = serde_json::from_str(input).expect("failed to deserialize DoubleEqual");
    assert_eq!(result, NewToken::DoubleEqual);
}

#[test]
fn test_token_format_human() {
    let input = NewToken::String {
        value: "hello".to_string(),
    };
    assert_eq!(format!("{}", input), "\"hello\"");

    let input = NewToken::Identifier {
        value: "hello".to_string(),
        default_value: Some(NewDefaultValue::NullStringDefault("null".to_string())),
    };
    assert_eq!(format!("{}", input), "hello = null");

    let input = NewToken::Less;
    assert_eq!(format!("{}", input), "<");
    let input = NewToken::And;
    assert_eq!(format!("{}", input), "&&");
}
