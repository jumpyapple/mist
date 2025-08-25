use std::fmt;
use std::fmt::write;
use crate::expressions::{Expression, NewExpression};
use serde::{Deserialize, Serialize};
use crate::MistContainer;
use crate::tokens::Value::NumberValue;

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum NewDefaultValue {
    #[serde(rename = "null")]
    NullStringDefault, // The "null".
    ExpressionDefault(Box<NewExpression>),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag = "token_type")]
pub(crate) enum NewToken {
    True,
    False,
    Null,
    Number { Value: f64 },
    String { value: String },
    Identifier { value: String, default_value: Option<NewDefaultValue> },

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
            NewToken::Null => write!(f, "null"),
            NewToken::Number { Value } => write!(f, "{}", Value),
            NewToken::String { value } => write!(f, "\"{}\"", value),
            NewToken::Identifier { value, default_value } => {
                write!(f, "{}", value)?;
                if let Some(v) = default_value {
                    match v {
                        NewDefaultValue::NullStringDefault => write!(f, "null")?,
                        NewDefaultValue::ExpressionDefault(exp) => todo!(),
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
    assert_eq!(result, NewToken::String { value: "Music/Jingles/GoToSleep".to_string() });

    let input = r#"{"token_type": "Identifier", "value": "__get_new_day_spawn_x" }"#;
    let result: NewToken = serde_json::from_str(input).expect("failed to deserialize Identifier");
    assert_eq!(result, NewToken::Identifier { value: "__get_new_day_spawn_x".to_string(), default_value: None });

    let input = r#"{ "token_type": "Identifier", "value": "dir", "default_value": "null" }"#;
    let result: NewToken = serde_json::from_str(input).expect("failed to deserialize Identifier (with default value = \"null\")");
    assert_eq!(result, NewToken::Identifier { value: "dir".to_string(), default_value: Some(NewDefaultValue::NullStringDefault) });

    let input = r#"{"token_type": "DoubleEqual"}"#;
    let result: NewToken = serde_json::from_str(input).expect("failed to deserialize DoubleEqual");
    assert_eq!(result, NewToken::DoubleEqual);
}

#[test]
fn test_token_format_human() {
    let input = NewToken::String { value: "hello".to_string() };
    assert_eq!(format!("{}", input), "\"hello\"");
}



#[derive(Serialize, Deserialize, Debug)]
pub enum TokenType {
    Identifier,
    Number,
    String,
    #[serde(rename = "True")]
    BooleanTrue,
    #[serde(rename = "False")]
    BooleanFalse,
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
    Bang,
    And,
}

// This is not a direct map.
#[derive(Serialize, Deserialize)]
#[serde(untagged)]
pub enum Token {
    // Identifier (without the default_value), Number (with capital V in value), String (with lowercase V in value).
    ValueToken(ValueToken),
    IdentifierWithDefaultValueToken(IdentifierWithDefaultValueToken),
    // BooleanTrue, BooleanFalse, Minus, DoubleEqual.
    // NOTE: If this is first, serde seem to take the lazy route and pick it
    TypeOnlyToken(TypeOnlyToken),
}

impl Token {
    pub(crate) fn new_boolean(val: bool) -> Self {
        if val {
            Token::TypeOnlyToken(TypeOnlyToken { token_type: TokenType::BooleanTrue })
        } else {
            Token::TypeOnlyToken(TypeOnlyToken { token_type: TokenType::BooleanFalse })
        }
    }

    pub(crate) fn new_number(val: f64) -> Self {
        Token::ValueToken(ValueToken { token_type: TokenType::Number, value: Value::NumberValue(val) })
    }

    pub(crate) fn new_string(val: String) -> Self {
        Token::ValueToken(ValueToken { token_type: TokenType::String, value: Value::StringValue(val) })
    }

    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Token::TypeOnlyToken(t) => t.fmt_indented(f, indent),
            Token::ValueToken(t) => t.fmt_indented(f, indent),
            Token::IdentifierWithDefaultValueToken(t) => t.fmt_indented(f, indent),
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TypeOnlyToken {
    pub(crate) token_type: TokenType, // BooleanTrue, BooleanFalse, DoubleEqual, Minus
}

impl TypeOnlyToken {

    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _indent: usize,
    ) -> std::fmt::Result {
        match self.token_type {
            TokenType::BooleanTrue => write!(f, "true"),
            TokenType::BooleanFalse => write!(f, "false"),
            TokenType::DoubleEqual => write!(f, "=="),
            TokenType::BangEqual => write!(f, "!="),
            TokenType::LessEqual => write!(f, "<="),
            TokenType::Less => write!(f, "<"),
            TokenType::GreaterEqual => write!(f, ">="),
            TokenType::Greater => write!(f, ">"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Star => write!(f, "*"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Bang => write!(f, "!"),
            TokenType::And => write!(f, "&&"),
            _ => write!(
                f,
                "([!!] Unknown type for TypeOnlyToken. found {:?})",
                self.token_type
            ),
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
pub enum Value {
    StringValue(String), // Could be Identifier's name or a String value
    NumberValue(f64),
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ValueToken {
    pub token_type: TokenType, // Identifier (without default_value), Number, String.
    #[serde(alias = "Value")]
    pub value: Value,
}

impl ValueToken {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _indent: usize,
    ) -> std::fmt::Result {
        match self.token_type {
            TokenType::Identifier => match &self.value {
                Value::StringValue(val) => write!(f, "{}", val),
                Value::NumberValue(val) => {
                    write!(f, "([!!] An Identifier has a number as its name: {})", val)
                }
            },
            TokenType::Number => match &self.value {
                Value::StringValue(val) => {
                    write!(f, "([!!] A NumberValue has a string as its value: {})", val)
                }
                Value::NumberValue(val) => write!(f, "{:.2}", val),
            },
            TokenType::String => match &self.value {
                Value::StringValue(val) => write!(f, "\"{}\"", val),
                Value::NumberValue(val) => {
                    write!(f, "([!!] A StringValue has a number as its value: {})", val)
                }
            },
            _ => write!(
                f,
                "([!!] Unexpect token type in ValueToken: {:?})",
                self.token_type
            ),
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
pub enum DefaultValue {
    NullStringDefault(String), // The "null".
    ExpressionDefault(Box<Expression>),
}

#[derive(Serialize, Deserialize)]
pub struct IdentifierWithDefaultValueToken {
    token_type: TokenType, // Identifier.
    #[serde(alias = "Value")]
    value: String, // Identifier can only has String as its name.
    default_value: DefaultValue, // Only Identifier uses this?
}

impl IdentifierWithDefaultValueToken {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _indent: usize,
    ) -> std::fmt::Result {
        match self.token_type {
            TokenType::Identifier => {
                write!(f, "{} = ", self.value)?;
                match &self.default_value {
                    DefaultValue::ExpressionDefault(exp) => exp.fmt_indented(f, 0),
                    DefaultValue::NullStringDefault(_) => write!(f, "null"),
                }
            }
            _ => write!(
                f,
                "([!!] Unexpect token type in IdentifierWithMaybeDefaultValueToken: {:?})",
                self.token_type
            ),
        }
    }
}

// There is no concept of this in the raw mist.
#[derive(Serialize, Deserialize)]
#[serde(untagged)]
pub enum FunctionParameterToken {
    IdentifierOnly(ValueToken),
    IdentifierWithDefault(IdentifierWithDefaultValueToken),
}

impl FunctionParameterToken {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _indent: usize,
    ) -> std::fmt::Result {
        match self {
            FunctionParameterToken::IdentifierOnly(exp) => exp.fmt_indented(f, 0),
            FunctionParameterToken::IdentifierWithDefault(exp) => exp.fmt_indented(f, 0),
        }
    }
}
