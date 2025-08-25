use serde::{Deserialize, Deserializer, Serialize};
use serde::de::{Error, IntoDeserializer};
use crate::expressions::Expression;

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
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);

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
    token_type: TokenType, // Identifier (without default_value), Number, String.
    #[serde(alias = "Value")]
    value: Value,
}

impl ValueToken {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);

        match self.token_type {
            TokenType::Identifier => {
                match &self.value {
                    Value::StringValue(val) => write!(f, "{}", val),
                    Value::NumberValue(val) => {
                        write!(f, "([!!] An Identifier has a number as its name: {})", val)
                    }
                }
            }
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
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);

        match self.token_type {
            TokenType::Identifier => {
                write!(f, "{} = ", self.value);
                match &self.default_value {
                    DefaultValue::ExpressionDefault(exp) => exp.fmt_indented(f, 0),
                    DefaultValue::NullStringDefault(val) => write!(f, "null"),
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
    IdentifierWithDefault(IdentifierWithDefaultValueToken)
}

impl FunctionParameterToken {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);

        match self {
            FunctionParameterToken::IdentifierOnly(exp) => exp.fmt_indented(f, 0),
            FunctionParameterToken::IdentifierWithDefault(exp) => exp.fmt_indented(f, 0),
        }
    }
}