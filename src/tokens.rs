use serde::{Deserialize, Serialize};

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
    LessEqual,
    GreaterEqual,
    Plus,
    Minus,
    Star,
    Slash,
    And,
}

// This is not a direct map.
#[derive(Serialize, Deserialize)]
#[serde(untagged)]
pub enum Token {
    // Identifier (with default_value), Number (with capital V in value), String (with lowercase V in value).
    ValueToken(ValueToken),
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
        }
    }
}

#[derive(Serialize, Deserialize)]
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
            TokenType::LessEqual => write!(f, "<="),
            TokenType::GreaterEqual => write!(f, ">="),
            TokenType::Plus => write!(f, "+"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Star => write!(f, "*"),
            TokenType::Slash => write!(f, "/"),
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
pub struct ValueToken {
    token_type: TokenType, // Identifier, Number, String.
    #[serde(alias = "Value")]
    value: Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    default_value: Option<String>,
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
                        write!(f, "([!!] An Identifer has a number as its name: {})", val)
                    }
                };

                if self.default_value.is_some() {
                    write!(f, " = {}", self.default_value.as_ref().unwrap());
                }
                Ok(())
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
