use std::fmt;
use crate::tokens::{NewToken, Token, TokenType, ValueToken};
use serde::{Deserialize, Serialize};
use crate::tokens::Token::TypeOnlyToken;

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag = "expr_type")]
pub enum NewExpression {
    Literal { value: NewToken },
    Named { name: NewToken },
    Unary { operator: NewToken, right: Box<NewExpression> },
    Binary { operator: NewToken, left: Box<NewExpression>, right: Box<NewExpression> },
    Logical { operator: NewToken, left: Box<NewExpression>, right: Box<NewExpression> },
    // Technically, only Named Identifier should be allowed as name.
    Assign { name: Box<NewExpression>, value: Box<NewExpression> },
    // Technically, only Named Identifier should be allowed as call.
    Call { call: Box<NewExpression>, args: Vec<NewExpression> },
    Grouping { expr: Box<NewExpression> },
}

impl NewExpression {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}", current_indent)?;
        match self {
            NewExpression::Literal { value } => value.fmt_indented(f, indent),
            NewExpression::Named { name } => name.fmt_indented(f, indent),
            NewExpression::Unary { operator, right   } => {
                operator.fmt_indented(f, 0)?;
                right.fmt_indented(f, 0)
            }
            NewExpression::Binary { operator, left, right } => {
                left.fmt_indented(f, 0)?;
                write!(f, " ")?;
                operator.fmt_indented(f, 0)?;
                write!(f, " ")?;
                right.fmt_indented(f, 0)
            }
            NewExpression::Logical { operator, left, right } => {
                left.fmt_indented(f, 0)?;
                operator.fmt_indented(f, 0)?;
                right.fmt_indented(f, 0)
            }
            NewExpression::Assign { name, value } => {
                name.fmt_indented(f, 0)?;
                write!(f, " = ")?;
                value.fmt_indented(f, 0)
            }
            NewExpression::Call { call, args } => {
                call.fmt_indented(f, indent)?;
                write!(f, "(")?;
                for (index, arg) in args.iter().enumerate() {
                    arg.fmt_indented(f, 0)?;
                    if index != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            NewExpression::Grouping { expr } => {
                write!(f, "(")?;
                expr.fmt_indented(f, 0)?;
                write!(f, ")")
            }
        }
    }
}

impl fmt::Display for NewExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_indented(f, 0)
    }
}

#[test]
fn test_expressions_de() {
    // Literals: True, False, String, Number.
    let input = r#"{"expr_type": "Literal", "value": {"token_type": "True"}}"#;
    let result: NewExpression = serde_json::from_str(input).expect("failed to deserialize Expression::Literal (True)");
    assert_eq!(result, NewExpression::Literal { value: NewToken::True } );

    let input = r#"{"expr_type": "Literal", "value": {"token_type": "String", "value": "Hello Mistrian"}}"#;
    let result: NewExpression = serde_json::from_str(input).expect("failed to deserialize Expression::Literal (String)");
    assert_eq!(result, NewExpression::Literal { value: NewToken::String { value: "Hello Mistrian".to_owned() } } );

    let input = r#"{"expr_type": "Literal", "value": {"token_type": "Number", "Value": 0.341}}"#;
    let result: NewExpression = serde_json::from_str(input).expect("failed to deserialize Expression::Literal (String)");
    assert_eq!(result, NewExpression::Literal { value: NewToken::Number { Value: 0.341 } } );

    // Named.
    let input = r#"{ "expr_type": "Named", "name": { "token_type": "Identifier", "value": "__get_new_day_spawn_x" }}"#;
    let result: NewExpression = serde_json::from_str(input).expect("failed to deserialize Expression::Named");
    let token = NewToken::Identifier { value: "__get_new_day_spawn_x".to_string(), default_value: None };
    assert_eq!(result, NewExpression::Named { name: token } );

    // Unary.
    let input = r#"
{
    "expr_type": "Unary",
    "operator": {"token_type": "Minus"},
    "right": {
        "expr_type": "Literal",
        "value": {"token_type": "Number", "Value": 1.0}
    }
}
    "#;
    let result: NewExpression = serde_json::from_str(input).expect("failed to deserialize Expression::Unary");
    let right_expr = NewExpression::Literal { value: NewToken::Number { Value: 1.0 } };
    assert_eq!(result, NewExpression::Unary {operator: NewToken::Minus, right: Box::from(right_expr) });

    // Binary.
    let input = r#"
{
    "expr_type": "Binary",
    "left": {
        "expr_type": "Literal",
        "value": {"token_type": "Number", "Value": 3.0}
    },
    "operator": {"token_type": "DoubleEqual"},
    "right": {
        "expr_type": "Literal",
        "value": {"token_type": "Number", "Value": 0.0}
    }
}
    "#;
    let result: NewExpression = serde_json::from_str(input).expect("failed to deserialize Expression::Binary");
    let left_expr = NewExpression::Literal { value: NewToken::Number { Value: 3.0 } };
    let right_expr = NewExpression::Literal { value: NewToken::Number { Value: 0.0 } };
    assert_eq!(result, NewExpression::Binary {operator: NewToken::DoubleEqual, left: Box::from(left_expr), right: Box::from(right_expr) });

    // Logical.
    let input  = r#"
{
    "expr_type": "Logical",
    "left": {
        "expr_type": "Binary",
        "left": {
            "expr_type": "Named",
            "name": {
                "token_type": "Identifier",
                "value": "ari_health"
            }
        },
        "operator": {"token_type": "DoubleEqual"},
        "right": {
            "expr_type": "Literal",
            "value": {"token_type": "Number", "Value": 0.0}
        }
    },
    "operator": {"token_type": "And"},
    "right": {
        "expr_type": "Binary",
        "left": {
            "expr_type": "Literal",
            "value": {"token_type": "False"}
        },
        "operator": {"token_type": "DoubleEqual"},
        "right": {
            "expr_type": "Literal",
            "value": {"token_type": "True"}
        }
    }
}
    "#;
    let result: NewExpression = serde_json::from_str(input).expect("failed to deserialize Expression::Logical");
    let left_expr = NewExpression::Binary {
        operator: NewToken::DoubleEqual,
        left: Box::from(NewExpression::Named { name: NewToken::Identifier { value: "ari_health".to_string(), default_value: None } }),
        right: Box::from(NewExpression::Literal {value: NewToken::Number { Value: 0.0 } }),
    };
    let right_expr = NewExpression::Binary {
        operator: NewToken::DoubleEqual,
        left: Box::from(NewExpression::Literal {value: NewToken::False }),
        right: Box::from(NewExpression::Literal {value: NewToken::True }),
    };
    assert_eq!(result, NewExpression::Logical {operator: NewToken::And, left: Box::from(left_expr), right: Box::from(right_expr) });

    // Assign.
    let input = r#"
{
    "expr_type": "Assign",
    "name": {
        "expr_type": "Named",
        "name": {"token_type": "Identifier", "value": "lemonade"}
    },
    "value": {
        "expr_type": "Literal",
        "value": {"token_type": "True"}
    }
}
    "#;
    let result: NewExpression = serde_json::from_str(input).expect("failed to deserialize Expression::Assign");
    assert_eq!(result, NewExpression::Assign {
        name: Box::from(NewExpression::Named { name: NewToken::Identifier { value: "lemonade".to_string(), default_value: None } }),
        value: Box::from(NewExpression::Literal { value: NewToken::True })
    });

    // Call.
    let input = r#"
{
    "expr_type": "Call",
    "call": {
        "expr_type": "Named",
        "name": {
            "token_type": "Identifier",
            "value": "get_response"
        }
    },
    "args": []
}
    "#;
    let result: NewExpression = serde_json::from_str(input).expect("failed to deserialize Expression::Call");
    assert_eq!(result, NewExpression::Call {
        call: Box::from(NewExpression::Named { name: NewToken::Identifier { value: "get_response".to_string(), default_value: None } }),
        args: vec!(),
    });

    let input = r#"
{
    "expr_type": "Grouping",
    "expr": {
        "expr_type": "Binary",
        "left": {
            "expr_type": "Named",
            "name": {
                "token_type": "Identifier",
                "value": "target_time"
            }
        },
        "operator": {"token_type": "Minus"},
        "right": {
            "expr_type": "Named",
            "name": {"token_type": "Identifier", "value": "start"}
        }
    }
}
    "#;
    let result: NewExpression = serde_json::from_str(input).expect("failed to deserialize Expression::Grouping");
    let left_expr = NewExpression::Named { name: NewToken::Identifier { value: "target_time".to_string(), default_value: None } };
    let right_expr = NewExpression::Named { name: NewToken::Identifier { value: "start".to_string(), default_value: None} };
    assert_eq!(result, NewExpression::Grouping {
        expr: Box::from(NewExpression::Binary { operator: NewToken::Minus, left: Box::from(left_expr), right: Box::from(right_expr) }),
    });
}

#[test]
fn test_expression_format_human() {
    // Literals.
    let input= NewExpression::Literal { value: NewToken::True };
    let ir = r#"{"expr_type": "Literal", "value": {"token_type": "True"}}"#;
    assert_eq!(format!("{}", input), "true");

    // Unary.
    let input = NewExpression::Unary {operator: NewToken::Minus, right: Box::from(NewExpression::Literal { value: NewToken::Number { Value: 1.3 } }) };
    assert_eq!(format!("{}", input), "-1.3");

    // Assign.
    let input = NewExpression::Assign {
        name: Box::from(NewExpression::Named { name: NewToken::Identifier { value: "lemonade".to_string(), default_value: None } }),
        value: Box::from(NewExpression::Literal { value: NewToken::True })
    };
    assert_eq!(format!("{}", input), "lemonade = true");

    // Call.
    let input = NewExpression::Call {
        call: Box::from(NewExpression::Named { name: NewToken::Identifier { value: "get_response".to_string(), default_value: None } }),
        args: vec!(),
    };
    assert_eq!(format!("{}", input), "get_response()");

    let input = NewExpression::Call {
        call: Box::from(NewExpression::Named { name: NewToken::Identifier { value: "scene".to_string(), default_value: None } }),
        args: vec!(NewExpression::Named { name: NewToken::Identifier { value: "farm".to_string(), default_value: None } }),
    };
    assert_eq!(format!("{}", input), "scene(farm)");

    let input = NewExpression::Call {
        call: Box::from(NewExpression::Named { name: NewToken::Identifier { value: "walk_slow".to_string(), default_value: None } }),
        args: vec!(
            NewExpression::Named { name: NewToken::Identifier { value: "eiland".to_string(), default_value: None } },
            NewExpression::Named { name: NewToken::Identifier { value: "dz_eiland_journal".to_string(), default_value: None } }
        ),
    };
    assert_eq!(format!("{}", input), "walk_slow(eiland, dz_eiland_journal)");

    // Grouping.
    let input = NewExpression::Grouping { expr: Box::from(NewExpression::Binary {
        operator: NewToken::Minus,
        left: Box::from(NewExpression::Named { name: NewToken::Identifier { value: "target_time".to_string(), default_value: None } }),
        right: Box::from(NewExpression::Named { name: NewToken::Identifier { value: "start".to_string(), default_value: None } }),
    })};
    assert_eq!(format!("{}", input), "(target_time - start)");
}

#[derive(Serialize, Deserialize)]
pub enum ExpressionType {
    Call,
    Named,
    Literal,
    Unary,
    Binary,
    Logical,
    Assign,
    Grouping,
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
pub enum Expression {
    Call(CallExpression),
    Named(NamedExpression),
    Literal(LiteralExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression), // We are using this for Logical as well
    Assign(AssignExpression),
    Grouping(GroupingExpression),
}

impl Expression {

    pub(crate) fn new_boolean_literal(val: bool) -> Self {
        Expression::Literal(LiteralExpression { expr_type: ExpressionType::Literal, value: Token::new_boolean(val) })
    }

    pub(crate) fn new_number_literal(val: f64) -> Self {
        Expression::Literal(LiteralExpression { expr_type: ExpressionType::Literal, value: Token::new_number(val) })
    }

    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Expression::Call(exp) => exp.fmt_indented(f, indent),
            Expression::Named(exp) => exp.fmt_indented(f, indent),
            Expression::Literal(exp) => exp.fmt_indented(f, indent),
            Expression::Binary(exp) => exp.fmt_indented(f, indent),
            Expression::Unary(exp) => exp.fmt_indented(f, indent),
            Expression::Assign(exp) => exp.fmt_indented(f, indent),
            Expression::Grouping(exp) => exp.fmt_indented(f, indent),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct CallExpression {
    expr_type: ExpressionType, // Call
    call: NamedExpression,
    args: Vec<Expression>,
}

impl CallExpression {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.call.fmt_indented(f, indent)?;
        write!(f, "(")?;
        for (index, arg) in self.args.iter().enumerate() {
            arg.fmt_indented(f, 0)?;
            if index != self.args.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct NamedExpression {
    expr_type: ExpressionType, // Named
    name: ValueToken,
}

impl NamedExpression {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}", current_indent)?;
        self.name.fmt_indented(f, indent)
    }
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LiteralExpression {
    expr_type: ExpressionType, // Literal
    pub value: Token,
}

impl LiteralExpression {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}", current_indent)?;
        self.value.fmt_indented(f, indent)
    }
}

#[derive(Serialize, Deserialize)]
// Without this, BinaryExpression will ended up as UnaryExpression
#[serde(deny_unknown_fields)]
pub struct UnaryExpression {
    expr_type: ExpressionType, // Unary
    operator: Token,
    right: Box<Expression>,
}

impl UnaryExpression {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _indent: usize,
    ) -> std::fmt::Result {
        match &self.operator {
            Token::TypeOnlyToken(t) => match t.token_type {
                TokenType::Minus => {
                    t.fmt_indented(f, 0)?;
                    self.right.fmt_indented(f, 0)
                }
                TokenType::Bang => {
                    t.fmt_indented(f, 0)?;
                    self.right.fmt_indented(f, 0)
                }
                _ => write!(
                    f,
                    "([!!] Unknown operator for a UnaryExpression. Found {:?}",
                    t.token_type
                ),
            },
            Token::ValueToken(_) => write!(
                f,
                "([!!] ValueToken found as operator for the UnaryExpression."
            ),
            Token::IdentifierWithDefaultValueToken(_) => write!(
                f,
                "([!!] IdentifierWithDefaultValueToken found as operator for the UnaryExpression."
            ),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct BinaryExpression {
    expr_type: ExpressionType, // Binary
    left: Box<Expression>,
    operator: Token,
    right: Box<Expression>,
}

impl BinaryExpression {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _indent: usize,
    ) -> std::fmt::Result {
        self.left.fmt_indented(f, 0)?;
        write!(f, " ")?;

        match &self.operator {
            Token::TypeOnlyToken(t) => match t.token_type {
                TokenType::DoubleEqual => t.fmt_indented(f, 0)?,
                TokenType::BangEqual => t.fmt_indented(f, 0)?,
                TokenType::LessEqual => t.fmt_indented(f, 0)?,
                TokenType::Less => t.fmt_indented(f, 0)?,
                TokenType::GreaterEqual => t.fmt_indented(f, 0)?,
                TokenType::Greater => t.fmt_indented(f, 0)?,
                TokenType::And => t.fmt_indented(f, 0)?,
                TokenType::Plus => t.fmt_indented(f, 0)?,
                TokenType::Minus => t.fmt_indented(f, 0)?,
                TokenType::Star => t.fmt_indented(f, 0)?,
                TokenType::Slash => t.fmt_indented(f, 0)?,
                _ => write!(
                    f,
                    "([!!] Unknown operator for a BinaryExpression. Found {:?}",
                    t.token_type
                )?,
            },
            Token::ValueToken(_) => write!(
                f,
                "([!!] ValueToken found as operator for the BinaryExpression."
            )?,
            Token::IdentifierWithDefaultValueToken(_) => write!(
                f,
                "([!!] IdentifierWithDefaultValueToken found as operator for the BinaryExpression."
            )?,
        };
        write!(f, " ")?;
        self.right.fmt_indented(f, 0)
    }
}

#[derive(Serialize, Deserialize)]
pub struct AssignExpression {
    expr_type: ExpressionType, // Assign
    name: NamedExpression,
    value: Box<Expression>,
}

impl AssignExpression {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}", current_indent)?;
        self.name.fmt_indented(f, 0)?;
        write!(f, " = ")?;
        self.value.fmt_indented(f, 0)
    }
}

#[derive(Serialize, Deserialize)]
pub struct GroupingExpression {
    expr_type: ExpressionType, // Grouping
    expr: Box<Expression>,
}

impl GroupingExpression {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}(", current_indent)?;
        self.expr.fmt_indented(f, 0)?;
        write!(f, ")")
    }
}
