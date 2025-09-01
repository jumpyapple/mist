use crate::tokens::{BinaryOperator, Token, UnaryOperator};
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag = "expr_type")]
pub enum Expression {
    Literal {
        value: Token,
    },
    Named {
        name: Token,
    },
    Unary {
        operator: UnaryOperator,
        right: Box<Expression>,
    },
    Binary {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Logical {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    // Technically, only Named Identifier should be allowed as name.
    Assign {
        name: Box<Expression>,
        value: Box<Expression>,
    },
    // Technically, only Named Identifier should be allowed as call.
    Call {
        call: Box<Expression>,
        args: Vec<Expression>,
    },
    Grouping {
        expr: Box<Expression>,
    },
}

impl Expression {
    pub(crate) fn fmt_indented(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}", current_indent)?;
        match self {
            Expression::Literal { value } => value.fmt_indented(f, indent),
            Expression::Named { name } => name.fmt_indented(f, indent),
            Expression::Unary { operator, right } => {
                operator.fmt_indented(f, 0)?;
                right.fmt_indented(f, 0)
            }
            Expression::Binary {
                operator,
                left,
                right,
            } => {
                left.fmt_indented(f, 0)?;
                write!(f, " ")?;
                operator.fmt_indented(f, 0)?;
                write!(f, " ")?;
                right.fmt_indented(f, 0)
            }
            Expression::Logical {
                operator,
                left,
                right,
            } => {
                left.fmt_indented(f, 0)?;
                operator.fmt_indented(f, 0)?;
                right.fmt_indented(f, 0)
            }
            Expression::Assign { name, value } => {
                name.fmt_indented(f, 0)?;
                write!(f, " = ")?;
                value.fmt_indented(f, 0)
            }
            Expression::Call { call, args } => {
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
            Expression::Grouping { expr } => {
                write!(f, "(")?;
                expr.fmt_indented(f, 0)?;
                write!(f, ")")
            }
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_indented(f, 0)
    }
}

#[test]
fn test_expression_de() {
    // Literals: True, False, String, Number.
    let input = r#"{"expr_type": "Literal", "value": {"token_type": "True"}}"#;
    let result: Expression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Literal (True)");
    assert_eq!(result, Expression::Literal { value: Token::True });

    let input =
        r#"{"expr_type": "Literal", "value": {"token_type": "String", "value": "Hello Mistrian"}}"#;
    let result: Expression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Literal (String)");
    assert_eq!(
        result,
        Expression::Literal {
            value: Token::String {
                value: "Hello Mistrian".to_owned()
            }
        }
    );

    let input = r#"{"expr_type": "Literal", "value": {"token_type": "Number", "Value": 0.341}}"#;
    let result: Expression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Literal (String)");
    assert_eq!(
        result,
        Expression::Literal {
            value: Token::Number { value: 0.341 }
        }
    );

    // Named.
    let input = r#"{ "expr_type": "Named", "name": { "token_type": "Identifier", "value": "__get_new_day_spawn_x" }}"#;
    let result: Expression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Named");
    let token = Token::Identifier {
        value: "__get_new_day_spawn_x".to_string(),
        default_value: None,
    };
    assert_eq!(result, Expression::Named { name: token });

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
    let result: Expression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Unary");
    let right_expr = Expression::Literal {
        value: Token::Number { value: 1.0 },
    };
    assert_eq!(
        result,
        Expression::Unary {
            operator: UnaryOperator::Minus,
            right: Box::from(right_expr)
        }
    );

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
    let result: Expression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Binary");
    let left_expr = Expression::Literal {
        value: Token::Number { value: 3.0 },
    };
    let right_expr = Expression::Literal {
        value: Token::Number { value: 0.0 },
    };
    assert_eq!(
        result,
        Expression::Binary {
            operator: BinaryOperator::DoubleEqual,
            left: Box::from(left_expr),
            right: Box::from(right_expr)
        }
    );

    // Logical.
    let input = r#"
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
    let result: Expression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Logical");
    let left_expr = Expression::Binary {
        operator: BinaryOperator::DoubleEqual,
        left: Box::from(Expression::Named {
            name: Token::Identifier {
                value: "ari_health".to_string(),
                default_value: None,
            },
        }),
        right: Box::from(Expression::Literal {
            value: Token::Number { value: 0.0 },
        }),
    };
    let right_expr = Expression::Binary {
        operator: BinaryOperator::DoubleEqual,
        left: Box::from(Expression::Literal {
            value: Token::False,
        }),
        right: Box::from(Expression::Literal { value: Token::True }),
    };
    assert_eq!(
        result,
        Expression::Logical {
            operator: BinaryOperator::And,
            left: Box::from(left_expr),
            right: Box::from(right_expr)
        }
    );

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
    let result: Expression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Assign");
    assert_eq!(
        result,
        Expression::Assign {
            name: Box::from(Expression::Named {
                name: Token::Identifier {
                    value: "lemonade".to_string(),
                    default_value: None
                }
            }),
            value: Box::from(Expression::Literal { value: Token::True })
        }
    );

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
    let result: Expression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Call");
    assert_eq!(
        result,
        Expression::Call {
            call: Box::from(Expression::Named {
                name: Token::Identifier {
                    value: "get_response".to_string(),
                    default_value: None
                }
            }),
            args: vec!(),
        }
    );

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
    let result: Expression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Grouping");
    let left_expr = Expression::Named {
        name: Token::Identifier {
            value: "target_time".to_string(),
            default_value: None,
        },
    };
    let right_expr = Expression::Named {
        name: Token::Identifier {
            value: "start".to_string(),
            default_value: None,
        },
    };
    assert_eq!(
        result,
        Expression::Grouping {
            expr: Box::from(Expression::Binary {
                operator: BinaryOperator::Minus,
                left: Box::from(left_expr),
                right: Box::from(right_expr)
            }),
        }
    );
}

#[test]
fn test_expression_format_human() {
    // Literals.
    let input = Expression::Literal { value: Token::True };
    assert_eq!(format!("{}", input), "true");

    // Unary.
    let input = Expression::Unary {
        operator: UnaryOperator::Minus,
        right: Box::from(Expression::Literal {
            value: Token::Number { value: 1.3 },
        }),
    };
    assert_eq!(format!("{}", input), "-1.3");

    // Assign.
    let input = Expression::Assign {
        name: Box::from(Expression::Named {
            name: Token::Identifier {
                value: "lemonade".to_string(),
                default_value: None,
            },
        }),
        value: Box::from(Expression::Literal { value: Token::True }),
    };
    assert_eq!(format!("{}", input), "lemonade = true");

    // Call.
    let input = Expression::Call {
        call: Box::from(Expression::Named {
            name: Token::Identifier {
                value: "get_response".to_string(),
                default_value: None,
            },
        }),
        args: vec![],
    };
    assert_eq!(format!("{}", input), "get_response()");

    let input = Expression::Call {
        call: Box::from(Expression::Named {
            name: Token::Identifier {
                value: "scene".to_string(),
                default_value: None,
            },
        }),
        args: vec![Expression::Named {
            name: Token::Identifier {
                value: "farm".to_string(),
                default_value: None,
            },
        }],
    };
    assert_eq!(format!("{}", input), "scene(farm)");

    let input = Expression::Call {
        call: Box::from(Expression::Named {
            name: Token::Identifier {
                value: "walk_slow".to_string(),
                default_value: None,
            },
        }),
        args: vec![
            Expression::Named {
                name: Token::Identifier {
                    value: "eiland".to_string(),
                    default_value: None,
                },
            },
            Expression::Named {
                name: Token::Identifier {
                    value: "dz_eiland_journal".to_string(),
                    default_value: None,
                },
            },
        ],
    };
    assert_eq!(format!("{}", input), "walk_slow(eiland, dz_eiland_journal)");

    // Grouping.
    let input = Expression::Grouping {
        expr: Box::from(Expression::Binary {
            operator: BinaryOperator::Minus,
            left: Box::from(Expression::Named {
                name: Token::Identifier {
                    value: "target_time".to_string(),
                    default_value: None,
                },
            }),
            right: Box::from(Expression::Named {
                name: Token::Identifier {
                    value: "start".to_string(),
                    default_value: None,
                },
            }),
        }),
    };
    assert_eq!(format!("{}", input), "(target_time - start)");
}
