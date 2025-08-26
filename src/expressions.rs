use crate::tokens::NewToken;
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag = "expr_type")]
pub enum NewExpression {
    Literal {
        value: NewToken,
    },
    Named {
        name: NewToken,
    },
    Unary {
        operator: NewToken,
        right: Box<NewExpression>,
    },
    Binary {
        operator: NewToken,
        left: Box<NewExpression>,
        right: Box<NewExpression>,
    },
    Logical {
        operator: NewToken,
        left: Box<NewExpression>,
        right: Box<NewExpression>,
    },
    // Technically, only Named Identifier should be allowed as name.
    Assign {
        name: Box<NewExpression>,
        value: Box<NewExpression>,
    },
    // Technically, only Named Identifier should be allowed as call.
    Call {
        call: Box<NewExpression>,
        args: Vec<NewExpression>,
    },
    Grouping {
        expr: Box<NewExpression>,
    },
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
            NewExpression::Unary { operator, right } => {
                operator.fmt_indented(f, 0)?;
                right.fmt_indented(f, 0)
            }
            NewExpression::Binary {
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
            NewExpression::Logical {
                operator,
                left,
                right,
            } => {
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
fn test_expression_de() {
    // Literals: True, False, String, Number.
    let input = r#"{"expr_type": "Literal", "value": {"token_type": "True"}}"#;
    let result: NewExpression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Literal (True)");
    assert_eq!(
        result,
        NewExpression::Literal {
            value: NewToken::True
        }
    );

    let input =
        r#"{"expr_type": "Literal", "value": {"token_type": "String", "value": "Hello Mistrian"}}"#;
    let result: NewExpression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Literal (String)");
    assert_eq!(
        result,
        NewExpression::Literal {
            value: NewToken::String {
                value: "Hello Mistrian".to_owned()
            }
        }
    );

    let input = r#"{"expr_type": "Literal", "value": {"token_type": "Number", "Value": 0.341}}"#;
    let result: NewExpression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Literal (String)");
    assert_eq!(
        result,
        NewExpression::Literal {
            value: NewToken::Number { Value: 0.341 }
        }
    );

    // Named.
    let input = r#"{ "expr_type": "Named", "name": { "token_type": "Identifier", "value": "__get_new_day_spawn_x" }}"#;
    let result: NewExpression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Named");
    let token = NewToken::Identifier {
        value: "__get_new_day_spawn_x".to_string(),
        default_value: None,
    };
    assert_eq!(result, NewExpression::Named { name: token });

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
    let result: NewExpression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Unary");
    let right_expr = NewExpression::Literal {
        value: NewToken::Number { Value: 1.0 },
    };
    assert_eq!(
        result,
        NewExpression::Unary {
            operator: NewToken::Minus,
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
    let result: NewExpression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Binary");
    let left_expr = NewExpression::Literal {
        value: NewToken::Number { Value: 3.0 },
    };
    let right_expr = NewExpression::Literal {
        value: NewToken::Number { Value: 0.0 },
    };
    assert_eq!(
        result,
        NewExpression::Binary {
            operator: NewToken::DoubleEqual,
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
    let result: NewExpression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Logical");
    let left_expr = NewExpression::Binary {
        operator: NewToken::DoubleEqual,
        left: Box::from(NewExpression::Named {
            name: NewToken::Identifier {
                value: "ari_health".to_string(),
                default_value: None,
            },
        }),
        right: Box::from(NewExpression::Literal {
            value: NewToken::Number { Value: 0.0 },
        }),
    };
    let right_expr = NewExpression::Binary {
        operator: NewToken::DoubleEqual,
        left: Box::from(NewExpression::Literal {
            value: NewToken::False,
        }),
        right: Box::from(NewExpression::Literal {
            value: NewToken::True,
        }),
    };
    assert_eq!(
        result,
        NewExpression::Logical {
            operator: NewToken::And,
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
    let result: NewExpression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Assign");
    assert_eq!(
        result,
        NewExpression::Assign {
            name: Box::from(NewExpression::Named {
                name: NewToken::Identifier {
                    value: "lemonade".to_string(),
                    default_value: None
                }
            }),
            value: Box::from(NewExpression::Literal {
                value: NewToken::True
            })
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
    let result: NewExpression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Call");
    assert_eq!(
        result,
        NewExpression::Call {
            call: Box::from(NewExpression::Named {
                name: NewToken::Identifier {
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
    let result: NewExpression =
        serde_json::from_str(input).expect("failed to deserialize Expression::Grouping");
    let left_expr = NewExpression::Named {
        name: NewToken::Identifier {
            value: "target_time".to_string(),
            default_value: None,
        },
    };
    let right_expr = NewExpression::Named {
        name: NewToken::Identifier {
            value: "start".to_string(),
            default_value: None,
        },
    };
    assert_eq!(
        result,
        NewExpression::Grouping {
            expr: Box::from(NewExpression::Binary {
                operator: NewToken::Minus,
                left: Box::from(left_expr),
                right: Box::from(right_expr)
            }),
        }
    );
}

#[test]
fn test_expression_format_human() {
    // Literals.
    let input = NewExpression::Literal {
        value: NewToken::True,
    };
    let ir = r#"{"expr_type": "Literal", "value": {"token_type": "True"}}"#;
    assert_eq!(format!("{}", input), "true");

    // Unary.
    let input = NewExpression::Unary {
        operator: NewToken::Minus,
        right: Box::from(NewExpression::Literal {
            value: NewToken::Number { Value: 1.3 },
        }),
    };
    assert_eq!(format!("{}", input), "-1.3");

    // Assign.
    let input = NewExpression::Assign {
        name: Box::from(NewExpression::Named {
            name: NewToken::Identifier {
                value: "lemonade".to_string(),
                default_value: None,
            },
        }),
        value: Box::from(NewExpression::Literal {
            value: NewToken::True,
        }),
    };
    assert_eq!(format!("{}", input), "lemonade = true");

    // Call.
    let input = NewExpression::Call {
        call: Box::from(NewExpression::Named {
            name: NewToken::Identifier {
                value: "get_response".to_string(),
                default_value: None,
            },
        }),
        args: vec![],
    };
    assert_eq!(format!("{}", input), "get_response()");

    let input = NewExpression::Call {
        call: Box::from(NewExpression::Named {
            name: NewToken::Identifier {
                value: "scene".to_string(),
                default_value: None,
            },
        }),
        args: vec![NewExpression::Named {
            name: NewToken::Identifier {
                value: "farm".to_string(),
                default_value: None,
            },
        }],
    };
    assert_eq!(format!("{}", input), "scene(farm)");

    let input = NewExpression::Call {
        call: Box::from(NewExpression::Named {
            name: NewToken::Identifier {
                value: "walk_slow".to_string(),
                default_value: None,
            },
        }),
        args: vec![
            NewExpression::Named {
                name: NewToken::Identifier {
                    value: "eiland".to_string(),
                    default_value: None,
                },
            },
            NewExpression::Named {
                name: NewToken::Identifier {
                    value: "dz_eiland_journal".to_string(),
                    default_value: None,
                },
            },
        ],
    };
    assert_eq!(format!("{}", input), "walk_slow(eiland, dz_eiland_journal)");

    // Grouping.
    let input = NewExpression::Grouping {
        expr: Box::from(NewExpression::Binary {
            operator: NewToken::Minus,
            left: Box::from(NewExpression::Named {
                name: NewToken::Identifier {
                    value: "target_time".to_string(),
                    default_value: None,
                },
            }),
            right: Box::from(NewExpression::Named {
                name: NewToken::Identifier {
                    value: "start".to_string(),
                    default_value: None,
                },
            }),
        }),
    };
    assert_eq!(format!("{}", input), "(target_time - start)");
}
