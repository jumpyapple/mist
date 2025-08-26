use crate::tokens::Token;
use std::fmt;
use std::fmt::Formatter;

use crate::expressions::Expression;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum OptionalStatement {
    #[serde(rename = "null")]
    NullString, // The "null".
    BlockStatement(Box<Statement>),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag = "stmt_type")]
pub enum Statement {
    Block {
        stmts: Vec<Statement>,
    },
    Expr {
        expr: Box<Expression>,
    },
    Function {
        name: Token,
        params: Vec<Token>,
        body: Box<Statement>,
        resolve: Option<OptionalStatement>,
    },
    Var {
        name: Token,
        initializer: Expression,
    },
    Simultaneous {
        body: Box<Statement>,
    },
    Free {
        stmt: Box<Statement>,
    },
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<OptionalStatement>,
    },
    Return {
        value: Option<Expression>,
    },
}

impl Statement {
    pub(crate) fn free_has_block_statement(&self) -> bool {
        match self {
            Statement::Free { stmt } => match *(*stmt) {
                Statement::Block { .. } => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub(crate) fn fmt_indented(
        &self,
        f: &mut Formatter<'_>,
        indent: usize,
    ) -> fmt::Result {
        let current_indent = " ".repeat(indent * 4);

        write!(f, "{}", current_indent)?;

        match self {
            Statement::Block { stmts } => {
                writeln!(f, "{{")?;
                if stmts.len() == 0 {
                    // E.g. an empty function that is used for its "resolve".
                    // write!(f, "stmts has zero length!!!");
                }
                for stmt in stmts {
                    // Handle the indentation and reset the indent level.
                    write!(f, "{}", " ".repeat((indent + 1) * 4))?;
                    stmt.fmt_indented(f, 0)?;

                    // Handle ";\n" for every statement within the block.
                    // Except when it is another Block.
                    if !stmt.free_has_block_statement() {
                        write!(f, ";\n")?;
                    }
                }
                writeln!(f, "{}}}", current_indent)
            }
            Statement::Expr { expr } => expr.fmt_indented(f, indent),
            Statement::Function {
                name,
                params,
                body,
                resolve,
            } => {
                write!(f, "function ")?;
                name.fmt_indented(f, indent)?;

                write!(f, "(")?;
                for (index, param) in params.iter().enumerate() {
                    param.fmt_indented(f, 0)?;
                    if index != &params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                writeln!(f, ")")?;

                body.fmt_indented(f, indent)?;

                // Not sure what syntax should look like.
                if let Some(r) = resolve.as_ref() {
                    match r {
                        OptionalStatement::NullString => write!(f, ""),
                        OptionalStatement::BlockStatement(stmt) => {
                            writeln!(f, "=>")?;
                            stmt.fmt_indented(f, indent)
                        }
                    }?;
                }
                Ok(())
            }
            Statement::Var { name, initializer } => {
                write!(f, "var ")?;
                name.fmt_indented(f, 0)?;
                write!(f, " = ")?;
                initializer.fmt_indented(f, 0)
                // a Var statement in a block will get its ;\m handled by the block.
                // However, the top level container needs to handle the ";\n" line end.
            }
            Statement::Simultaneous { body } => {
                writeln!(f, "simultaneous")?;
                body.fmt_indented(f, indent)
            }
            Statement::Free { stmt } => {
                write!(f, "free")?;
                match stmt.as_ref() {
                    Statement::Block { .. } => {
                        write!(f, "\n")?;
                        stmt.as_ref().fmt_indented(f, indent)
                    }
                    _ => {
                        write!(f, " ")?;
                        stmt.fmt_indented(f, 0)
                    }
                }
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(f, "if (")?;
                condition.fmt_indented(f, 0)?;
                writeln!(f, ")")?;
                then_branch.fmt_indented(f, indent)?;
                if let Some(opt_st) = else_branch {
                    match opt_st {
                        OptionalStatement::NullString => (),
                        OptionalStatement::BlockStatement(st) => {
                            writeln!(f, "{}else", current_indent)?;
                            st.fmt_indented(f, indent)?;
                        }
                    }
                }

                Ok(())
            }
            Statement::Return { value } => {
                write!(f, "return ")?;
                if let Some(v) = value {
                    v.fmt_indented(f, 0)
                    // ";\n" is handled by the Block.
                } else {
                    write!(f, "null")
                }
            }
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_indented(f, 0)
    }
}

#[test]
fn test_statement_de() {
    // Blocks.
    let input = r#"{"stmt_type": "Block", "stmts": []}"#;
    let result: Statement =
        serde_json::from_str(input).expect("failed to deserialize Statement::Block (empty)");
    assert_eq!(result, Statement::Block { stmts: vec!() });

    let input = r#"
{
    "stmt_type": "Block",
    "stmts": [
        {"stmt_type": "Return", "value": { "expr_type": "Named", "name": { "token_type": "Identifier", "value": "target_time" } }}
    ]
}
    "#;
    let result: Statement =
        serde_json::from_str(input).expect("failed to deserialize Statement::Block (one return)");
    assert_eq!(
        result,
        Statement::Block {
            stmts: vec!(Statement::Return {
                value: Some(Expression::Named {
                    name: Token::Identifier {
                        value: "target_time".to_string(),
                        default_value: None
                    }
                })
            })
        }
    );

    // Return.
    let input = r#"
{
    "stmt_type": "Return",
    "value": {
        "expr_type": "Binary",
        "left": {
            "expr_type": "Binary",
            "left": {
                "expr_type": "Named",
                "name": {"token_type": "Identifier", "value": "start"}
            },
            "operator": {"token_type": "Plus"},
            "right": {
                "expr_type": "Named",
                "name": {"token_type": "Identifier", "value": "timer"}
            }
        },
        "operator": {"token_type": "DoubleEqual"},
        "right": {
            "expr_type": "Named",
            "name": {
                "token_type": "Identifier",
                "value": "target_time"
            }
        }
    }
}
    "#;
    let result: Statement = serde_json::from_str(input)
        .expect("failed to deserialize Statement::Return start + timer == target_time");
    assert_eq!(
        result,
        Statement::Return {
            value: Some(Expression::Binary {
                operator: Token::DoubleEqual,
                left: Box::from(Expression::Binary {
                    operator: Token::Plus,
                    left: Box::from(Expression::Named {
                        name: Token::Identifier {
                            value: "start".to_string(),
                            default_value: None
                        }
                    }),
                    right: Box::from(Expression::Named {
                        name: Token::Identifier {
                            value: "timer".to_string(),
                            default_value: None
                        }
                    }),
                }),
                right: Box::from(Expression::Named {
                    name: Token::Identifier {
                        value: "target_time".to_string(),
                        default_value: None
                    }
                }),
            })
        }
    );
}

#[test]
fn test_statement_format_human() {
    // Function.
    let input = Statement::Function {
        name: Token::Identifier {
            value: "hello".to_string(),
            default_value: None,
        },
        params: vec![],
        body: Box::from(Statement::Block {
            stmts: vec![Statement::Return {
                value: Some(Expression::Literal {
                    value: Token::String {
                        value: "world!".to_string(),
                    },
                }),
            }],
        }),
        resolve: None,
    };
    assert_eq!(
        format!("{}", input),
        r#"function hello()
{
    return "world!";
}
"#
    );

    // Var.
    let var_stmt = Statement::Var {
        name: Token::Identifier {
            value: "timer".to_string(),
            default_value: None,
        },
        initializer: Expression::Binary {
            operator: Token::Star,
            left: Box::from(Expression::Named {
                name: Token::Identifier {
                    value: "seconds".to_string(),
                    default_value: None,
                },
            }),
            right: Box::from(Expression::Literal {
                value: Token::Number { value: 60.0 },
            }),
        },
    };
    // This one is at the top-level, so no ";\n" is expected.
    assert_eq!(format!("{}", var_stmt), r#"var timer = seconds * 60"#);

    let input = Statement::Block {
        stmts: vec![var_stmt],
    };
    // This is Var in a Block, so ";\n" is expected.
    assert_eq!(
        format!("{}", input),
        r#"{
    var timer = seconds * 60;
}
"#
    );

    // Simultaneous.
    let input = Statement::Simultaneous {
        body: Box::from(Statement::Block {
            stmts: vec![Statement::Expr {
                expr: Box::from(Expression::Call {
                    call: Box::from(Expression::Named {
                        name: Token::Identifier {
                            value: "animate".to_string(),
                            default_value: None,
                        },
                    }),
                    args: vec![
                        Expression::Literal {
                            value: Token::Identifier {
                                value: "adeline".to_string(),
                                default_value: None,
                            },
                        },
                        Expression::Literal {
                            value: Token::Identifier {
                                value: "idle".to_string(),
                                default_value: None,
                            },
                        },
                    ],
                }),
            }],
        }),
    };
    assert_eq!(
        format!("{}", input),
        r#"simultaneous
{
    animate(adeline, idle);
}
"#
    );

    // Free.
    let input = Statement::Free {
        stmt: Box::from(Statement::Expr {
            expr: Box::from(Expression::Call {
                call: Box::from(Expression::Named {
                    name: Token::Identifier {
                        value: "bark_speech".to_string(),
                        default_value: None,
                    },
                }),
                args: vec![
                    Expression::Literal {
                        value: Token::Identifier {
                            value: "adeline".to_string(),
                            default_value: None,
                        },
                    },
                    Expression::Literal {
                        value: Token::Identifier {
                            value: "exclamation_mark".to_string(),
                            default_value: None,
                        },
                    },
                ],
            }),
        }),
    };
    assert_eq!(
        format!("{}", input),
        "free bark_speech(adeline, exclamation_mark)"
    );

    // If.
    let input = Statement::If {
        condition: Expression::Binary {
            operator: Token::DoubleEqual,
            left: Box::from(Expression::Call {
                call: Box::from(Expression::Named {
                    name: Token::Identifier {
                        value: "get_response".to_string(),
                        default_value: None,
                    },
                }),
                args: vec![],
            }),
            right: Box::from(Expression::Literal {
                value: Token::Number { value: 0.0 },
            }),
        },
        then_branch: Box::from(Statement::Block {
            stmts: vec![Statement::Expr {
                expr: Box::from(Expression::Call {
                    call: Box::from(Expression::Named {
                        name: Token::Identifier {
                            value: "wait".to_string(),
                            default_value: None,
                        },
                    }),
                    args: vec![Expression::Literal {
                        value: Token::Number { value: 1.0 },
                    }],
                }),
            }],
        }),
        else_branch: None,
    };
    assert_eq!(
        format!("{}", input),
        r#"if (get_response() == 0)
{
    wait(1);
}
"#
    );
}
