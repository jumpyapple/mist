use crate::tokens::Token;
use std::{fmt, fs};
use std::fmt::Formatter;

use crate::expressions::Expression;
use crate::statements::Optional::OptionalExpression;
use serde::de::IntoDeserializer;
use serde::{Deserialize, Deserializer, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum Optional {
    NullString(String), // The "null".
    OptionalStatement(Box<Statement>),
    OptionalExpression(Box<Expression>),
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
        resolve: Optional,
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
        else_branch: Optional,
    },
    Return {
        value: Optional,
    },
}

impl Statement {
    pub(crate) fn should_skip_semicolon(&self) -> bool {
        match self {
            Statement::Free { stmt } => match *(*stmt) {
                Statement::Block { .. } => true,
                _ => false,
            },
            Statement::Block { .. } => true,
            Statement::Simultaneous { .. } => true,
            Statement::If { .. } => true,
            _ => false,
        }
    }

    pub(crate) fn fmt_indented(&self, f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
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
                    // write!(f, "{}", " ".repeat((indent + 1) * 4))?;
                    stmt.fmt_indented(f, indent + 1)?;

                    // Handle ";\n" for every statement within the block.
                    // Except when it is another Block.
                    if !stmt.should_skip_semicolon() {
                        write!(f, ";\n")?;
                    }
                }
                writeln!(f, "{}}}", current_indent)
            }
            Statement::Expr { expr } => expr.fmt_indented(f, 0),
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
                match resolve {
                    Optional::NullString(_) => write!(f, ""),
                    Optional::OptionalStatement(stmt) => {
                        writeln!(f, "=>")?;
                        stmt.fmt_indented(f, indent)
                    }
                    _ => write!(f, "([!!] Did not expect OptionalExpression.)"),
                }
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

                match else_branch {
                    Optional::NullString(_) => Ok(()),
                    Optional::OptionalStatement(st) => {
                        writeln!(f, "{}else", current_indent)?;
                        st.fmt_indented(f, indent)
                    }
                    _ => write!(f, "([!!] Did not expect OptionalExpression.)"),
                }
            }
            Statement::Return { value } => {
                write!(f, "return ")?;
                match value {
                    Optional::NullString(_) => write!(f, "null"),
                    Optional::OptionalExpression(expr) => expr.fmt_indented(f, 0),
                    Optional::OptionalStatement(_) => {
                        write!(f, "([!!] Statement is used with Return)")
                    }
                }
                // ";\n" is handled by the Block.
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
                value: Optional::OptionalExpression(Box::from(Expression::Named {
                    name: Token::Identifier {
                        value: "target_time".to_string(),
                        default_value: None
                    }
                }))
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
            value: Optional::OptionalExpression(Box::from(
                Expression::Binary {
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
                }
            ))
        }
    );

    let input = r#"{ "stmt_type": "Return", "value": "null" }"#;
    let result: Statement =
        serde_json::from_str(input).expect("failed to deserialize Statement::Return with \"null\"");
    assert_eq!(
        result,
        Statement::Return {
            value: Optional::NullString("null".to_string())
        }
    );
}

#[test]
fn test_function_de_with_resolve_null() {
    let input = r#"
{
    "stmt_type": "Function",
    "name": {
        "token_type": "Identifier",
        "value": "magic_circle_sound"
    },
    "params": [],
    "body": {
        "stmt_type": "Block",
        "stmts": [
            {
                "stmt_type": "Expr",
                "expr": {
                    "expr_type": "Call",
                    "call": {
                        "expr_type": "Named",
                        "name": {
                            "token_type": "Identifier",
                            "value": "play_sound"
                        }
                    },
                    "args": [
                        {
                            "expr_type": "Literal",
                            "value": {
                                "token_type": "String",
                                "value": "SoundEffects/SpecialEvents/MagicCirclePulse"
                            }
                        }
                    ]
                }
            }
        ]
    },
    "resolve": "null"
}
"#;
    let func_name = Token::Identifier {
        value: "magic_circle_sound".to_string(),
        default_value: None,
    };
    let func_body = Statement::Block {
        stmts: vec![Statement::Expr {
            expr: Box::from(Expression::Call {
                call: Box::from(Expression::Named {
                    name: Token::Identifier {
                        value: "play_sound".to_string(),
                        default_value: None,
                    },
                }),
                args: vec![Expression::Literal {
                    value: Token::String {
                        value: "SoundEffects/SpecialEvents/MagicCirclePulse".to_string(),
                    },
                }],
            }),
        }],
    };
    let result: Statement = serde_json::from_str(input)
        .expect("failed to deserialize Statement::Function (resolve = \"null\")");
    assert_eq!(
        result,
        Statement::Function {
            name: func_name,
            params: vec![],
            body: Box::from(func_body),
            resolve: Optional::NullString("null".to_string()),
        }
    );
}

#[test]
fn test_function_de_with_resolve_statement() {
    let input = r#"
{
    "stmt_type": "Function",
    "name": {
        "token_type": "Identifier",
        "value": "magic_circle_sound"
    },
    "params": [],
    "body": {
        "stmt_type": "Block",
        "stmts": [
            {
                "stmt_type": "Expr",
                "expr": {
                    "expr_type": "Call",
                    "call": {
                        "expr_type": "Named",
                        "name": {
                            "token_type": "Identifier",
                            "value": "play_sound"
                        }
                    },
                    "args": [
                        {
                            "expr_type": "Literal",
                            "value": {
                                "token_type": "String",
                                "value": "SoundEffects/SpecialEvents/MagicCirclePulse"
                            }
                        }
                    ]
                }
            }
        ]
    },
    "resolve": {
        "stmt_type": "Block",
        "stmts": [
            {
                "stmt_type": "Return",
                "value": {
                    "expr_type": "Call",
                    "call": {
                        "expr_type": "Named",
                        "name": {
                            "token_type": "Identifier",
                            "value": "quest_is_active"
                        }
                    },
                    "args": [
                        {
                            "expr_type": "Literal",
                            "value": {
                                "token_type": "String",
                                "value": "greet_the_townsfolk"
                            }
                        }
                    ]
                }
            }
        ]
    }
}
"#;
    let func_name = Token::Identifier {
        value: "magic_circle_sound".to_string(),
        default_value: None,
    };
    let func_body = Statement::Block {
        stmts: vec![Statement::Expr {
            expr: Box::from(Expression::Call {
                call: Box::from(Expression::Named {
                    name: Token::Identifier {
                        value: "play_sound".to_string(),
                        default_value: None,
                    },
                }),
                args: vec![Expression::Literal {
                    value: Token::String {
                        value: "SoundEffects/SpecialEvents/MagicCirclePulse".to_string(),
                    },
                }],
            }),
        }],
    };
    let resolve_body = Statement::Block {
        stmts: vec![Statement::Return {
            value: OptionalExpression(Box::from(Expression::Call {
                call: Box::from(Expression::Named {
                    name: Token::Identifier {
                        value: "quest_is_active".to_string(),
                        default_value: None,
                    },
                }),
                args: vec![Expression::Literal {
                    value: Token::String {
                        value: "greet_the_townsfolk".to_string(),
                    },
                }],
            })),
        }],
    };
    let result: Statement = serde_json::from_str(input)
        .expect("failed to deserialize Statement::Function (resolve = \"null\")");
    assert_eq!(
        result,
        Statement::Function {
            name: func_name,
            params: vec![],
            body: Box::from(func_body),
            resolve: Optional::OptionalStatement(Box::from(resolve_body)),
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
                value: Optional::OptionalExpression(Box::from(
                    Expression::Literal {
                        value: Token::String {
                            value: "world!".to_string(),
                        },
                    },
                )),
            }],
        }),
        resolve: Optional::NullString("null".to_string()),
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
        else_branch: Optional::NullString("null".to_string()),
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
