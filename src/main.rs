use std::collections::HashMap;
use std::io;
use std::fs;
use std::env;

use serde::de::Visitor;
use serde::{Deserialize, Serialize};
use serde_json::Result;

#[derive(Serialize, Deserialize)]
struct MistsContainer {
    #[serde(rename = "day_zero.mist")]
    programs: Vec<Statement>
}

#[derive(Serialize, Deserialize)]
enum TokenType {
    #[serde(rename = "Identifier")]
    Identifier,
    #[serde(rename = "Number")]
    Number,
    #[serde(rename = "String")]
    String,
    #[serde(rename = "True")]
    BooleanTrue,
    #[serde(rename = "False")]
    BooleanFalse,
    #[serde(rename = "DoubleEqual")]
    DoubleEqual,
    #[serde(rename = "Minus")]
    Minus,
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum Token {
    Identifier(IdentifierToken),
    Number(NumberToken),
    String(StringToken),
    Boolean(BooleanToken),
    DoubleEqual(DoubleEqualToken),
    Minus(MinusToken),
}

#[derive(Serialize, Deserialize)]
enum StatementType {
    #[serde(rename = "Block")]
    Block,
    #[serde(rename = "Expr")]
    Expr,
    #[serde(rename = "Function")]
    Function,
    #[serde(rename = "Var")]
    Var,
    #[serde(rename = "Simultaneous")]
    Simultaneous,
    #[serde(rename = "Free")]
    Free,
    #[serde(rename = "If")]
    If
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum Statement {
    Block(BlockStatement),
    Expr(ExpressionStatement),
    Function(FunctionStatement),
    Var(VarStatement),
    Simultaneous(SimultaneousStatement),
    Free(FreeStatement),
    If(IfStatement),
}

#[derive(Serialize, Deserialize)]
enum ExpressionType {
    #[serde(rename = "Call")]
    Call,
    #[serde(rename = "Named")]
    Named,
    #[serde(rename = "Literal")]
    Literal,
    #[serde(rename = "Unary")]
    Unary,
    #[serde(rename = "Binary")]
    Binary,
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum Expression {
    Call(CallExpression),
    Named(NamedExpression),
    Literal(LiteralExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
}



#[derive(Serialize, Deserialize)]
struct IdentifierToken {
    token_type: TokenType, // Identifier
    value: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    default_value: Option<String>
}

#[derive(Serialize, Deserialize)]
struct NumberToken {
    token_type: TokenType, // Number
    value: f64
}

#[derive(Serialize, Deserialize)]
struct StringToken {
    token_type: TokenType, // String
    value: String
}

#[derive(Serialize, Deserialize)]
struct BooleanToken {
    token_type: TokenType, // True or False
}

#[derive(Serialize, Deserialize)]
struct DoubleEqualToken {
    token_type: TokenType, // DoubleEqual (==)
}

#[derive(Serialize, Deserialize)]
struct MinusToken {
    token_type: TokenType, // Minus (-)
}


#[derive(Serialize, Deserialize)]
struct VarStatement {
    stmt_type: StatementType, // Var
    name: IdentifierToken,
    initializer: Expression,
}


#[derive(Serialize, Deserialize)]
struct FunctionStatement {
    stmt_type: StatementType, // Function
    name: IdentifierToken,
    params: Vec<IdentifierToken>,
    body: BlockStatement,
    resolve: String
}


#[derive(Serialize, Deserialize)]
struct BlockStatement {
    stmt_type: StatementType, // Block
    stmts: Vec<Statement>
}

#[derive(Serialize, Deserialize)]
struct SimultaneousStatement {
    stmt_type: StatementType, // Simultaneous
    body: BlockStatement
}

#[derive(Serialize, Deserialize)]
struct FreeStatement {
    stmt_type: StatementType, // Free
    stmt: Box<Statement>
}

#[derive(Serialize, Deserialize)]
struct IfStatement {
    stmt_type: StatementType, // If
    condition: Expression,
    then_branch: BlockStatement,
    // TODO: Double check if not having else_branch is possible.
    else_branch: Option<BlockStatement>
}

#[derive(Serialize, Deserialize)]
struct ExpressionStatement {
    stmt_type: StatementType, // Expr
    expr: Expression
}

#[derive(Serialize, Deserialize)]
struct CallExpression {
    expr_type: ExpressionType, // Call
    call: NamedExpression,
    args: Vec<Expression>
}

#[derive(Serialize, Deserialize)]
struct NamedExpression {
    expr_type: ExpressionType, // Named
    name: IdentifierToken
}

#[derive(Serialize, Deserialize)]
struct LiteralExpression {
    expr_type: ExpressionType, // Literal
    value: Token
}

#[derive(Serialize, Deserialize)]
struct UnaryExpression {
    expr_type: ExpressionType, // Unary
    operator: Token,
    right: Box<Expression> // TODO: Other variant may show up?
}

#[derive(Serialize, Deserialize)]
struct BinaryExpression {
    expr_type: ExpressionType, // Binary
    left: Box<Expression>,
    operator: Token,
    right: Box<Expression>
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2{
        println!("usage: {} <mist-file-path>", args[0]);
        return;
    }

    let file_path = args[1].to_string();
    let result = fs::read_to_string(file_path);
    match result {
        Ok(data) => {
            let result: Result<MistsContainer> = serde_json::from_str(data.as_str());
            if result.is_ok() {
                println!("Successfully parsed 'day_zero.mist.json'");
            } else {
                println!("Error: Failed to parse file 'day_zero.mist.json': {:?}", result.err());
            }
            
        },
        Err(error) => println!("Error: Failed to read file 'day_zero.mist.json': {}", error),
    };
}
