use core::fmt;
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

impl fmt::Display for MistsContainer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.programs {
            stmt.fmt_indented(f, 0);
            match stmt {
                Statement::Block(s) => write!(f, "")?,
                Statement::Expr(s) => write!(f, ";\n")?,
                Statement::Function(s) => write!(f, "\n")?,
                Statement::Var(s) => write!(f, "\n")?,
                Statement::Simultaneous(s) => write!(f, "\n")?,
                Statement::Free(s) => write!(f, ";\n")?,
                Statement::If(s) => write!(f, "\n")?,
            }
        }
        Ok(())
    }
}

#[derive(Serialize, Deserialize, Debug)]
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

impl Statement {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        
        match self {
            Statement::Block(stmt) => stmt.fmt_indented(f, indent),
            Statement::Expr(stmt) => stmt.fmt_indented(f, indent),
            Statement::Function(stmt) => stmt.fmt_indented(f, indent),
            Statement::Var(stmt) => stmt.fmt_indented(f, indent),
            Statement::Simultaneous(stmt) => stmt.fmt_indented(f, indent),
            Statement::Free(stmt) => stmt.fmt_indented(f, indent),
            Statement::If(stmt) => stmt.fmt_indented(f, indent),
        }
    }
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

impl Expression {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        
        match self {
            Expression::Call(exp) => exp.fmt_indented(f, indent),
            Expression::Named(exp) => exp.fmt_indented(f, indent),
            Expression::Literal(exp) => exp.fmt_indented(f, indent),
            Expression::Unary(exp) => exp.fmt_indented(f, indent),
            Expression::Binary(exp) => exp.fmt_indented(f, indent),
        }
    }
}


#[derive(Serialize, Deserialize)]
struct IdentifierToken {
    token_type: TokenType, // Identifier
    value: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    default_value: Option<String>
}

impl IdentifierToken {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        
        match &self.default_value {
            None => write!(f, "{}", self.value),
            Some(val) => write!(f, "{} = {}", self.value, val)
        }
    }
}

#[derive(Serialize, Deserialize)]
struct NumberToken {
    token_type: TokenType, // Number
    #[serde(rename = "Value")] // That sneaky little upper case!
    value: f64
}

impl NumberToken {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, _indent: usize) -> std::fmt::Result {
        write!(f, "what happejlaskjf{:.2}", self.value)
    }
}

#[derive(Serialize, Deserialize)]
struct StringToken {
    token_type: TokenType, // String
    value: String
}

impl StringToken {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, _indent: usize) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Serialize, Deserialize)]
struct BooleanToken {
    token_type: TokenType, // True or False
}

impl BooleanToken {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, _indent: usize) -> std::fmt::Result {
        match self.token_type {
            TokenType::BooleanTrue => write!(f, "true"),
            TokenType::BooleanFalse => write!(f, "false"),
            _ => write!(f, ""),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct DoubleEqualToken {
    token_type: TokenType, // DoubleEqual (==)
}

impl DoubleEqualToken {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, _indent: usize) -> std::fmt::Result {
        write!(f, "==")
    }
}

#[derive(Serialize, Deserialize)]
struct MinusToken {
    token_type: TokenType, // Minus (-)
}

impl MinusToken {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, _indent: usize) -> std::fmt::Result {
        write!(f, "-")
    }
}


#[derive(Serialize, Deserialize)]
struct VarStatement {
    stmt_type: StatementType, // Var
    name: IdentifierToken,
    initializer: Expression,
}

impl VarStatement {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}var ", current_indent);
        self.name.fmt_indented(f, 0);
        write!(f, " = ");
        self.initializer.fmt_indented(f, 0)
    }
}

#[derive(Serialize, Deserialize)]
struct FunctionStatement {
    stmt_type: StatementType, // Function
    name: IdentifierToken,
    params: Vec<IdentifierToken>,
    body: BlockStatement,
    resolve: String
}

impl FunctionStatement {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}function ", current_indent)?;
        self.name.fmt_indented(f, indent)?;
        write!(f, "(")?;
        self.params.iter().map(|item| item.fmt_indented(f, indent));
        writeln!(f, ")")?;
        self.body.fmt_indented(f, indent)
    }
}


#[derive(Serialize, Deserialize)]
struct BlockStatement {
    stmt_type: StatementType, // Block
    stmts: Vec<Statement>
}

impl BlockStatement {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        writeln!(f, "{}{{", current_indent);
        // self.stmts.iter().map(|item| item.fmt_indented(f, indent + 1));
        if self.stmts.len() == 0 {
            write!(f, "stmts has zero length!!!");
        }
        for stmt in &self.stmts {
            stmt.fmt_indented(f, indent + 1);
            match stmt {
                Statement::Expr(_) => { writeln!(f, ";"); }
                Statement::Var(_) => { writeln!(f, ";"); }
                Statement::Free(_) => { writeln!(f, ";"); }
                _ => {}
            }
        }
        writeln!(f, "{}}}", current_indent)
    }
}

#[derive(Serialize, Deserialize)]
struct SimultaneousStatement {
    stmt_type: StatementType, // Simultaneous
    body: BlockStatement
}

impl SimultaneousStatement {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        writeln!(f, "{}simultaneous ", current_indent);
        self.body.fmt_indented(f, indent)
    }
}

#[derive(Serialize, Deserialize)]
struct FreeStatement {
    stmt_type: StatementType, // Free
    stmt: Box<Statement>
}

impl FreeStatement {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}free ", current_indent);
        match self.stmt.as_ref() {
            Statement::Block(_) => { 
                writeln!(f, "{{");
                self.stmt.as_ref().fmt_indented(f, indent);
                writeln!(f, "}}")
            },
            _ => self.stmt.fmt_indented(f, 0),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct IfStatement {
    stmt_type: StatementType, // If
    condition: Expression,
    then_branch: BlockStatement,
    // TODO: Double check if not having else_branch is possible.
    else_branch: Option<BlockStatement>
}

impl IfStatement {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}if (", current_indent);
        self.condition.fmt_indented(f, 0);
        writeln!(f, ")");
        self.then_branch.fmt_indented(f, indent);
        if self.else_branch.is_some() {
            writeln!(f, "{}else", current_indent);
            self.else_branch.as_ref().unwrap().fmt_indented(f, indent);
        }
        Ok(())
    }
}

#[derive(Serialize, Deserialize)]
struct ExpressionStatement {
    stmt_type: StatementType, // Expr
    expr: Expression
}

impl ExpressionStatement {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        self.expr.fmt_indented(f, indent)
    }
}

#[derive(Serialize, Deserialize)]
struct CallExpression {
    expr_type: ExpressionType, // Call
    call: NamedExpression,
    args: Vec<Expression>
}

impl CallExpression {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        self.call.fmt_indented(f, indent);
        write!(f, "(");
        for (index, arg) in self.args.iter().enumerate() {
            arg.fmt_indented(f, 0);
            if index != self.args.len() -1 {
                write!(f, ", ");
            }
        }
        write!(f, ")")
    }
}

#[derive(Serialize, Deserialize)]
struct NamedExpression {
    expr_type: ExpressionType, // Named
    name: IdentifierToken
}

impl NamedExpression {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}{}", current_indent, self.name.value)
    }
}

#[derive(Serialize, Deserialize)]
struct LiteralExpression {
    expr_type: ExpressionType, // Literal
    value: Token
}

impl LiteralExpression {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}", current_indent);
        match &self.value {
            Token::Identifier(t) => write!(f, "{}", t.value),
            Token::Number(t) => write!(f, "{}", t.value),
            Token::String(t) => {
                write!(f, "\"");
                t.fmt_indented(f, indent);
                write!(f, "\"")
            },
            Token::Boolean(t) => match t.token_type {
                TokenType::BooleanTrue => write!(f, "true"),
                TokenType::BooleanFalse => write!(f, "false"),
                _ => write!(f, "Unknown boolean value. Expect BooleanTrue or BooleanFalse, but found {:?}", t.token_type),
            },
            Token::DoubleEqual(t) => write!(f, "=="),
            Token::Minus(t) => write!(f, "-"),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct UnaryExpression {
    expr_type: ExpressionType, // Unary
    operator: Token,
    right: Box<Expression> // TODO: Other variant may show up?
}

impl UnaryExpression {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        match &self.operator {
            Token::Minus(t) => {
                write!(f, "-");
                self.right.fmt_indented(f, indent)
            },
            _ => write!(f, "Unkown operator type. Expect Minus"),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct BinaryExpression {
    expr_type: ExpressionType, // Binary
    left: Box<Expression>,
    operator: Token,
    right: Box<Expression>
}

impl BinaryExpression {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        match &self.operator {
            Token::DoubleEqual(t) => {
                self.left.fmt_indented(f, indent);
                write!(f, "==");
                self.right.fmt_indented(f, indent)
            },
            _ => write!(f, "unkown token type (expected DoubleEqual)"),
        }
    }
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2{
        println!("usage: {} <mist-file-path>", args[0]);
        // return;
    }

    // let file_path = args[1].to_string();

    let file_path = "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Fields of Mistria\\day_zero.mist.json";
    let result = fs::read_to_string(file_path);
    match result {
        Ok(data) => {
            let result: Result<MistsContainer> = serde_json::from_str(data.as_str());
            if result.is_ok() {
                println!("Successfully parsed '{}'", file_path);
                println!("{}", result.unwrap());
            } else {
                println!("Error: Failed to parse file 'day_zero.mist.json': {:?}", result.err());
            }
            
        },
        Err(error) => println!("Error: Failed to read file 'day_zero.mist.json': {}", error),
    };
}
