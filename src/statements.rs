use crate::tokens::{
    FunctionParameterToken, IdentifierWithDefaultValueToken, TypeOnlyToken, ValueToken,
};

use crate::expressions::Expression;
use serde::de::IntoDeserializer;
use serde::de::Visitor;
use serde::{Deserialize, Deserializer, Serialize};

#[derive(Serialize, Deserialize)]
pub enum StatementType {
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
    If,
    #[serde(rename = "Return")]
    Return,
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
pub enum Statement {
    Block(BlockStatement),
    Expr(ExpressionStatement),
    Function(FunctionStatement),
    Var(VarStatement),
    Simultaneous(SimultaneousStatement),
    Free(FreeStatement),
    If(IfStatement),
    Return(ReturnStatement),
}

impl Statement {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);

        match self {
            Statement::Block(stmt) => stmt.fmt_indented(f, indent),
            Statement::Expr(stmt) => stmt.fmt_indented(f, indent),
            Statement::Function(stmt) => stmt.fmt_indented(f, indent),
            Statement::Var(stmt) => stmt.fmt_indented(f, indent),
            Statement::Simultaneous(stmt) => stmt.fmt_indented(f, indent),
            Statement::Free(stmt) => stmt.fmt_indented(f, indent),
            Statement::If(stmt) => stmt.fmt_indented(f, indent),
            Statement::Return(stmt) => stmt.fmt_indented(f, indent),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct VarStatement {
    stmt_type: StatementType, // Var
    name: ValueToken,
    initializer: Expression,
}

impl VarStatement {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}var ", current_indent);
        self.name.fmt_indented(f, 0);
        write!(f, " = ");
        self.initializer.fmt_indented(f, 0)
    }
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum FunctionResolveType {
    NullString(String), // The "null" string.
    BlockStatement(BlockStatement),
}

#[derive(Serialize, Deserialize)]
pub struct FunctionStatement {
    stmt_type: StatementType, // Function
    name: ValueToken,
    params: Vec<FunctionParameterToken>,
    body: BlockStatement,
    resolve: FunctionResolveType,
}

impl FunctionStatement {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}function ", current_indent)?;
        self.name.fmt_indented(f, indent)?;
        write!(f, "(")?;
        for (index, param) in self.params.iter().enumerate() {
            param.fmt_indented(f, 0);
            if index != &self.params.len() - 1 {
                write!(f, ", ")?;
            }
        }
        // self.params.iter().map(|item| item.fmt_indented(f, indent));
        writeln!(f, ")")?;
        self.body.fmt_indented(f, indent);

        // Not sure what syntax should look like.
        match &self.resolve {
            FunctionResolveType::NullString(_) => write!(f, ""),
            FunctionResolveType::BlockStatement(stmt) => {
                writeln!(f, "=>");
                stmt.fmt_indented(f, indent)
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct BlockStatement {
    stmt_type: StatementType, // Block
    stmts: Vec<Statement>,
}

impl BlockStatement {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        writeln!(f, "{}{{", current_indent);
        // self.stmts.iter().map(|item| item.fmt_indented(f, indent + 1));
        if self.stmts.len() == 0 {
            // E.g. an empty function that is used for its "resolve".
            // write!(f, "stmts has zero length!!!");
        }
        for stmt in &self.stmts {
            stmt.fmt_indented(f, indent + 1);
            match stmt {
                Statement::Expr(_) => {
                    writeln!(f, ";");
                }
                Statement::Var(_) => {
                    writeln!(f, ";");
                }
                Statement::Free(stmt) => {
                    if stmt.has_block_statement() {
                        write!(f, "");
                    } else {
                        write!(f, ";\n");
                    }
                }
                Statement::Return(_) => {
                    writeln!(f, ";");
                }
                _ => {}
            }
        }
        writeln!(f, "{}}}", current_indent)
    }
}

#[derive(Serialize, Deserialize)]
// Without this, serde will pick this instead of FunctionStatement
#[serde(deny_unknown_fields)]
pub struct SimultaneousStatement {
    stmt_type: StatementType, // Simultaneous
    body: BlockStatement,
}

impl SimultaneousStatement {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        writeln!(f, "{}simultaneous ", current_indent);
        self.body.fmt_indented(f, indent)
    }
}

#[derive(Serialize, Deserialize)]
pub struct FreeStatement {
    stmt_type: StatementType, // Free
    stmt: Box<Statement>,
}

impl FreeStatement {
    pub(crate) fn has_block_statement(&self) -> bool {
        match self.stmt.as_ref() {
            Statement::Block(_) => true,
            _ => false,
        }
    }

    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}free", current_indent);
        match self.stmt.as_ref() {
            Statement::Block(_) => {
                write!(f, "\n");
                self.stmt.as_ref().fmt_indented(f, indent)
            }
            _ => {
                write!(f, " ");
                self.stmt.fmt_indented(f, 0)
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct IfStatement {
    stmt_type: StatementType, // If
    condition: Expression,
    then_branch: Box<Statement>,
    // An empty else branch is possible, but it is marked as "null" instead of just JSON's null.
    #[serde(deserialize_with = "deserialize_else_branch")]
    else_branch: Option<Box<Statement>>,
}

fn deserialize_else_branch<'de, D>(
    deserializer: D,
) -> std::result::Result<Option<Box<Statement>>, D::Error>
where
    D: Deserializer<'de>,
{
    // We cant reuse deserializer multiple time, so we deserialize into generic JSON value first.
    let result: std::result::Result<serde_json::value::Value, <D as Deserializer>::Error> =
        Deserialize::deserialize(deserializer);

    match result {
        Ok(generic_json) => match generic_json {
            serde_json::Value::String(value) => {
                if value == "null" {
                    Ok(None)
                } else {
                    Err(serde::de::Error::custom(
                        "else_branch is a String, expect a Map or \"null\"",
                    ))
                }
            }
            serde_json::Value::Object(value) => {
                match Deserialize::deserialize(value.into_deserializer()) {
                    Ok(value) => Ok(Some(value)),
                    Err(error) => Err(serde::de::Error::custom(error.to_string())),
                }
            }
            _ => Err(serde::de::Error::custom(
                "else_branch is not a String \"null\" or a Map",
            )),
        },
        Err(error) => Err(serde::de::Error::custom(format!(
            "Error parsing else_branch: {}",
            error.to_string()
        ))),
    }
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
pub struct ReturnStatement {
    stmt_type: StatementType, // Return
    // Similar to the else_branch of the IfStatement, this can be "null".
    #[serde(deserialize_with = "deserialize_return_value")]
    value: Option<Expression>,
}

fn deserialize_return_value<'de, D>(
    deserializer: D,
) -> std::result::Result<Option<Expression>, D::Error>
where
    D: Deserializer<'de>,
{
    // We cant reuse deserializer multiple time, so we deserialize into generic JSON value first.
    let result: std::result::Result<serde_json::value::Value, <D as Deserializer>::Error> =
        Deserialize::deserialize(deserializer);

    match result {
        Ok(generic_json) => match generic_json {
            serde_json::Value::String(value) => {
                if value == "null" {
                    Ok(None)
                } else {
                    Err(serde::de::Error::custom(
                        "return's value is a String, expect a Map or \"null\"",
                    ))
                }
            }
            serde_json::Value::Object(value) => {
                match Deserialize::deserialize(value.into_deserializer()) {
                    Ok(value) => Ok(Some(value)),
                    Err(error) => Err(serde::de::Error::custom(error.to_string())),
                }
            }
            _ => Err(serde::de::Error::custom(
                "return's value is not a String \"null\" or a Map",
            )),
        },
        Err(error) => Err(serde::de::Error::custom(format!(
            "Error parsing return statement: {}",
            error.to_string()
        ))),
    }
}

impl ReturnStatement {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}return ", current_indent);
        if self.value.is_some() {
            self.value.as_ref().unwrap().fmt_indented(f, 0)
        } else {
            write!(f, "null")
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct ExpressionStatement {
    stmt_type: StatementType, // Expr
    expr: Expression,
}

impl ExpressionStatement {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.expr.fmt_indented(f, indent)
    }
}
