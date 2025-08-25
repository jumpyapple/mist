use crate::tokens::{Token, TokenType, ValueToken};
use serde::{Deserialize, Serialize};

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
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);

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
        self.call.fmt_indented(f, indent);
        write!(f, "(");
        for (index, arg) in self.args.iter().enumerate() {
            arg.fmt_indented(f, 0);
            if index != self.args.len() - 1 {
                write!(f, ", ");
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
        write!(f, "{}", current_indent);
        self.name.fmt_indented(f, indent)
    }
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LiteralExpression {
    expr_type: ExpressionType, // Literal
    value: Token,
}

impl LiteralExpression {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}", current_indent);
        self.value.fmt_indented(f, indent)
    }
}

#[derive(Serialize, Deserialize)]
// Without this, BinaryExpression will ended up as UnaryExpression
#[serde(deny_unknown_fields)]
pub struct UnaryExpression {
    expr_type: ExpressionType, // Unary
    operator: Token,
    right: Box<Expression>, // TODO: Other variant may show up?
}

impl UnaryExpression {
    pub(crate) fn fmt_indented(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match &self.operator {
            Token::TypeOnlyToken(t) => match t.token_type {
                TokenType::Minus => {
                    t.fmt_indented(f, 0);
                    self.right.fmt_indented(f, 0)
                }
                TokenType::Bang => {
                    t.fmt_indented(f, 0);
                    self.right.fmt_indented(f, 0)
                }
                _ => write!(
                    f,
                    "([!!] Unknown operator for a UnaryExpression. Found {:?}",
                    t.token_type
                ),
            },
            Token::ValueToken(t) => write!(
                f,
                "([!!] ValueToken found as operator for the UnaryExpression."
            ),
            Token::IdentifierWithDefaultValueToken(t) =>write!(
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
        indent: usize,
    ) -> std::fmt::Result {
        match &self.operator {
            Token::TypeOnlyToken(t) => match t.token_type {
                TokenType::DoubleEqual => {
                    self.left.fmt_indented(f, 0);
                    write!(f, " ");
                    t.fmt_indented(f, 0);
                    write!(f, " ");
                    self.right.fmt_indented(f, 0)
                }
                TokenType::BangEqual => {
                    self.left.fmt_indented(f, 0);
                    write!(f, " ");
                    t.fmt_indented(f, 0);
                    write!(f, " ");
                    self.right.fmt_indented(f, 0)
                }
                TokenType::LessEqual => {
                    self.left.fmt_indented(f, 0);
                    write!(f, " ");
                    t.fmt_indented(f, 0);
                    write!(f, " ");
                    self.right.fmt_indented(f, 0)
                }
                TokenType::GreaterEqual => {
                    self.left.fmt_indented(f, 0);
                    write!(f, " ");
                    t.fmt_indented(f, 0);
                    write!(f, " ");
                    self.right.fmt_indented(f, 0)
                }
                TokenType::And => {
                    self.left.fmt_indented(f, 0);
                    write!(f, " ");
                    t.fmt_indented(f, 0);
                    write!(f, " ");
                    self.right.fmt_indented(f, 0)
                }
                TokenType::Plus => {
                    self.left.fmt_indented(f, 0);
                    write!(f, " ");
                    t.fmt_indented(f, 0);
                    write!(f, " ");
                    self.right.fmt_indented(f, 0)
                }
                TokenType::Minus => {
                    self.left.fmt_indented(f, 0);
                    write!(f, " ");
                    t.fmt_indented(f, 0);
                    write!(f, " ");
                    self.right.fmt_indented(f, 0)
                }
                TokenType::Star => {
                    self.left.fmt_indented(f, 0);
                    write!(f, " ");
                    t.fmt_indented(f, 0);
                    write!(f, " ");
                    self.right.fmt_indented(f, 0)
                }
                TokenType::Slash => {
                    self.left.fmt_indented(f, 0);
                    write!(f, " ");
                    t.fmt_indented(f, 0);
                    write!(f, " ");
                    self.right.fmt_indented(f, 0)
                }
                _ => write!(
                    f,
                    "([!!] Unknown operator for a BinaryExpression. Found {:?}",
                    t.token_type
                ),
            },
            Token::ValueToken(t) => write!(
                f,
                "([!!] ValueToken found as operator for the BinaryExpression."
            ),
            Token::IdentifierWithDefaultValueToken(t) => write!(
                f,
                "([!!] IdentifierWithDefaultValueToken found as operator for the BinaryExpression."
            ),
        }
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
        write!(f, "{}", current_indent);
        self.name.fmt_indented(f, 0);
        write!(f, " = ");
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
        write!(f, "{}(", current_indent);
        self.expr.fmt_indented(f, 0);
        write!(f, ")")
    }
}
