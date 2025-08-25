use core::fmt;
use std::collections::HashMap;
use std::io;
use std::fs;
use std::env;

use serde::de::Visitor;
use serde::de::IntoDeserializer;
use serde::{Deserialize, Serialize, Deserializer, de};
use serde_json::Result;

#[derive(Serialize, Deserialize)]
#[serde(transparent)]
struct MistContainer {
    mists: HashMap<String, Mist>
}

impl MistContainer {
    fn render_mist_as_human_readable(&self, key: &str) -> Option<String> {
        let res = self.mists.get(key);
        match res {
            Some(mist) => Some(format!("{}", mist)),
            None => None
        }
    }
}

#[derive(Serialize, Deserialize)]
// #[serde(transparent)]
struct Mist {
    #[serde(alias = "adeline_eight_hearts.mist", alias = "adeline_four_hearts.mist", alias = "adeline_quest_board.mist", alias = "adeline_six_hearts.mist", alias = "adeline_two_hearts.mist", alias = "animal_festival_upcoming_eligible_year_one.mist", alias = "animal_festival_upcoming_eligible_year_two.mist", alias = "animal_festival_upcoming_ineligible_year_one.mist", alias = "animal_festival_upcoming_ineligible_year_two.mist", alias = "balor_eight_hearts.mist", alias = "balor_four_hearts.mist", alias = "balor_six_hearts.mist", alias = "balor_two_hearts.mist", alias = "bathhouse.mist", alias = "bathhouse_date.mist", alias = "beach_date.mist", alias = "beach_secret.mist", alias = "break_earth_seal.mist", alias = "break_fire_seal.mist", alias = "break_water_seal.mist", alias = "caldarus_eight_hearts.mist", alias = "caldarus_recovery.mist", alias = "celine_eight_hearts.mist", alias = "celine_four_hearts.mist", alias = "celine_six_hearts.mist", alias = "celine_two_hearts.mist", alias = "cop_some_ore.mist", alias = "crafting.mist", alias = "crafting_tutorial.mist", alias = "cutscene_basement.mist", alias = "darcy_two_hearts.mist", alias = "day_zero.mist", alias = "deep_woods_picnic_date.mist", alias = "delivering_the_sealing_scroll.mist", alias = "destroy_vines_eastern_road.mist", alias = "do_a_bro_a_favor.mist", alias = "dying.mist", alias = "earth_seal.mist", alias = "eiland_eight_hearts.mist", alias = "eiland_four_hearts.mist", alias = "eiland_six_hearts.mist", alias = "eiland_two_hearts.mist", alias = "elsie_dating_tutorial.mist", alias = "elsie_two_hearts.mist", alias = "errol_two_hearts.mist", alias = "farm_introduction.mist", alias = "find_the_weathervane.mist", alias = "find_the_weathervane_setup.mist", alias = "fire_seal.mist", alias = "full_restore.mist", alias = "gem_cutting_date.mist", alias = "harvest_festival_dance.mist", alias = "harvest_festival_first_place.mist", alias = "harvest_festival_morning.mist", alias = "harvest_festival_no_place.mist", alias = "harvest_festival_second_place.mist", alias = "harvest_festival_setup.mist", alias = "harvest_festival_third_place.mist", alias = "hayden_eight_hearts.mist", alias = "hayden_four_hearts.mist", alias = "hayden_six_hearts.mist", alias = "hayden_two_hearts.mist", alias = "hemlock_two_hearts.mist", alias = "inn_meal_date.mist", alias = "jos_cooking_class.mist", alias = "josephine_two_hearts.mist", alias = "juniper_eight_hearts.mist", alias = "juniper_four_hearts.mist", alias = "juniper_six_hearts.mist", alias = "juniper_two_hearts.mist", alias = "luc_two_hearts.mist", alias = "maple_two_hearts.mist", alias = "march_eight_hearts.mist", alias = "march_four_hearts.mist", alias = "march_six_hearts.mist", alias = "march_two_hearts.mist", alias = "museum_donation_wanted.mist", alias = "narrows_secret.mist", alias = "nora_two_hearts.mist", alias = "olric_two_hearts.mist", alias = "park_date.mist", alias = "pet_arrival.mist", alias = "pet_dream.mist", alias = "post_earth_seal.mist", alias = "post_water_seal.mist", alias = "procuring_the_sealing_scroll.mist", alias = "prologue.mist", alias = "reina_eight_hearts.mist", alias = "reina_four_hearts.mist", alias = "reina_six_hearts.mist", alias = "reina_two_hearts.mist", alias = "repair_haydens_barn_pt_1.mist", alias = "repair_haydens_barn_pt_2.mist", alias = "repair_the_beach_bridge_pt_1.mist", alias = "repair_the_beach_bridge_pt_2.mist", alias = "repair_the_bridge_pt_1.mist", alias = "repair_the_bridge_pt_2.mist", alias = "repair_the_general_store_pt_1.mist", alias = "repair_the_general_store_pt_2.mist", alias = "repair_the_inn_pt_1.mist", alias = "repair_the_inn_pt_2.mist", alias = "repair_the_mill_pt_1.mist", alias = "repair_the_mill_pt_2.mist", alias = "repair_the_summit_stairs.mist", alias = "replenishing_mistrias_food_reserves_1.mist", alias = "replenishing_mistrias_food_reserves_2_pt_1.mist", alias = "replenishing_mistrias_food_reserves_2_pt_2.mist", alias = "reveal_mistmare.mist", alias = "ruins_seal.mist", alias = "ryis_eight_hearts.mist", alias = "ryis_four_hearts.mist", alias = "ryis_six_hearts.mist", alias = "ryis_two_hearts.mist", alias = "shooting_star_adeline.mist", alias = "shooting_star_balor.mist", alias = "shooting_star_blocked.mist", alias = "shooting_star_caldarus.mist", alias = "shooting_star_celine.mist", alias = "shooting_star_date.mist", alias = "shooting_star_eiland.mist", alias = "shooting_star_hayden.mist", alias = "shooting_star_juniper.mist", alias = "shooting_star_march.mist", alias = "shooting_star_morning.mist", alias = "shooting_star_reina.mist", alias = "shooting_star_romantic.mist", alias = "shooting_star_ryis.mist", alias = "shooting_star_solo.mist", alias = "shooting_star_valen.mist", alias = "somethings_bugging_me.mist", alias = "spell_learned.mist", alias = "spring_festival_first_place.mist", alias = "spring_festival_first_place_plus.mist", alias = "spring_festival_morning.mist", alias = "spring_festival_no_place.mist", alias = "spring_festival_second_place.mist", alias = "spring_festival_setup.mist", alias = "spring_festival_setup_year_2.mist", alias = "spring_festival_third_place.mist", alias = "std.mist", alias = "stillwell_two_hearts.mist", alias = "stinky_stamina_potion.mist", alias = "stone_refinery_pt_1.mist", alias = "stone_refinery_pt_2.mist", alias = "taliferro_two_hearts.mist", alias = "tea_with_hayden.mist", alias = "terithia_two_hearts.mist", alias = "the_animal_festival.mist", alias = "the_unusual_tree_pt_1.mist", alias = "the_unusual_tree_pt_2.mist", alias = "translating_the_earth_tablet.mist", alias = "translating_the_fire_tablet.mist", alias = "translating_the_water_tablet.mist", alias = "unit_test.mist", alias = "unlocking_the_mines.mist", alias = "unlocking_the_mines_pt_1.mist", alias = "unlocking_the_mines_pt_2.mist", alias = "upgrade_the_carpenters_shop_pt_1.mist", alias = "upgrade_the_carpenters_shop_pt_2.mist", alias = "upgrade_the_saturday_market.mist", alias = "valen_eight_hearts.mist", alias = "valen_four_hearts.mist", alias = "valen_six_hearts.mist", alias = "valen_two_hearts.mist", alias = "water_seal.mist", alias = "woodcrafting_essentials.mist")]
    statements: Vec<Statement>
}

impl fmt::Display for Mist {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
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
    #[serde(rename = "LessEqual")]
    LessEqual,
    #[serde(rename = "Minus")]
    Minus,
    #[serde(rename = "And")]
    And,
}

// This is not a direct map.
#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum Token {
    // Identifier (with default_value), Number (with capital V in value), String (with lowercase V in value).
    ValueToken(ValueToken),
    // BooleanTrue, BooleanFalse, Minus, DoubleEqual.
    // NOTE: If this is first, serde seem to take the lazy route and pick it
    TypeOnlyToken(TypeOnlyToken),
}

impl Token {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        
        match self {
            Token::TypeOnlyToken(t) => t.fmt_indented(f, indent),
            Token::ValueToken(t) => t.fmt_indented(f, indent),
        }
    }
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
    #[serde(rename = "Logical")]
    Logical,
    #[serde(rename = "Assign")]
    Assign,
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum Expression {
    Call(CallExpression),
    Named(NamedExpression),
    Literal(LiteralExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression), // We are using this for Logical as well
    Assign(AssignExpression),
}

impl Expression {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        
        match self {
            Expression::Call(exp) => exp.fmt_indented(f, indent),
            Expression::Named(exp) => exp.fmt_indented(f, indent),
            Expression::Literal(exp) => exp.fmt_indented(f, indent),
            Expression::Binary(exp) => exp.fmt_indented(f, indent),
            Expression::Unary(exp) => exp.fmt_indented(f, indent),
            Expression::Assign(exp) => exp.fmt_indented(f, indent),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct TypeOnlyToken {
    token_type: TokenType, // BooleanTrue, BooleanFalse, DoubleEqual, Minus
}

impl TypeOnlyToken {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, _indent: usize) -> std::fmt::Result {
        match self.token_type {
            TokenType::BooleanTrue => write!(f, "true"),
            TokenType::BooleanFalse => write!(f, "false"),
            TokenType::DoubleEqual => write!(f, "=="),
            TokenType::LessEqual => write!(f, "<="),
            TokenType::Minus => write!(f, "-"),
            TokenType::And => write!(f, "&&"),
            _ => write!(f, "([!!] Unkown type for TypeOnlyToken. found {:?})", self.token_type),
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum Value {
    StringValue(String), // Could be Identifier's name or a String value
    NumberValue(f64),
}

#[derive(Serialize, Deserialize)]
struct ValueToken {
    token_type: TokenType, // Identifier, Number, String.
    #[serde(alias = "Value")]
    value: Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    default_value: Option<String>
}

impl ValueToken {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);

        match self.token_type {
            TokenType::Identifier => {
                match &self.value {
                    Value::StringValue(val) => write!(f, "{}", val),
                    Value::NumberValue(val) => write!(f, "([!!] An Identifer has a number as its name: {})", val),
                };

                if self.default_value.is_some() {
                    write!(f, " = {}", self.default_value.as_ref().unwrap());
                }
                Ok(())
            }
            TokenType::Number => {
                match &self.value {
                    Value::StringValue(val) => write!(f, "([!!] A NumberValue has a string as its value: {})", val),
                    Value::NumberValue(val) => write!(f, "{:.2}", val),
                }
            },
            TokenType::String => {
                match &self.value {
                    Value::StringValue(val) => write!(f, "\"{}\"", val),
                    Value::NumberValue(val) => write!(f, "([!!] A StringValue has a number as its value: {})", val),
                }
            },
            _ => write!(f, "([!!] Unexpect token type in ValueToken: {:?})", self.token_type)
        }
    }
}


#[derive(Serialize, Deserialize)]
struct VarStatement {
    stmt_type: StatementType, // Var
    name: ValueToken,
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
    name: ValueToken,
    params: Vec<ValueToken>,
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
        write!(f, "{}free", current_indent);
        match self.stmt.as_ref() {
            Statement::Block(_) => { 
                write!(f, "\n");
                self.stmt.as_ref().fmt_indented(f, indent)
            },
            _ => {
                write!(f, " ");
                self.stmt.fmt_indented(f, 0)
            },
        }
    }
}

#[derive(Serialize, Deserialize)]
struct IfStatement {
    stmt_type: StatementType, // If
    condition: Expression,
    then_branch: Box<Statement>,
    // An empty else branch is possible, but it is marked as "null" instead of just JSON's null. 
    #[serde(deserialize_with="deserialize_else_branch")]
    else_branch: Option<Box<Statement>>
}

fn deserialize_else_branch<'de, D>(deserializer: D) -> std::result::Result<Option<Box<Statement>>, D::Error>
where
    D: Deserializer<'de>,
{
    // We cant reuse deserializer multiple time, so we deserialize into generic JSON value first.
    let result: std::result::Result<serde_json::value::Value, <D as Deserializer>::Error> = Deserialize::deserialize(deserializer);

    match result {
        Ok(generic_json) => match generic_json {
                serde_json::Value::String(value) => if value == "null" {
                    Ok(None)
                } else {
                    Err(serde::de::Error::custom("else_branch is a String, expect a Map or \"null\""))
                },
                serde_json::Value::Object(value) => match Deserialize::deserialize(value.into_deserializer()) {
                    Ok(value) => Ok(Some(value)),
                    Err(error) => Err(serde::de::Error::custom(error.to_string())),
                },
                _ => Err(serde::de::Error::custom("else_branch is not a String \"null\" or a Map"))
            },
        Err(error) => {
            println!("Error in the first attempt: {}", error.to_string());
            Err(serde::de::Error::custom(format!("else_branch is error: {}", error.to_string())))
        }
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
#[serde(deny_unknown_fields)]
struct NamedExpression {
    expr_type: ExpressionType, // Named
    name: ValueToken
}

impl NamedExpression {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}", current_indent);
        self.name.fmt_indented(f, indent)
    }
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct LiteralExpression {
    expr_type: ExpressionType, // Literal
    value: Token
}

impl LiteralExpression {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}", current_indent);
        self.value.fmt_indented(f, indent)
    }
}

#[derive(Serialize, Deserialize)]
// Without this, BinaryExpression will ended up as UnaryExpression
#[serde(deny_unknown_fields)]
struct UnaryExpression {
    expr_type: ExpressionType, // Unary
    operator: Token,
    right: Box<Expression> // TODO: Other variant may show up?
}

impl UnaryExpression {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        match &self.operator {
            Token::TypeOnlyToken(t) => match t.token_type {
                TokenType::Minus => {
                    t.fmt_indented(f, 0);
                    self.right.fmt_indented(f, 0)
                },
                _ => write!(f, "([!!] Unknown operator for a UnaryExpression. Found {:?}", t.token_type)
            },
            Token::ValueToken(t) => write!(f, "([!!] ValueToken found as operator for the UnaryExpression.")
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
            Token::TypeOnlyToken(t) => match t.token_type {
                TokenType::DoubleEqual => {
                    self.left.fmt_indented(f, 0);
                    write!(f, " ");
                    t.fmt_indented(f, 0);
                    write!(f, " ");
                    self.right.fmt_indented(f, 0)
                },
                TokenType::LessEqual => {
                    self.left.fmt_indented(f, 0);
                    write!(f, " ");
                    t.fmt_indented(f, 0);
                    write!(f, " ");
                    self.right.fmt_indented(f, 0)
                },
                TokenType::And => {
                    self.left.fmt_indented(f, 0);
                    write!(f, " ");
                    t.fmt_indented(f, 0);
                    write!(f, " ");
                    self.right.fmt_indented(f, 0)
                },
                _ => write!(f, "([!!] Unknown operator for a BinaryExpression. Found {:?}", t.token_type)
            },
            Token::ValueToken(t) => write!(f, "([!!] ValueToken found as operator for the BinaryExpression.")
        }
    }
}

#[derive(Serialize, Deserialize)]
struct AssignExpression {
    expr_type: ExpressionType, // Assign
    name: NamedExpression,
    value: Box<Expression>
}

impl AssignExpression {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let current_indent = " ".repeat(indent * 4);
        write!(f, "{}", current_indent);
        self.name.fmt_indented(f, 0);
        write!(f, " = ");
        self.value.fmt_indented(f, 0)
    }
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2{
        println!("usage: {} <mist-file-path>", args[0]);
        return;
    }
    // let file_path = ".\\mists\\day_zero.mist.json";
    // let file_path = ".\\mists\\adeline_eight_hearts.mist.json";
    // let file_path = ".\\mists\\adeline_four_hearts.mist.json";
    // let file_path = ".\\mists\\adeline_quest_board.mist.json";
    // let file_path = ".\\mists\\adeline_six_hearts.mist.json";
    // let file_path = ".\\mists\\animal_festival_upcoming_eligible_year_one.mist.json";
    // let file_path = ".\\mists\\animal_festival_upcoming_eligible_year_two.mist.json";
    // let file_path = ".\\mists\\animal_festival_upcoming_ineligible_year_one.mist.json";
    // let file_path = ".\\mists\\animal_festival_upcoming_ineligible_year_two.mist.json";
    // let file_path = ".\\mists\\balor_eight_hearts.mist.json";
    // let file_path = ".\\mists\\balor_four_hearts.mist.json";
    // let file_path = ".\\mists\\balor_six_hearts.mist.json";
    // let file_path = ".\\mists\\_test.json";

    let file_path = args[1].to_string();
    
    let result = fs::read_to_string(file_path.clone());
    match result {
        Ok(data) => {
            // let result: Result<Mist> = serde_json::from_str(data.as_str());
            let json_deserializer = &mut serde_json::Deserializer::from_str(data.as_str());

            let result: std::result::Result<Mist, _> = serde_path_to_error::deserialize(json_deserializer);
            match result {
                Ok(mist) => println!("{}", mist),
                Err(err) => {
                    let path = err.path().to_string();
                    println!("Error: Failed to parse file '{}' with path: {}", file_path, path);
                }

            }
        },
        Err(error) => println!("Error: Failed to read file '{}': {}", file_path, error),
    };
}
