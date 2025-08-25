mod expressions;
mod parser;
mod statements;
mod tokens;

use core::fmt;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io;

use crate::statements::Statement;
use serde::de::IntoDeserializer;
use serde::de::Visitor;
use serde::{Deserialize, Deserializer, Serialize, de};

#[derive(Serialize, Deserialize)]
#[serde(transparent)]
struct MistContainer {
    mists: HashMap<String, Mist>,
}

impl MistContainer {
    fn render_mist_as_human_readable(&self, key: &str) -> Option<String> {
        let res = self.mists.get(key);
        match res {
            Some(mist) => Some(format!("{}", mist)),
            None => None,
        }
    }
}

#[derive(Serialize, Deserialize)]
// #[serde(transparent)]
struct Mist {
    #[serde(
        alias = "adeline_eight_hearts.mist",
        alias = "adeline_four_hearts.mist",
        alias = "adeline_quest_board.mist",
        alias = "adeline_six_hearts.mist",
        alias = "adeline_two_hearts.mist",
        alias = "animal_festival_upcoming_eligible_year_one.mist",
        alias = "animal_festival_upcoming_eligible_year_two.mist",
        alias = "animal_festival_upcoming_ineligible_year_one.mist",
        alias = "animal_festival_upcoming_ineligible_year_two.mist",
        alias = "balor_eight_hearts.mist",
        alias = "balor_four_hearts.mist",
        alias = "balor_six_hearts.mist",
        alias = "balor_two_hearts.mist",
        alias = "bathhouse.mist",
        alias = "bathhouse_date.mist",
        alias = "beach_date.mist",
        alias = "beach_secret.mist",
        alias = "break_earth_seal.mist",
        alias = "break_fire_seal.mist",
        alias = "break_water_seal.mist",
        alias = "caldarus_eight_hearts.mist",
        alias = "caldarus_recovery.mist",
        alias = "celine_eight_hearts.mist",
        alias = "celine_four_hearts.mist",
        alias = "celine_six_hearts.mist",
        alias = "celine_two_hearts.mist",
        alias = "cop_some_ore.mist",
        alias = "crafting.mist",
        alias = "crafting_tutorial.mist",
        alias = "cutscene_basement.mist",
        alias = "darcy_two_hearts.mist",
        alias = "day_zero.mist",
        alias = "deep_woods_picnic_date.mist",
        alias = "delivering_the_sealing_scroll.mist",
        alias = "destroy_vines_eastern_road.mist",
        alias = "do_a_bro_a_favor.mist",
        alias = "dying.mist",
        alias = "earth_seal.mist",
        alias = "eiland_eight_hearts.mist",
        alias = "eiland_four_hearts.mist",
        alias = "eiland_six_hearts.mist",
        alias = "eiland_two_hearts.mist",
        alias = "elsie_dating_tutorial.mist",
        alias = "elsie_two_hearts.mist",
        alias = "errol_two_hearts.mist",
        alias = "farm_introduction.mist",
        alias = "find_the_weathervane.mist",
        alias = "find_the_weathervane_setup.mist",
        alias = "fire_seal.mist",
        alias = "full_restore.mist",
        alias = "gem_cutting_date.mist",
        alias = "harvest_festival_dance.mist",
        alias = "harvest_festival_first_place.mist",
        alias = "harvest_festival_morning.mist",
        alias = "harvest_festival_no_place.mist",
        alias = "harvest_festival_second_place.mist",
        alias = "harvest_festival_setup.mist",
        alias = "harvest_festival_third_place.mist",
        alias = "hayden_eight_hearts.mist",
        alias = "hayden_four_hearts.mist",
        alias = "hayden_six_hearts.mist",
        alias = "hayden_two_hearts.mist",
        alias = "hemlock_two_hearts.mist",
        alias = "inn_meal_date.mist",
        alias = "jos_cooking_class.mist",
        alias = "josephine_two_hearts.mist",
        alias = "juniper_eight_hearts.mist",
        alias = "juniper_four_hearts.mist",
        alias = "juniper_six_hearts.mist",
        alias = "juniper_two_hearts.mist",
        alias = "luc_two_hearts.mist",
        alias = "maple_two_hearts.mist",
        alias = "march_eight_hearts.mist",
        alias = "march_four_hearts.mist",
        alias = "march_six_hearts.mist",
        alias = "march_two_hearts.mist",
        alias = "museum_donation_wanted.mist",
        alias = "narrows_secret.mist",
        alias = "nora_two_hearts.mist",
        alias = "olric_two_hearts.mist",
        alias = "park_date.mist",
        alias = "pet_arrival.mist",
        alias = "pet_dream.mist",
        alias = "post_earth_seal.mist",
        alias = "post_water_seal.mist",
        alias = "procuring_the_sealing_scroll.mist",
        alias = "prologue.mist",
        alias = "reina_eight_hearts.mist",
        alias = "reina_four_hearts.mist",
        alias = "reina_six_hearts.mist",
        alias = "reina_two_hearts.mist",
        alias = "repair_haydens_barn_pt_1.mist",
        alias = "repair_haydens_barn_pt_2.mist",
        alias = "repair_the_beach_bridge_pt_1.mist",
        alias = "repair_the_beach_bridge_pt_2.mist",
        alias = "repair_the_bridge_pt_1.mist",
        alias = "repair_the_bridge_pt_2.mist",
        alias = "repair_the_general_store_pt_1.mist",
        alias = "repair_the_general_store_pt_2.mist",
        alias = "repair_the_inn_pt_1.mist",
        alias = "repair_the_inn_pt_2.mist",
        alias = "repair_the_mill_pt_1.mist",
        alias = "repair_the_mill_pt_2.mist",
        alias = "repair_the_summit_stairs.mist",
        alias = "replenishing_mistrias_food_reserves_1.mist",
        alias = "replenishing_mistrias_food_reserves_2_pt_1.mist",
        alias = "replenishing_mistrias_food_reserves_2_pt_2.mist",
        alias = "reveal_mistmare.mist",
        alias = "ruins_seal.mist",
        alias = "ryis_eight_hearts.mist",
        alias = "ryis_four_hearts.mist",
        alias = "ryis_six_hearts.mist",
        alias = "ryis_two_hearts.mist",
        alias = "shooting_star_adeline.mist",
        alias = "shooting_star_balor.mist",
        alias = "shooting_star_blocked.mist",
        alias = "shooting_star_caldarus.mist",
        alias = "shooting_star_celine.mist",
        alias = "shooting_star_date.mist",
        alias = "shooting_star_eiland.mist",
        alias = "shooting_star_hayden.mist",
        alias = "shooting_star_juniper.mist",
        alias = "shooting_star_march.mist",
        alias = "shooting_star_morning.mist",
        alias = "shooting_star_reina.mist",
        alias = "shooting_star_romantic.mist",
        alias = "shooting_star_ryis.mist",
        alias = "shooting_star_solo.mist",
        alias = "shooting_star_valen.mist",
        alias = "somethings_bugging_me.mist",
        alias = "spell_learned.mist",
        alias = "spring_festival_first_place.mist",
        alias = "spring_festival_first_place_plus.mist",
        alias = "spring_festival_morning.mist",
        alias = "spring_festival_no_place.mist",
        alias = "spring_festival_second_place.mist",
        alias = "spring_festival_setup.mist",
        alias = "spring_festival_setup_year_2.mist",
        alias = "spring_festival_third_place.mist",
        alias = "std.mist",
        alias = "stillwell_two_hearts.mist",
        alias = "stinky_stamina_potion.mist",
        alias = "stone_refinery_pt_1.mist",
        alias = "stone_refinery_pt_2.mist",
        alias = "taliferro_two_hearts.mist",
        alias = "tea_with_hayden.mist",
        alias = "terithia_two_hearts.mist",
        alias = "the_animal_festival.mist",
        alias = "the_unusual_tree_pt_1.mist",
        alias = "the_unusual_tree_pt_2.mist",
        alias = "translating_the_earth_tablet.mist",
        alias = "translating_the_fire_tablet.mist",
        alias = "translating_the_water_tablet.mist",
        alias = "unit_test.mist",
        alias = "unlocking_the_mines.mist",
        alias = "unlocking_the_mines_pt_1.mist",
        alias = "unlocking_the_mines_pt_2.mist",
        alias = "upgrade_the_carpenters_shop_pt_1.mist",
        alias = "upgrade_the_carpenters_shop_pt_2.mist",
        alias = "upgrade_the_saturday_market.mist",
        alias = "valen_eight_hearts.mist",
        alias = "valen_four_hearts.mist",
        alias = "valen_six_hearts.mist",
        alias = "valen_two_hearts.mist",
        alias = "water_seal.mist",
        alias = "woodcrafting_essentials.mist"
    )]
    statements: Vec<Statement>,
}

impl fmt::Display for Mist {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            stmt.fmt_indented(f, 0);
            match stmt {
                Statement::Block(s) => write!(f, "")?,
                Statement::Expr(s) => write!(f, ";\n")?,
                Statement::Function(s) => write!(f, "\n")?,
                Statement::Var(s) => write!(f, ";\n")?,
                Statement::Simultaneous(s) => write!(f, "\n")?,
                Statement::Free(s) => {
                    if s.has_block_statement() {
                        write!(f, "\n")?
                    } else {
                        write!(f, ";\n")?
                    }
                }
                Statement::If(s) => write!(f, "\n")?,
                Statement::Return(s) => write!(f, "")?,
            }
        }
        Ok(())
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
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

            let result: std::result::Result<Mist, _> =
                serde_path_to_error::deserialize(json_deserializer);
            match result {
                Ok(mist) => println!("{}", mist),
                Err(err) => {
                    let path = err.path().to_string();
                    println!(
                        "Error: Failed to parse file '{}' with path: {}",
                        file_path, path
                    );
                }
            }
        }
        Err(error) => println!("Error: Failed to read file '{}': {}", file_path, error),
    };
}
