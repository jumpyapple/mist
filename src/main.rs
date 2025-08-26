mod cli;
mod expressions;
mod parser;
mod statements;
mod tokens;

use crate::cli::{Cli, Commands};
use crate::statements::NewStatement;
use clap::Parser;
use core::fmt;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;

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
#[serde(transparent)]
struct Mist {
    statements: Vec<NewStatement>,
}

impl fmt::Display for Mist {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in &self.statements {
            stmt.fmt_indented(f, 0)?;
            match stmt {
                NewStatement::Block { .. } => write!(f, "")?,
                NewStatement::Expr { .. } => write!(f, ";\n")?,
                NewStatement::Function { .. } => write!(f, "\n")?,
                NewStatement::Var { .. } => write!(f, ";\n")?,
                NewStatement::Simultaneous { .. } => write!(f, "\n")?,
                NewStatement::Free { stmt } => {
                    if stmt.free_has_block_statement() {
                        write!(f, "\n")?
                    } else {
                        write!(f, ";\n")?
                    }
                }
                NewStatement::If { .. } => write!(f, "\n")?,
                NewStatement::Return { .. } => write!(f, "")?,
            }
        }
        Ok(())
    }
}

fn main() {
    let args = Cli::parse();

    match args.command {
        Commands::List { mist_path } => {
            let result = fs::read_to_string(mist_path);
            match result {
                Ok(data) => {
                    let json_deserializer = &mut serde_json::Deserializer::from_str(data.as_str());
                    let result: Result<MistContainer, _> =
                        serde_path_to_error::deserialize(json_deserializer);
                    match result {
                        Ok(mist_container) => {
                            println!("Found {} mists:", mist_container.mists.len());
                            let mut mist_names =
                                mist_container.mists.keys().collect::<Vec<&String>>();
                            mist_names.sort();
                            for name in mist_names {
                                println!("{}", name);
                            }
                        }
                        Err(err) => {
                            let error_path = err.path().to_string();
                            println!("Error: '{}' occur at path '{}'", err, error_path);
                        }
                    }
                }
                Err(err) => println!("Error: {}", err),
            }
        }
        Commands::Info {
            mist_path,
            mist_name,
        } => {
            let result = fs::read_to_string(mist_path.clone());
            match result {
                Ok(data) => {
                    let json_deserializer = &mut serde_json::Deserializer::from_str(data.as_str());
                    let result: Result<MistContainer, _> =
                        serde_path_to_error::deserialize(json_deserializer);
                    match result {
                        Ok(mist_container) => {
                            if mist_container.mists.contains_key(&mist_name) {
                                let mist = mist_container.mists.get(&mist_name).unwrap();
                                println!("Mist: '{}'", mist_name);
                                println!(" - {} statements", mist.statements.len());
                            } else {
                                println!(
                                    "No mist with name '{}' found in file '{}'.",
                                    mist_name,
                                    mist_path.display()
                                );
                            }
                        }
                        Err(err) => {
                            let error_path = err.path().to_string();
                            println!("Error: '{}' occur at path '{}'", err, error_path);
                        }
                    }
                }
                Err(err) => println!("Error: {}", err),
            }
        }
    }
}
