use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[command(name = "mist")]
#[command(about = "A tool for working with Fields of Mistria's __mist__.json file")]
pub(crate) struct Cli {
    #[command(subcommand)]
    pub(crate) command: Commands,
}

#[derive(Debug, Subcommand)]
pub(crate) enum Commands {
    /// List the name of mists in a file.
    #[command(arg_required_else_help = true)]
    List {
        /// A path to the mist file to list mists' name from
        #[arg(value_name = "PATH")]
        mist_path: PathBuf,
    },
    /// Show information of a mist
    #[command(arg_required_else_help = true)]
    Info {
        /// A path to the mist file containing mists.
        #[arg(value_name = "PATH")]
        mist_path: PathBuf,
        /// The name of the mist.
        #[arg(value_name = "NAME")]
        mist_name: String,
    },
}
