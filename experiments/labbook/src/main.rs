use anyhow::Result;
use clap::{Parser, Subcommand};

mod commands;
mod config;
mod artifacts;
mod editor;
mod experiment;
mod git;

#[derive(Parser)]
#[command(name = "lab")]
#[command(about = "Experiment notebook CLI")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Initialize lab in current repository
    Init,
    /// Open a new experiment
    Open {
        /// Experiment name
        name: Option<String>,
    },
    /// Show current experiment status
    Status,
    /// Take a snapshot of current git state and artifacts
    Snapshot,
    /// Add artifacts to current experiment
    Add {
        /// Files to add
        #[arg(required = true)]
        files: Vec<String>,
    },
    /// Edit current experiment notes
    Edit,
    /// Close current experiment
    Close,
    /// List all experiments
    List,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Init => commands::init::run(),
        Commands::Open { name } => commands::open::run(name),
        Commands::Status => commands::status::run(),
        Commands::Snapshot => commands::snapshot::run(),
        Commands::Add { files } => commands::add::run(&files),
        Commands::Edit => commands::edit::run(),
        Commands::Close => commands::close::run(),
        Commands::List => commands::list::run(),
    }
}
