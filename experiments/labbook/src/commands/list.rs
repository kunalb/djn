use anyhow::Result;

use crate::config;
use crate::experiment::{self, ExperimentStatus};

pub fn run() -> Result<()> {
    let cfg = config::load()?;
    let lab_dir = config::expand_path(&cfg.lab_dir);

    let experiments = experiment::list_all(&lab_dir)?;

    if experiments.is_empty() {
        println!("No experiments found");
        println!("Use 'lab open [name]' to create one");
        return Ok(());
    }

    let current = config::get_current_experiment()?;

    println!("Experiments ({}):", experiments.len());
    println!();

    for id in &experiments {
        let marker = if current.as_ref() == Some(id) { "* " } else { "  " };

        match experiment::load_meta(&lab_dir, id) {
            Ok(exp) => {
                let status = match exp.status {
                    ExperimentStatus::Open => "open",
                    ExperimentStatus::Closed => "closed",
                };
                println!(
                    "{}{} [{}] ({} snapshots, {} artifacts)",
                    marker,
                    id,
                    status,
                    exp.snapshots.len(),
                    exp.artifacts.len()
                );
            }
            Err(_) => {
                println!("{marker}{id} [error reading metadata]");
            }
        }
    }

    Ok(())
}
