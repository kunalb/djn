use anyhow::Result;

use crate::config;
use crate::experiment::{self, ExperimentStatus};
use crate::git;

pub fn run() -> Result<()> {
    let cfg = config::load()?;
    let lab_dir = config::expand_path(&cfg.lab_dir);

    let current = config::get_current_experiment()?
        .ok_or_else(|| anyhow::anyhow!("no experiment currently open"))?;

    // Load and update experiment
    let mut exp = experiment::load_meta(&lab_dir, &current)?;
    exp.status = ExperimentStatus::Closed;
    experiment::save_meta(&lab_dir, &exp)?;

    // Clear current experiment
    config::set_current_experiment(None)?;

    // Commit to lab repo
    let short_id = current.rsplit('/').next().unwrap_or(&current);
    git::commit_in_dir(&lab_dir, &format!("close: {short_id}"))?;

    println!("Closed experiment: {current}");
    println!("  Snapshots: {}", exp.snapshots.len());
    println!("  Artifacts: {}", exp.artifacts.len());

    Ok(())
}
