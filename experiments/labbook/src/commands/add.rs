use anyhow::Result;
use std::path::PathBuf;

use crate::artifacts;
use crate::config;
use crate::experiment;
use crate::git;

pub fn run(files: &[String]) -> Result<()> {
    let cfg = config::load()?;
    let lab_dir = config::expand_path(&cfg.lab_dir);

    let current = config::get_current_experiment()?
        .ok_or_else(|| anyhow::anyhow!("no experiment currently open"))?;

    // Load experiment
    let mut exp = experiment::load_meta(&lab_dir, &current)?;

    // Copy files
    let artifacts_dir = experiment::artifacts_dir(&lab_dir, &current);
    let sources: Vec<PathBuf> = files.iter().map(PathBuf::from).collect();
    let copied = artifacts::copy_files(&sources, &artifacts_dir)?;

    // Update artifact list
    for artifact in &copied {
        if !exp.artifacts.contains(artifact) {
            exp.artifacts.push(artifact.clone());
        }
    }

    // Save metadata
    experiment::save_meta(&lab_dir, &exp)?;

    // Commit to lab repo
    let short_id = current.rsplit('/').next().unwrap_or(&current);
    let files_str = copied.join(", ");
    git::commit_in_dir(&lab_dir, &format!("add: {files_str} to {short_id}"))?;

    println!("Added {} artifact(s) to: {}", copied.len(), current);
    for artifact in &copied {
        println!("  - {artifact}");
    }

    Ok(())
}
