use anyhow::Result;

use crate::artifacts;
use crate::config;
use crate::experiment::{self, Snapshot};
use crate::git;

pub fn run() -> Result<()> {
    let cfg = config::load()?;
    let repo_root = config::find_repo_root()?;
    let lab_dir = config::expand_path(&cfg.lab_dir);

    let current = config::get_current_experiment()?
        .ok_or_else(|| anyhow::anyhow!("no experiment currently open"))?;

    // Load experiment
    let mut exp = experiment::load_meta(&lab_dir, &current)?;

    // Get current git status
    let git_status = git::status()?;

    // Add snapshot
    exp.snapshots.push(Snapshot::from(git_status.clone()));

    // Update artifacts
    let artifacts_dir = experiment::artifacts_dir(&lab_dir, &current);
    let copied = artifacts::copy_configured_artifacts(
        &cfg.artifacts,
        &repo_root,
        &artifacts_dir,
    )?;

    // Merge new artifacts (avoid duplicates)
    for artifact in copied {
        if !exp.artifacts.contains(&artifact) {
            exp.artifacts.push(artifact);
        }
    }

    // Save metadata
    experiment::save_meta(&lab_dir, &exp)?;

    // Commit to lab repo
    let short_id = current.rsplit('/').next().unwrap_or(&current);
    git::commit_in_dir(&lab_dir, &format!("snapshot: {short_id}"))?;

    println!("Snapshot recorded for: {current}");
    println!("  Commit: {} ({})", git_status.commit, git_status.branch);
    if git_status.dirty {
        println!("  Dirty: {} files", git_status.dirty_files.len());
    }
    println!("  Total snapshots: {}", exp.snapshots.len());

    Ok(())
}
