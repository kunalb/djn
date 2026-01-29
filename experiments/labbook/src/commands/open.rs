use anyhow::Result;

use crate::artifacts;
use crate::config;
use crate::editor;
use crate::experiment::{self, Experiment, ExperimentStatus, Snapshot};
use crate::git;

#[allow(clippy::needless_pass_by_value)] // name comes from clap
pub fn run(name: Option<String>) -> Result<()> {
    let cfg = config::load()?;
    let repo_root = config::find_repo_root()?;
    let lab_dir = config::expand_path(&cfg.lab_dir);

    // Check if experiment is already open
    if let Some(current) = config::get_current_experiment()? {
        anyhow::bail!(
            "experiment already open: {current}\nUse 'lab close' first or 'lab status' to see current state"
        );
    }

    // Check git status
    let git_status = git::status()?;
    if git_status.dirty {
        eprintln!("Warning: working directory has uncommitted changes");
        eprintln!("  Dirty files: {}", git_status.dirty_files.join(", "));
        eprintln!("  Consider committing before starting experiment");
        eprintln!();
    }

    // Generate experiment ID
    let id = experiment::generate_id(&lab_dir, name.as_deref())?;

    // Create experiment directories
    experiment::create_experiment_dirs(&lab_dir, &id)?;

    // Copy configured artifacts
    let artifacts_dir = experiment::artifacts_dir(&lab_dir, &id);
    let copied_artifacts = artifacts::copy_configured_artifacts(
        &cfg.artifacts,
        &repo_root,
        &artifacts_dir,
    )?;

    // Create experiment metadata
    let exp = Experiment {
        id: id.clone(),
        repo_name: cfg.repo_name.clone(),
        source_repo: repo_root.clone(),
        status: ExperimentStatus::Open,
        snapshots: vec![Snapshot::from(git_status)],
        artifacts: copied_artifacts,
    };

    experiment::save_meta(&lab_dir, &exp)?;

    // Write notes template
    experiment::write_notes(&lab_dir, &id, &cfg.notes_template)?;

    // Set as current experiment
    config::set_current_experiment(Some(&id))?;

    // Commit to lab repo
    git::commit_in_dir(&lab_dir, &format!("open: {}", short_id(&id)))?;

    println!("Created experiment: {id}");

    // Open editor
    let notes_path = experiment::notes_path(&lab_dir, &id);
    editor::open(&notes_path)?;

    // Commit any edits
    git::commit_in_dir(&lab_dir, &format!("edit: {}", short_id(&id)))?;

    Ok(())
}

fn short_id(id: &str) -> &str {
    id.rsplit('/').next().unwrap_or(id)
}
