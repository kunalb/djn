use anyhow::Result;

use crate::config;
use crate::experiment;
use crate::git;

pub fn run() -> Result<()> {
    let cfg = config::load()?;
    let lab_dir = config::expand_path(&cfg.lab_dir);

    let current = config::get_current_experiment()?;

    if let Some(id) = current {
        let exp = experiment::load_meta(&lab_dir, &id)?;
        let git_status = git::status()?;

        println!("Current experiment: {id}");
        println!("  Status: {:?}", exp.status);
        println!("  Snapshots: {}", exp.snapshots.len());
        println!("  Artifacts: {}", exp.artifacts.len());

        if !exp.artifacts.is_empty() {
            for artifact in &exp.artifacts {
                println!("    - {artifact}");
            }
        }

        println!();
        println!("Git state:");
        println!("  Commit: {} ({})", git_status.commit, git_status.branch);
        if git_status.dirty {
            println!("  Dirty: yes");
            for file in &git_status.dirty_files {
                println!("    - {file}");
            }
        } else {
            println!("  Dirty: no");
        }
    } else {
        println!("No experiment currently open");
        println!();
        println!("Use 'lab open [name]' to start a new experiment");
        println!("Use 'lab list' to see past experiments");
    }

    Ok(())
}
