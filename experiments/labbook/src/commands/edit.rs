use anyhow::Result;

use crate::config;
use crate::editor;
use crate::experiment;
use crate::git;

pub fn run() -> Result<()> {
    let cfg = config::load()?;
    let lab_dir = config::expand_path(&cfg.lab_dir);

    let current = config::get_current_experiment()?
        .ok_or_else(|| anyhow::anyhow!("no experiment currently open\nUse 'lab list' to select an experiment"))?;

    // Open notes in editor
    let notes_path = experiment::notes_path(&lab_dir, &current);
    let modified = editor::open(&notes_path)?;

    if modified {
        // Commit changes
        let short_id = current.rsplit('/').next().unwrap_or(&current);
        git::commit_in_dir(&lab_dir, &format!("edit: {short_id}"))?;
        println!("Notes updated for: {current}");
    } else {
        println!("No changes made");
    }

    Ok(())
}
