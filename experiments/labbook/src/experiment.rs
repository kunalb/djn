use anyhow::{Context, Result};
use chrono::{Local, NaiveDate};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};

use crate::git::GitStatus;

const META_FILE: &str = "meta.yaml";
const NOTES_FILE: &str = "notes.md";
const ARTIFACTS_DIR: &str = "artifacts";

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Experiment {
    pub id: String,
    pub repo_name: String,
    pub source_repo: PathBuf,
    pub status: ExperimentStatus,
    #[serde(default)]
    pub snapshots: Vec<Snapshot>,
    #[serde(default)]
    pub artifacts: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ExperimentStatus {
    Open,
    Closed,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Snapshot {
    pub commit: String,
    pub branch: String,
    pub dirty: bool,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub dirty_files: Vec<String>,
}

impl From<GitStatus> for Snapshot {
    fn from(status: GitStatus) -> Self {
        Self {
            commit: status.commit,
            branch: status.branch,
            dirty: status.dirty,
            dirty_files: status.dirty_files,
        }
    }
}

/// Generate an experiment ID for today with the given name
/// Format: YYYY/MM/DD/NNN-name
pub fn generate_id(lab_dir: &Path, name: Option<&str>) -> Result<String> {
    let today = Local::now().date_naive();
    let date_path = date_to_path(today);
    let day_dir = lab_dir.join(&date_path);

    // Find the next available number
    let next_num = if day_dir.exists() {
        let mut max_num = 0;
        for entry in fs::read_dir(&day_dir)? {
            let entry = entry?;
            if entry.file_type()?.is_dir() {
                if let Some(name) = entry.file_name().to_str() {
                    // Parse NNN from NNN-name or NNN
                    if let Some(num_str) = name.split('-').next() {
                        if let Ok(num) = num_str.parse::<u32>() {
                            max_num = max_num.max(num);
                        }
                    }
                }
            }
        }
        max_num + 1
    } else {
        1
    };

    let dir_name = match name {
        Some(n) => format!("{next_num:03}-{n}"),
        None => format!("{next_num:03}"),
    };

    Ok(format!("{date_path}/{dir_name}"))
}

/// Convert a date to path format: YYYY/MM/DD
fn date_to_path(date: NaiveDate) -> String {
    date.format("%Y/%m/%d").to_string()
}

/// Get the full path to an experiment directory
#[must_use] 
pub fn experiment_dir(lab_dir: &Path, id: &str) -> PathBuf {
    lab_dir.join(id)
}

/// Get the path to an experiment's meta.yaml
#[must_use] 
pub fn meta_path(lab_dir: &Path, id: &str) -> PathBuf {
    experiment_dir(lab_dir, id).join(META_FILE)
}

/// Get the path to an experiment's notes.md
#[must_use] 
pub fn notes_path(lab_dir: &Path, id: &str) -> PathBuf {
    experiment_dir(lab_dir, id).join(NOTES_FILE)
}

/// Get the path to an experiment's artifacts directory
#[must_use] 
pub fn artifacts_dir(lab_dir: &Path, id: &str) -> PathBuf {
    experiment_dir(lab_dir, id).join(ARTIFACTS_DIR)
}

/// Create experiment directory structure
pub fn create_experiment_dirs(lab_dir: &Path, id: &str) -> Result<()> {
    let exp_dir = experiment_dir(lab_dir, id);
    let art_dir = artifacts_dir(lab_dir, id);

    fs::create_dir_all(&exp_dir)
        .with_context(|| format!("failed to create experiment directory: {}", exp_dir.display()))?;
    fs::create_dir_all(&art_dir)
        .with_context(|| format!("failed to create artifacts directory: {}", art_dir.display()))?;

    Ok(())
}

/// Load experiment metadata
pub fn load_meta(lab_dir: &Path, id: &str) -> Result<Experiment> {
    let path = meta_path(lab_dir, id);
    let content = fs::read_to_string(&path)
        .with_context(|| format!("failed to read experiment metadata: {}", path.display()))?;
    let experiment: Experiment = serde_yaml::from_str(&content)
        .with_context(|| format!("failed to parse experiment metadata: {}", path.display()))?;
    Ok(experiment)
}

/// Save experiment metadata
pub fn save_meta(lab_dir: &Path, experiment: &Experiment) -> Result<()> {
    let path = meta_path(lab_dir, &experiment.id);
    let content = serde_yaml::to_string(experiment)?;
    fs::write(&path, content)
        .with_context(|| format!("failed to write experiment metadata: {}", path.display()))?;
    Ok(())
}

/// Write notes template to experiment
pub fn write_notes(lab_dir: &Path, id: &str, template: &str) -> Result<()> {
    let path = notes_path(lab_dir, id);
    fs::write(&path, template)
        .with_context(|| format!("failed to write notes: {}", path.display()))?;
    Ok(())
}

/// List all experiment IDs in the lab directory
pub fn list_all(lab_dir: &Path) -> Result<Vec<String>> {
    let mut experiments = Vec::new();
    collect_experiments(lab_dir, lab_dir, &mut experiments)?;
    experiments.sort();
    experiments.reverse(); // Most recent first
    Ok(experiments)
}

fn collect_experiments(base: &Path, current: &Path, experiments: &mut Vec<String>) -> Result<()> {
    if !current.exists() {
        return Ok(());
    }

    for entry in fs::read_dir(current)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() {
            // Check if this directory contains meta.yaml (is an experiment)
            if path.join(META_FILE).exists() {
                if let Ok(relative) = path.strip_prefix(base) {
                    experiments.push(relative.to_string_lossy().to_string());
                }
            } else {
                // Recurse into subdirectories
                collect_experiments(base, &path, experiments)?;
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_date_to_path() {
        let date = NaiveDate::from_ymd_opt(2024, 1, 29).unwrap();
        assert_eq!(date_to_path(date), "2024/01/29");
    }

    #[test]
    fn test_snapshot_from_git_status() {
        let status = GitStatus {
            commit: "abc123".to_string(),
            branch: "main".to_string(),
            dirty: true,
            dirty_files: vec!["file.rs".to_string()],
        };
        let snapshot: Snapshot = status.into();
        assert_eq!(snapshot.commit, "abc123");
        assert!(snapshot.dirty);
    }
}
