use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};

const LAB_DIR: &str = ".lab";
const CONFIG_FILE: &str = "config.yaml";
const CURRENT_FILE: &str = "current";

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub lab_dir: PathBuf,
    pub repo_name: String,
    #[serde(default)]
    pub artifacts: Vec<String>,
    #[serde(default)]
    pub notes_template: String,
    #[serde(default)]
    pub git_hook: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            lab_dir: PathBuf::from("../lab"), // placeholder, set during init
            repo_name: String::new(),
            artifacts: Vec::new(),
            notes_template: default_notes_template(),
            git_hook: false,
        }
    }
}

fn default_notes_template() -> String {
    r"---
tags: []
hypothesis:
result:
---
## Notes

"
    .to_string()
}

/// Find the repository root by looking for .git directory
pub fn find_repo_root() -> Result<PathBuf> {
    let mut current = std::env::current_dir()?;
    loop {
        if current.join(".git").exists() {
            return Ok(current);
        }
        if !current.pop() {
            anyhow::bail!("not in a git repository");
        }
    }
}

/// Get the .lab directory path
pub fn lab_config_dir() -> Result<PathBuf> {
    Ok(find_repo_root()?.join(LAB_DIR))
}

/// Check if lab is initialized in current repo
#[must_use] 
pub fn is_initialized() -> bool {
    lab_config_dir()
        .map(|p| p.join(CONFIG_FILE).exists())
        .unwrap_or(false)
}

/// Load config from .lab/config.yaml
pub fn load() -> Result<Config> {
    let config_path = lab_config_dir()?.join(CONFIG_FILE);
    let content = fs::read_to_string(&config_path)
        .with_context(|| format!("failed to read config: {}", config_path.display()))?;
    let config: Config = serde_yaml::from_str(&content)
        .with_context(|| format!("failed to parse config: {}", config_path.display()))?;
    Ok(config)
}

/// Save config to .lab/config.yaml
pub fn save(config: &Config) -> Result<()> {
    let lab_dir = lab_config_dir()?;
    fs::create_dir_all(&lab_dir)
        .with_context(|| format!("failed to create .lab directory: {}", lab_dir.display()))?;

    let config_path = lab_dir.join(CONFIG_FILE);
    let content = serde_yaml::to_string(config)?;
    fs::write(&config_path, content)
        .with_context(|| format!("failed to write config: {}", config_path.display()))?;
    Ok(())
}

/// Get the current experiment ID (if any)
pub fn get_current_experiment() -> Result<Option<String>> {
    let current_path = lab_config_dir()?.join(CURRENT_FILE);
    if !current_path.exists() {
        return Ok(None);
    }
    let content = fs::read_to_string(&current_path)?.trim().to_string();
    if content.is_empty() {
        Ok(None)
    } else {
        Ok(Some(content))
    }
}

/// Set the current experiment ID
pub fn set_current_experiment(id: Option<&str>) -> Result<()> {
    let current_path = lab_config_dir()?.join(CURRENT_FILE);
    match id {
        Some(id) => fs::write(&current_path, id)?,
        None => {
            if current_path.exists() {
                fs::write(&current_path, "")?;
            }
        }
    }
    Ok(())
}

/// Expand ~ in paths
#[must_use] 
pub fn expand_path(path: &Path) -> PathBuf {
    if let Ok(stripped) = path.strip_prefix("~") {
        if let Some(home) = dirs_home() {
            return home.join(stripped);
        }
    }
    path.to_path_buf()
}

fn dirs_home() -> Option<PathBuf> {
    std::env::var_os("HOME").map(PathBuf::from)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = Config::default();
        assert!(config.artifacts.is_empty());
        assert!(!config.git_hook);
    }

    #[test]
    fn test_expand_path_with_home() {
        let path = Path::new("~/foo/bar");
        let expanded = expand_path(path);
        assert!(!expanded.starts_with("~"));
    }

    #[test]
    fn test_expand_path_without_home() {
        let path = Path::new("/absolute/path");
        let expanded = expand_path(path);
        assert_eq!(expanded, path);
    }
}
