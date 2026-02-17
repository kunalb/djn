use anyhow::{Context, Result};
use std::path::Path;
use std::process::Command;

#[derive(Debug, Clone)]
pub struct GitStatus {
    pub commit: String,
    pub branch: String,
    pub dirty: bool,
    pub dirty_files: Vec<String>,
}

/// Get current git status using porcelain output
pub fn status() -> Result<GitStatus> {
    let commit = get_commit_hash()?;
    let branch = get_branch_name()?;
    let (dirty, dirty_files) = get_dirty_status()?;

    Ok(GitStatus {
        commit,
        branch,
        dirty,
        dirty_files,
    })
}

/// Get the current commit hash (short)
pub fn get_commit_hash() -> Result<String> {
    let output = Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .output()
        .context("failed to run git rev-parse")?;

    if !output.status.success() {
        anyhow::bail!("git rev-parse failed: {}", String::from_utf8_lossy(&output.stderr));
    }

    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

/// Get the current branch name
pub fn get_branch_name() -> Result<String> {
    let output = Command::new("git")
        .args(["rev-parse", "--abbrev-ref", "HEAD"])
        .output()
        .context("failed to run git rev-parse")?;

    if !output.status.success() {
        anyhow::bail!("git rev-parse failed: {}", String::from_utf8_lossy(&output.stderr));
    }

    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

/// Check if working directory is dirty and get list of dirty files
pub fn get_dirty_status() -> Result<(bool, Vec<String>)> {
    let output = Command::new("git")
        .args(["status", "--porcelain"])
        .output()
        .context("failed to run git status")?;

    if !output.status.success() {
        anyhow::bail!("git status failed: {}", String::from_utf8_lossy(&output.stderr));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let dirty_files: Vec<String> = stdout
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            // Porcelain format: XY filename
            // Skip the first 3 characters (status + space)
            line.get(3..).unwrap_or(line).to_string()
        })
        .collect();

    Ok((!dirty_files.is_empty(), dirty_files))
}

/// Commit all changes in a specific directory
pub fn commit_in_dir(dir: &Path, message: &str) -> Result<()> {
    // Stage all changes
    let output = Command::new("git")
        .args(["add", "-A"])
        .current_dir(dir)
        .output()
        .context("failed to run git add")?;

    if !output.status.success() {
        anyhow::bail!("git add failed: {}", String::from_utf8_lossy(&output.stderr));
    }

    // Check if there's anything to commit
    let output = Command::new("git")
        .args(["status", "--porcelain"])
        .current_dir(dir)
        .output()
        .context("failed to run git status")?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    if stdout.trim().is_empty() {
        // Nothing to commit
        return Ok(());
    }

    // Commit
    let output = Command::new("git")
        .args(["commit", "-m", message])
        .current_dir(dir)
        .output()
        .context("failed to run git commit")?;

    if !output.status.success() {
        anyhow::bail!("git commit failed: {}", String::from_utf8_lossy(&output.stderr));
    }

    Ok(())
}

/// Get diff between two commits
#[allow(dead_code)] // Used by lab-view binary
pub fn get_diff(repo_path: &Path, from: &str, to: &str) -> Result<String> {
    let output = Command::new("git")
        .args(["diff", &format!("{from}..{to}")])
        .current_dir(repo_path)
        .output()
        .context("failed to run git diff")?;

    if !output.status.success() {
        anyhow::bail!("git diff failed: {}", String::from_utf8_lossy(&output.stderr));
    }

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

/// Initialize a new git repository
pub fn init_repo(dir: &Path) -> Result<()> {
    let output = Command::new("git")
        .args(["init"])
        .current_dir(dir)
        .output()
        .context("failed to run git init")?;

    if !output.status.success() {
        anyhow::bail!("git init failed: {}", String::from_utf8_lossy(&output.stderr));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_parse_porcelain_status() {
        // This tests the parsing logic without needing a real git repo
        let porcelain_output = " M src/main.rs\n?? new_file.txt\n";
        let dirty_files: Vec<String> = porcelain_output
            .lines()
            .filter(|line| !line.is_empty())
            .map(|line| line.get(3..).unwrap_or(line).to_string())
            .collect();

        assert_eq!(dirty_files.len(), 2);
        assert_eq!(dirty_files[0], "src/main.rs");
        assert_eq!(dirty_files[1], "new_file.txt");
    }
}
