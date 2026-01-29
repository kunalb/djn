use anyhow::{Context, Result};
use std::fs;
use std::path::Path;
use std::process::Command;

/// Open a file in the user's editor
/// Returns true if the file was modified
pub fn open(path: &Path) -> Result<bool> {
    let editor = get_editor();
    let content_before = fs::read_to_string(path).unwrap_or_default();

    let status = Command::new(&editor)
        .arg(path)
        .status()
        .with_context(|| format!("failed to open editor: {editor}"))?;

    if !status.success() {
        anyhow::bail!("editor exited with non-zero status");
    }

    let content_after = fs::read_to_string(path).unwrap_or_default();
    Ok(content_before != content_after)
}

/// Get the editor command from environment
fn get_editor() -> String {
    std::env::var("EDITOR")
        .or_else(|_| std::env::var("VISUAL"))
        .unwrap_or_else(|_| "vi".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_editor_default() {
        // This test just verifies the function doesn't panic
        let editor = get_editor();
        assert!(!editor.is_empty());
    }
}
