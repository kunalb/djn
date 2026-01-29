use anyhow::{Context, Result};
use glob::glob;
use std::fs;
use std::path::{Path, PathBuf};

/// Copy a single file to the artifacts directory
pub fn copy_file(source: &Path, artifacts_dir: &Path) -> Result<String> {
    let file_name = source
        .file_name()
        .context("source path has no filename")?
        .to_string_lossy()
        .to_string();

    let dest = artifacts_dir.join(&file_name);

    fs::copy(source, &dest).with_context(|| {
        format!(
            "failed to copy {} to {}",
            source.display(),
            dest.display()
        )
    })?;

    Ok(file_name)
}

/// Copy multiple files to the artifacts directory
pub fn copy_files(sources: &[PathBuf], artifacts_dir: &Path) -> Result<Vec<String>> {
    let mut copied = Vec::new();
    for source in sources {
        let name = copy_file(source, artifacts_dir)?;
        copied.push(name);
    }
    Ok(copied)
}

/// Expand glob patterns relative to a base directory
pub fn expand_globs(patterns: &[String], base_dir: &Path) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();

    for pattern in patterns {
        let full_pattern = base_dir.join(pattern);
        let pattern_str = full_pattern.to_string_lossy();

        for entry in glob(&pattern_str)? {
            match entry {
                Ok(path) => {
                    if path.is_file() {
                        files.push(path);
                    }
                }
                Err(e) => {
                    eprintln!("warning: glob error for {pattern}: {e}");
                }
            }
        }
    }

    Ok(files)
}

/// Copy all files matching configured patterns
pub fn copy_configured_artifacts(
    patterns: &[String],
    source_dir: &Path,
    artifacts_dir: &Path,
) -> Result<Vec<String>> {
    let files = expand_globs(patterns, source_dir)?;
    copy_files(&files, artifacts_dir)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use tempfile::tempdir;

    #[test]
    fn test_copy_file() {
        let temp = tempdir().unwrap();
        let source = temp.path().join("source.txt");
        let artifacts = temp.path().join("artifacts");
        fs::create_dir(&artifacts).unwrap();

        File::create(&source).unwrap();
        fs::write(&source, "test content").unwrap();

        let name = copy_file(&source, &artifacts).unwrap();
        assert_eq!(name, "source.txt");
        assert!(artifacts.join("source.txt").exists());
    }

    #[test]
    fn test_expand_globs() {
        let temp = tempdir().unwrap();
        let subdir = temp.path().join("outputs");
        fs::create_dir(&subdir).unwrap();

        File::create(subdir.join("a.png")).unwrap();
        File::create(subdir.join("b.png")).unwrap();
        File::create(subdir.join("c.txt")).unwrap();

        let patterns = vec!["outputs/*.png".to_string()];
        let files = expand_globs(&patterns, temp.path()).unwrap();

        assert_eq!(files.len(), 2);
    }
}
