use anyhow::{Context, Result};
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

use crate::config::{self, Config};
use crate::git;

pub fn run() -> Result<()> {
    if config::is_initialized() {
        anyhow::bail!("lab is already initialized in this repository");
    }

    let repo_root = config::find_repo_root()?;
    let repo_name = repo_root
        .file_name().map_or_else(|| "unknown".to_string(), |s| s.to_string_lossy().to_string());

    println!("Initializing lab in: {}", repo_root.display());

    // Prompt for lab directory
    let lab_dir = prompt_lab_dir(&repo_name)?;
    let lab_dir_expanded = config::expand_path(&lab_dir);

    // Create lab directory if it doesn't exist
    if !lab_dir_expanded.exists() {
        let create_as_branch = prompt_yes_no("Lab directory doesn't exist. Create as orphan branch of current repo? [y/N] ")?;

        if create_as_branch {
            create_lab_as_worktree(&repo_root, &lab_dir_expanded)?;
        } else {
            fs::create_dir_all(&lab_dir_expanded)
                .with_context(|| format!("failed to create lab directory: {}", lab_dir_expanded.display()))?;
            git::init_repo(&lab_dir_expanded)?;
            setup_gitattributes(&lab_dir_expanded)?;
            git::commit_in_dir(&lab_dir_expanded, "init: lab notebook")?;
        }
    }

    // Prompt for artifact patterns
    let artifacts = prompt_artifacts()?;

    // Prompt for git hook
    let git_hook = prompt_yes_no("Install git commit hook to tag commits with experiment ID? [y/N] ")?;

    let config = Config {
        lab_dir,
        repo_name,
        artifacts,
        notes_template: config::Config::default().notes_template,
        git_hook,
    };

    config::save(&config)?;

    if git_hook {
        install_git_hook(&repo_root)?;
    }

    println!("Lab initialized successfully!");
    println!("  Config: {}/.lab/config.yaml", repo_root.display());
    println!("  Lab directory: {}", lab_dir_expanded.display());

    Ok(())
}

fn prompt_lab_dir(repo_name: &str) -> Result<PathBuf> {
    let default_dir = format!("../{repo_name}-lab");
    print!("Lab directory [{default_dir}]: ");
    io::stdout().flush()?;

    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    let input = input.trim();

    if input.is_empty() {
        Ok(PathBuf::from(default_dir))
    } else {
        Ok(PathBuf::from(input))
    }
}

fn prompt_artifacts() -> Result<Vec<String>> {
    println!("Enter artifact patterns (glob), one per line. Empty line to finish:");
    println!("  Examples: outputs/*.png, config.yaml, logs/*.log");

    let mut patterns = Vec::new();
    loop {
        print!("> ");
        io::stdout().flush()?;

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        let input = input.trim();

        if input.is_empty() {
            break;
        }
        patterns.push(input.to_string());
    }

    Ok(patterns)
}

fn prompt_yes_no(prompt: &str) -> Result<bool> {
    print!("{prompt}");
    io::stdout().flush()?;

    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    let input = input.trim().to_lowercase();

    Ok(input == "y" || input == "yes")
}

fn create_lab_as_worktree(_repo_root: &Path, lab_dir: &Path) -> Result<()> {
    let branch_name = format!("{}-lab", git::get_branch_name()?);

    // Create the directory
    fs::create_dir_all(lab_dir)
        .with_context(|| format!("failed to create lab directory: {}", lab_dir.display()))?;

    // Initialize as git repo and create orphan branch
    git::init_repo(lab_dir)?;

    // Note: This creates a separate repo, not a worktree
    // For true worktree support, we'd need additional git commands

    setup_gitattributes(lab_dir)?;

    // Initial commit
    git::commit_in_dir(lab_dir, &format!("init: lab notebook for {branch_name}"))?;

    println!("Created lab repository at: {}", lab_dir.display());

    Ok(())
}

fn setup_gitattributes(lab_dir: &Path) -> Result<()> {
    let gitattributes = lab_dir.join(".gitattributes");
    let content = r"# Git LFS for artifact files
*.png filter=lfs diff=lfs merge=lfs -text
*.jpg filter=lfs diff=lfs merge=lfs -text
*.jpeg filter=lfs diff=lfs merge=lfs -text
*.gif filter=lfs diff=lfs merge=lfs -text
*.pdf filter=lfs diff=lfs merge=lfs -text
*.pt filter=lfs diff=lfs merge=lfs -text
*.pth filter=lfs diff=lfs merge=lfs -text
*.bin filter=lfs diff=lfs merge=lfs -text
*.npy filter=lfs diff=lfs merge=lfs -text
*.npz filter=lfs diff=lfs merge=lfs -text
";
    fs::write(&gitattributes, content)?;
    Ok(())
}

fn install_git_hook(repo_root: &Path) -> Result<()> {
    let hooks_dir = repo_root.join(".git/hooks");
    fs::create_dir_all(&hooks_dir)?;

    let hook_path = hooks_dir.join("prepare-commit-msg");
    let hook_content = r#"#!/bin/bash
# Lab experiment notebook hook
current_file=".lab/current"
if [ -f "$current_file" ]; then
    current=$(cat "$current_file" 2>/dev/null)
    if [ -n "$current" ]; then
        echo "" >> "$1"
        echo "Lab: $current" >> "$1"
    fi
fi
"#;

    fs::write(&hook_path, hook_content)?;

    // Make executable
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&hook_path)?.permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&hook_path, perms)?;
    }

    Ok(())
}
