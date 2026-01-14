use std::process::Command;

fn run_x(args: &[&str]) -> (String, String, bool) {
    let output = Command::new("./target/release/x")
        .args(args)
        .output()
        .expect("Failed to run x");

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    (stdout, stderr, output.status.success())
}

/// Check that output looks like a shell command, not command results
fn looks_like_command(s: &str) -> bool {
    let s = s.trim();
    if s.is_empty() {
        return false;
    }

    // Commands typically start with a known command name or path
    let common_commands = [
        "ls", "find", "cat", "grep", "rg", "mkdir", "rm", "cp", "mv", "cd",
        "echo", "touch", "chmod", "chown", "du", "df", "pwd", "git", "cargo",
        "npm", "yarn", "docker", "kubectl", "curl", "wget", "tar", "zip",
        "unzip", "ssh", "scp", "rsync", "head", "tail", "sort", "uniq",
        "wc", "awk", "sed", "cut", "tr", "xargs", "tee", "diff", "patch",
    ];

    let first_word = s.split_whitespace().next().unwrap_or("");
    let base_cmd = first_word.split('/').last().unwrap_or(first_word);

    // Check if it starts with a known command
    if common_commands.contains(&base_cmd) {
        return true;
    }

    // Check if it's a path to an executable
    if first_word.starts_with('/') || first_word.starts_with("./") {
        return true;
    }

    // Check for shell constructs
    if s.contains("&&") || s.contains("||") || s.contains("|") || s.contains(";") {
        return true;
    }

    // Reject if it looks like file listing or error output
    let lines: Vec<&str> = s.lines().collect();
    if lines.len() > 5 {
        // Too many lines - probably command output, not a command
        return false;
    }

    // Single short line is probably a command
    lines.len() <= 2 && s.len() < 200
}

#[test]
fn test_dry_run_list_files() {
    let (stdout, _stderr, success) = run_x(&["--dry-run", "list files"]);
    assert!(success, "Command should succeed");
    assert!(
        looks_like_command(&stdout),
        "Output should be a command, got: {}",
        stdout
    );
}

#[test]
fn test_dry_run_disk_usage() {
    let (stdout, _stderr, success) = run_x(&["--dry-run", "show disk usage"]);
    assert!(success, "Command should succeed");
    let trimmed = stdout.trim();
    assert!(
        trimmed.starts_with("du") || trimmed.starts_with("df"),
        "Should generate du or df command, got: {}",
        stdout
    );
}

#[test]
fn test_dry_run_create_directory() {
    let (stdout, _stderr, success) = run_x(&["--dry-run", "create directory called foo"]);
    assert!(success, "Command should succeed");
    assert!(
        stdout.trim().starts_with("mkdir"),
        "Should generate mkdir command, got: {}",
        stdout
    );
}

#[test]
fn test_dry_run_no_markdown() {
    let (stdout, _stderr, success) = run_x(&["--dry-run", "list all rust files"]);
    assert!(success, "Command should succeed");
    assert!(
        !stdout.contains("```"),
        "Output should not contain markdown code blocks, got: {}",
        stdout
    );
}

#[test]
fn test_context_only() {
    let (_stdout, stderr, success) = run_x(&["--context"]);
    assert!(success, "Command should succeed");
    assert!(
        stderr.contains("Working directory:"),
        "Should show working directory in context"
    );
}

#[test]
fn test_init_zsh() {
    let (stdout, _stderr, success) = run_x(&["--init", "zsh"]);
    assert!(success, "Command should succeed");
    assert!(
        stdout.contains("x()") && stdout.contains("command x"),
        "Should output zsh function definition"
    );
}

#[test]
fn test_init_bash() {
    let (stdout, _stderr, success) = run_x(&["--init", "bash"]);
    assert!(success, "Command should succeed");
    assert!(
        stdout.contains("x()") && stdout.contains("command x"),
        "Should output bash function definition"
    );
}
