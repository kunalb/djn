use std::process::Command;

use crate::context::Context;

/// Resource usage metrics from a child process
#[derive(Debug, Clone, Default)]
pub struct ResourceUsage {
    /// User CPU time in microseconds
    pub user_time_us: u64,
    /// System CPU time in microseconds
    pub system_time_us: u64,
    /// Maximum resident set size in kilobytes
    pub max_rss_kb: u64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Provider {
    Claude,
    Gemini,
    OpenAI,
}

impl Provider {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "claude" | "anthropic" => Some(Provider::Claude),
            "gemini" | "google" => Some(Provider::Gemini),
            "openai" | "gpt" | "chatgpt" | "codex" => Some(Provider::OpenAI),
            _ => None,
        }
    }

    pub fn cli_name(&self) -> &'static str {
        match self {
            Provider::Claude => "claude",
            Provider::Gemini => "gemini",
            Provider::OpenAI => "codex",
        }
    }

    pub fn is_available(&self) -> bool {
        self.find_executable().is_some()
    }

    /// Find the executable path, checking common locations
    pub fn find_executable(&self) -> Option<String> {
        let name = self.cli_name();

        // Check if it's in PATH
        if Command::new("which")
            .arg(name)
            .output()
            .map(|o| o.status.success())
            .unwrap_or(false)
        {
            return Some(name.to_string());
        }

        // Check common locations
        let home = std::env::var("HOME").unwrap_or_default();
        let common_paths: Vec<String> = match self {
            Provider::Claude => vec![
                format!("{}/.claude/local/claude", home),
                format!("{}/.local/bin/claude", home),
            ],
            Provider::Gemini => vec![
                format!("{}/.local/bin/gemini", home),
            ],
            Provider::OpenAI => vec![
                format!("{}/.local/bin/codex", home),
            ],
        };

        for path in common_paths {
            if std::path::Path::new(&path).exists() {
                return Some(path);
            }
        }

        None
    }

    pub fn display_name(&self) -> &'static str {
        match self {
            Provider::Claude => "claude",
            Provider::Gemini => "gemini",
            Provider::OpenAI => "codex",
        }
    }
}

pub const SYSTEM_PROMPT: &str = r#"You are a shell command generator. Given the user's request and context, generate a shell command to execute.

Output the command wrapped in <cmd> tags like this:
<cmd>your command here</cmd>

Rules:
- Put the final command inside <cmd>...</cmd> tags
- For multiple commands, use && or ; inside the tags
- You may explain your reasoning before the tags if helpful
- Use the context to understand what the user wants
- If the request references "previous command" or "last command", use the history context
- If fixing an error, look at the terminal content for error messages"#;

/// Build the full prompt that would be sent to the model
pub fn build_prompt(request: &str, context: &Context) -> String {
    format!(
        "{}\n\nContext:\n{}\n\nUser request: {}",
        SYSTEM_PROMPT,
        context.format_for_prompt(),
        request
    )
}

pub fn generate_command(
    provider: Provider,
    model: Option<&str>,
    request: &str,
    context: &Context,
) -> Result<(String, ResourceUsage), String> {
    let full_prompt = format!(
        "{}\n\nContext:\n{}\n\nUser request: {}",
        SYSTEM_PROMPT,
        context.format_for_prompt(),
        request
    );

    match provider {
        Provider::Claude => generate_with_claude(&full_prompt, model),
        Provider::Gemini => generate_with_gemini(&full_prompt, model),
        Provider::OpenAI => generate_with_openai(&full_prompt, model),
    }
}

/// Ensure gemini settings file exists with tools disabled
fn ensure_gemini_settings() -> Result<std::path::PathBuf, String> {
    let config_dir = std::env::var("XDG_CONFIG_HOME")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|_| {
            dirs::home_dir()
                .unwrap_or_else(|| std::path::PathBuf::from("."))
                .join(".config")
        })
        .join("x")
        .join("gemini");

    let settings_path = config_dir.join("settings.json");

    if !settings_path.exists() {
        std::fs::create_dir_all(&config_dir)
            .map_err(|e| format!("Failed to create gemini config dir: {}", e))?;

        // Disable all tools to prevent gemini from executing commands
        let settings = r#"{
  "tools": {
    "coreTools": []
  }
}"#;
        std::fs::write(&settings_path, settings)
            .map_err(|e| format!("Failed to write gemini settings: {}", e))?;
    }

    Ok(settings_path)
}

fn generate_with_claude(prompt: &str, model: Option<&str>) -> Result<(String, ResourceUsage), String> {
    use std::io::Read;

    let exe = Provider::Claude.find_executable()
        .ok_or_else(|| "claude CLI not found. Install from: https://claude.ai/code".to_string())?;

    // Map short names to full model names
    let model_arg = model.map(|m| match m {
        "opus" => "claude-opus-4-5-20250514",
        "sonnet" => "claude-sonnet-4-5-20250514",
        "haiku" => "claude-haiku-4-5-20250514",
        _ => m,
    });

    let mut cmd = Command::new(&exe);
    cmd.args(["--print", prompt]);
    if let Some(m) = model_arg {
        cmd.args(["--model", m]);
    }

    let mut child = cmd
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to run claude: {}", e))?;

    let pid = child.id() as i32;

    // Read stdout/stderr before waiting
    let mut stdout_buf = Vec::new();
    let mut stderr_buf = Vec::new();
    if let Some(mut stdout) = child.stdout.take() {
        let _ = stdout.read_to_end(&mut stdout_buf);
    }
    if let Some(mut stderr) = child.stderr.take() {
        let _ = stderr.read_to_end(&mut stderr_buf);
    }

    // Wait with resource tracking
    let (exit_code, usage) = wait_with_rusage(pid);

    if exit_code != 0 {
        let stderr = String::from_utf8_lossy(&stderr_buf);
        let detail = if stderr.is_empty() {
            "no error output".to_string()
        } else {
            stderr.trim().to_string()
        };
        return Err(format!("claude exited with code {}: {}", exit_code, detail));
    }

    let stdout = String::from_utf8_lossy(&stdout_buf).trim().to_string();
    if stdout.is_empty() {
        return Err("claude returned empty output".to_string());
    }

    Ok((clean_command_output(&stdout), usage))
}

fn generate_with_gemini(prompt: &str, model: Option<&str>) -> Result<(String, ResourceUsage), String> {
    use std::io::{Read, Write};

    let exe = Provider::Gemini.find_executable()
        .ok_or_else(|| "gemini CLI not found. Install from: https://github.com/google-gemini/gemini-cli".to_string())?;

    // Create safe gemini settings that disable tool execution
    let settings_path = ensure_gemini_settings()?;

    let mut cmd = Command::new(&exe);
    cmd.env("GEMINI_CLI_SYSTEM_SETTINGS_PATH", &settings_path);
    if let Some(m) = model {
        cmd.args(["-m", m]);
    }

    let mut child = cmd
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to run gemini: {}", e))?;

    let pid = child.id() as i32;

    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(prompt.as_bytes());
    }

    // Read stdout/stderr before waiting
    let mut stdout_buf = Vec::new();
    let mut stderr_buf = Vec::new();
    if let Some(mut stdout) = child.stdout.take() {
        let _ = stdout.read_to_end(&mut stdout_buf);
    }
    if let Some(mut stderr) = child.stderr.take() {
        let _ = stderr.read_to_end(&mut stderr_buf);
    }

    // Wait with resource tracking
    let (exit_code, usage) = wait_with_rusage(pid);

    if exit_code != 0 {
        let stderr = String::from_utf8_lossy(&stderr_buf);
        let detail = if stderr.is_empty() {
            "no error output".to_string()
        } else {
            stderr.trim().to_string()
        };
        return Err(format!("gemini exited with code {}: {}", exit_code, detail));
    }

    let stdout = String::from_utf8_lossy(&stdout_buf).trim().to_string();
    if stdout.is_empty() {
        return Err("gemini returned empty output".to_string());
    }

    Ok((clean_command_output(&stdout), usage))
}

fn generate_with_openai(prompt: &str, model: Option<&str>) -> Result<(String, ResourceUsage), String> {
    let exe = Provider::OpenAI.find_executable()
        .ok_or_else(|| "codex CLI not found. Install from: https://github.com/openai/codex".to_string())?;

    // Use temp file for output (codex duplicates when using /dev/stdout)
    let tmp_file = format!("/tmp/x-codex-{}", std::process::id());
    let err_file = format!("/tmp/x-codex-err-{}", std::process::id());

    let mut cmd = Command::new(&exe);
    cmd.args(["exec", "--skip-git-repo-check", "-o", &tmp_file]);
    if let Some(m) = model {
        cmd.args(["-m", m]);
    }
    cmd.arg(prompt);

    // Capture stderr for error messages
    let err_fd = std::fs::File::create(&err_file).ok();
    if let Some(f) = err_fd {
        cmd.stderr(f);
    }
    cmd.stdout(std::process::Stdio::null());

    let child = cmd
        .stdin(std::process::Stdio::null())
        .spawn()
        .map_err(|e| format!("Failed to run codex: {}", e))?;

    let pid = child.id() as i32;

    // Wait with resource tracking
    let (exit_code, usage) = wait_with_rusage(pid);

    let output = std::fs::read_to_string(&tmp_file).unwrap_or_default();
    let stderr = std::fs::read_to_string(&err_file).unwrap_or_default();
    let _ = std::fs::remove_file(&tmp_file);
    let _ = std::fs::remove_file(&err_file);

    if exit_code != 0 {
        let detail = if stderr.is_empty() {
            "no error output".to_string()
        } else {
            stderr.trim().to_string()
        };
        return Err(format!("codex exited with code {}: {}", exit_code, detail));
    }

    if output.is_empty() {
        return Err("codex returned empty output".to_string());
    }

    Ok((clean_command_output(&output), usage))
}

fn clean_command_output(output: &str) -> String {
    let trimmed = output.trim();

    // Extract from <cmd>...</cmd> tags (use LAST one for agentic CLIs that iterate)
    if let Some(start) = trimmed.rfind("<cmd>") {
        if let Some(end) = trimmed[start..].find("</cmd>") {
            let cmd_start = start + 5; // len("<cmd>")
            let cmd_end = start + end;
            if cmd_start < cmd_end {
                return trimmed[cmd_start..cmd_end].trim().to_string();
            }
        }
    }

    // Remove markdown code blocks
    if trimmed.starts_with("```") {
        let lines: Vec<&str> = trimmed.lines().collect();
        if lines.len() >= 2 {
            // Skip first line (```bash or similar) and last line (```)
            let end = if lines.last() == Some(&"```") {
                lines.len() - 1
            } else {
                lines.len()
            };
            return lines[1..end].join("\n").trim().to_string();
        }
    }

    // Remove inline backticks
    if trimmed.starts_with('`') && trimmed.ends_with('`') {
        return trimmed[1..trimmed.len()-1].to_string();
    }

    trimmed.to_string()
}

/// Wait for a child process and collect resource usage via wait4
fn wait_with_rusage(pid: i32) -> (i32, ResourceUsage) {
    use std::mem::MaybeUninit;

    let mut status: i32 = 0;
    let mut rusage = MaybeUninit::<libc::rusage>::uninit();

    // Wait for the specific child and get resource usage
    unsafe {
        libc::wait4(pid, &mut status, 0, rusage.as_mut_ptr());
    }

    let rusage = unsafe { rusage.assume_init() };

    // Convert timeval to microseconds
    let user_time_us = (rusage.ru_utime.tv_sec as u64) * 1_000_000 + (rusage.ru_utime.tv_usec as u64);
    let system_time_us = (rusage.ru_stime.tv_sec as u64) * 1_000_000 + (rusage.ru_stime.tv_usec as u64);

    // ru_maxrss is in kilobytes on Linux
    let max_rss_kb = rusage.ru_maxrss as u64;

    let exit_code = if libc::WIFEXITED(status) {
        libc::WEXITSTATUS(status)
    } else {
        -1
    };

    (exit_code, ResourceUsage {
        user_time_us,
        system_time_us,
        max_rss_kb,
    })
}
