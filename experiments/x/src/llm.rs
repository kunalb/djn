use std::process::Command;

use crate::context::Context;

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

pub const SYSTEM_PROMPT: &str = r#"You are a shell command generator. Given the user's request and context, output ONLY the shell command(s) to execute.

Rules:
- Output ONLY the command, nothing else
- No explanations, no markdown, no code blocks
- For multiple commands, separate with && or use appropriate shell constructs
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
) -> Result<String, String> {
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

fn generate_with_claude(prompt: &str, model: Option<&str>) -> Result<String, String> {
    let exe = Provider::Claude.find_executable()
        .ok_or_else(|| "claude CLI not found".to_string())?;

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

    run_command(cmd, "claude")
}

fn generate_with_gemini(prompt: &str, model: Option<&str>) -> Result<String, String> {
    use std::io::Write;

    let exe = Provider::Gemini.find_executable()
        .ok_or_else(|| "gemini CLI not found".to_string())?;

    let mut cmd = Command::new(&exe);
    if let Some(m) = model {
        cmd.args(["-m", m]);
    }

    let mut child = cmd
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::null())
        .spawn()
        .map_err(|e| format!("Failed to run gemini: {}", e))?;

    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(prompt.as_bytes());
    }

    let output = child.wait_with_output()
        .map_err(|e| format!("Failed to wait for gemini: {}", e))?;

    if !output.status.success() {
        return Err("gemini failed".to_string());
    }

    let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();
    Ok(clean_command_output(&stdout))
}

fn generate_with_openai(prompt: &str, model: Option<&str>) -> Result<String, String> {
    let exe = Provider::OpenAI.find_executable()
        .ok_or_else(|| "codex CLI not found".to_string())?;

    // Use temp file for output (codex duplicates when using /dev/stdout)
    let tmp_file = format!("/tmp/x-codex-{}", std::process::id());

    let mut cmd = Command::new(&exe);
    cmd.args(["exec", "-o", &tmp_file]);
    if let Some(m) = model {
        cmd.args(["-m", m]);
    }
    cmd.arg(prompt);

    // Codex writes UI to stderr, suppress it
    cmd.stderr(std::process::Stdio::null());
    cmd.stdout(std::process::Stdio::null());

    let status = cmd
        .stdin(std::process::Stdio::null())
        .status()
        .map_err(|e| format!("Failed to run codex: {}", e))?;

    let output = std::fs::read_to_string(&tmp_file)
        .unwrap_or_default();
    let _ = std::fs::remove_file(&tmp_file);

    if !status.success() {
        return Err("codex failed".to_string());
    }

    Ok(clean_command_output(&output))
}

fn run_command(mut cmd: Command, name: &str) -> Result<String, String> {
    let output = cmd
        .stdin(std::process::Stdio::null())
        .output()
        .map_err(|e| format!("Failed to run {}: {}", name, e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("{} failed: {}", name, stderr));
    }

    let stdout = String::from_utf8_lossy(&output.stdout)
        .trim()
        .to_string();

    // Clean up the output - remove markdown code blocks if present
    let cleaned = clean_command_output(&stdout);

    Ok(cleaned)
}

fn clean_command_output(output: &str) -> String {
    let trimmed = output.trim();

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
