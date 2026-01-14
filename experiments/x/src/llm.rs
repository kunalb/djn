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
        // Use user's shell interactively to check, so aliases/functions are resolved
        let shell = std::env::var("SHELL").unwrap_or_else(|_| "sh".to_string());
        Command::new(&shell)
            .args(["-ic", &format!("command -v {} >/dev/null 2>&1", self.cli_name())])
            .status()
            .map(|s| s.success())
            .unwrap_or(false)
    }

    pub fn default_model(&self) -> &'static str {
        match self {
            Provider::Claude => "claude-sonnet-4-20250514",
            Provider::Gemini => "gemini-2.0-flash",
            Provider::OpenAI => "gpt-4o",
        }
    }
}

const SYSTEM_PROMPT: &str = r#"You are a shell command generator. Given the user's request and context, output ONLY the shell command(s) to execute.

Rules:
- Output ONLY the command, nothing else
- No explanations, no markdown, no code blocks
- For multiple commands, separate with && or use appropriate shell constructs
- Use the context to understand what the user wants
- If the request references "previous command" or "last command", use the history context
- If fixing an error, look at the terminal content for error messages"#;

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
    // Map short names to full model names
    let model_arg = model.map(|m| match m {
        "opus" => "claude-opus-4-20250514",
        "sonnet" => "claude-sonnet-4-20250514",
        _ => m,
    });

    let model_flag = model_arg
        .map(|m| format!(" --model {}", shell_escape(m)))
        .unwrap_or_default();

    let shell_cmd = format!("claude --print {}{}", shell_escape(prompt), model_flag);
    run_shell_command(&shell_cmd, "claude")
}

fn generate_with_gemini(prompt: &str, model: Option<&str>) -> Result<String, String> {
    let model_flag = model
        .map(|m| format!(" -m {}", shell_escape(m)))
        .unwrap_or_default();

    let shell_cmd = format!("gemini{} {}", model_flag, shell_escape(prompt));
    run_shell_command(&shell_cmd, "gemini")
}

fn generate_with_openai(prompt: &str, model: Option<&str>) -> Result<String, String> {
    // Use codex CLI (OpenAI's official CLI)
    let model_flag = model
        .map(|m| format!(" -m {}", shell_escape(m)))
        .unwrap_or_default();

    let shell_cmd = format!("codex exec -o /dev/stdout{} {}", model_flag, shell_escape(prompt));
    run_shell_command(&shell_cmd, "codex")
}

fn shell_escape(s: &str) -> String {
    // Use single quotes and escape any single quotes within
    format!("'{}'", s.replace('\'', "'\\''"))
}

fn run_shell_command(cmd: &str, name: &str) -> Result<String, String> {
    // Use user's shell interactively so aliases/functions are available
    let shell = std::env::var("SHELL").unwrap_or_else(|_| "sh".to_string());
    let output = Command::new(&shell)
        .args(["-ic", cmd])
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
