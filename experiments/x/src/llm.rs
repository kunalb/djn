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
            "openai" | "gpt" | "chatgpt" => Some(Provider::OpenAI),
            _ => None,
        }
    }

    pub fn cli_name(&self) -> &'static str {
        match self {
            Provider::Claude => "claude",
            Provider::Gemini => "gemini",
            Provider::OpenAI => "openai",
        }
    }

    pub fn is_available(&self) -> bool {
        Command::new("which")
            .arg(self.cli_name())
            .output()
            .map(|o| o.status.success())
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
    let mut cmd = Command::new("claude");
    cmd.args(["--print", prompt]);

    if let Some(m) = model {
        // Map short names to full model names
        let full_model = match m {
            "opus" => "claude-opus-4-20250514",
            "sonnet" => "claude-sonnet-4-20250514",
            _ => m,
        };
        cmd.args(["--model", full_model]);
    }

    run_command(cmd, "claude")
}

fn generate_with_gemini(prompt: &str, model: Option<&str>) -> Result<String, String> {
    let mut cmd = Command::new("gemini");

    if let Some(m) = model {
        cmd.args(["-m", m]);
    }

    cmd.arg(prompt);

    run_command(cmd, "gemini")
}

fn generate_with_openai(prompt: &str, model: Option<&str>) -> Result<String, String> {
    // Try 'openai' CLI first, fall back to other common CLI tools
    let mut cmd = Command::new("openai");
    cmd.args(["api", "chat.completions.create"]);

    if let Some(m) = model {
        cmd.args(["-m", m]);
    }

    cmd.args(["-g", "user", prompt]);

    run_command(cmd, "openai")
}

fn run_command(mut cmd: Command, name: &str) -> Result<String, String> {
    let output = cmd
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
