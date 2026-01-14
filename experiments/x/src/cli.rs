use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "x")]
#[command(about = "Fast terminal LLM command generator")]
#[command(version)]
pub struct Cli {
    /// The natural language request for command generation
    #[arg(trailing_var_arg = true, required_unless_present_any = ["init", "context", "config"])]
    pub request: Vec<String>,

    /// Print shell initialization script (add `eval "$(x --init zsh)"` to .zshrc)
    #[arg(long, value_name = "SHELL")]
    pub init: Option<String>,

    /// Open config file in $EDITOR (creates template if missing)
    #[arg(long)]
    pub config: bool,

    /// Skip confirmation and run immediately
    #[arg(short = 'y', long)]
    pub yes: bool,

    /// Only print the generated command, don't run
    #[arg(short = 'n', long)]
    pub dry_run: bool,

    /// LLM provider to use (claude, gemini, openai)
    #[arg(short, long)]
    pub provider: Option<String>,

    /// Model to use (opus, sonnet, gpt-4o, etc.)
    #[arg(short, long)]
    pub model: Option<String>,

    /// Show captured context (for debugging)
    #[arg(long)]
    pub context: bool,

    /// Last command exit code (passed from shell wrapper)
    #[arg(long, hide = true)]
    pub last_exit: Option<i32>,
}

impl Cli {
    pub fn request_string(&self) -> String {
        self.request.join(" ")
    }
}
