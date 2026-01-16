use clap::Parser;

// Build version string with git hash and timestamp
const VERSION: &str = concat!(
    env!("CARGO_PKG_VERSION"),
    " (",
    env!("BUILD_GIT_HASH"),
    " ",
    env!("BUILD_TIMESTAMP"),
    ")"
);

#[derive(Parser, Debug)]
#[command(name = "x")]
#[command(about = "Fast terminal LLM command generator")]
#[command(after_help = "Built with Claude")]
#[command(version = VERSION)]
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

    /// Include tmux terminal content in context
    #[arg(short = 't', long)]
    pub tmux: bool,

    /// Last command exit code (passed from shell wrapper)
    #[arg(long, hide = true)]
    pub last_exit: Option<i32>,

    /// File to write executed command to (for shell history integration)
    #[arg(long, hide = true)]
    pub hist_file: Option<String>,
}

impl Cli {
    pub fn request_string(&self) -> String {
        self.request.join(" ")
    }
}
