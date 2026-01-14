mod cli;
mod config;
mod context;
mod executor;
mod llm;

use clap::Parser;

use cli::Cli;
use config::Config;
use context::Context;
use executor::{confirm_command, execute_command, print_command, ConfirmResult, Spinner};
use llm::{generate_command, Provider};

const ZSH_INIT: &str = r#"x() {
    local last_exit=$?
    if [[ $# -eq 0 ]]; then
        echo "Usage: x <natural language request>" >&2
        return 1
    fi
    command x --last-exit="$last_exit" "$@"
}"#;

const BASH_INIT: &str = r#"x() {
    local last_exit=$?
    if [[ $# -eq 0 ]]; then
        echo "Usage: x <natural language request>" >&2
        return 1
    fi
    command x --last-exit="$last_exit" "$@"
}"#;

fn main() {
    let cli = Cli::parse();

    // Handle --init flag (fast path, no config/context needed)
    if let Some(shell) = &cli.init {
        match shell.as_str() {
            "zsh" => println!("{}", ZSH_INIT),
            "bash" => println!("{}", BASH_INIT),
            _ => {
                eprintln!("Unsupported shell: {}. Supported: zsh, bash", shell);
                std::process::exit(1);
            }
        }
        return;
    }

    let config = Config::load();

    // Gather context
    let context = Context::gather(cli.last_exit);

    // Debug: show context
    if cli.context {
        eprintln!("\x1b[1;33m=== Captured Context ===\x1b[0m");
        eprintln!("{}", context.format_for_prompt());
        eprintln!("\x1b[1;33m========================\x1b[0m");
        // Exit if no request provided (just introspection)
        if cli.request.is_empty() {
            return;
        }
        eprintln!();
    }

    // Determine provider
    let provider_name = cli
        .provider
        .as_deref()
        .unwrap_or(&config.default_provider);

    let provider = match Provider::from_str(provider_name) {
        Some(p) => p,
        None => {
            eprintln!("\x1b[1;31mUnknown provider: {}\x1b[0m", provider_name);
            eprintln!("Available: claude, gemini, openai");
            std::process::exit(1);
        }
    };

    // Check if CLI is available
    if !provider.is_available() {
        eprintln!(
            "\x1b[1;31m{} CLI not found. Please install it first.\x1b[0m",
            provider.cli_name()
        );
        std::process::exit(1);
    }

    // Determine model (CLI flag takes precedence over config)
    let model_str: Option<String> = cli.model.clone().or_else(|| config.get_model(provider_name));
    let model_ref = model_str.as_deref();

    // Generate command
    let request = cli.request_string();

    // Show spinner while generating (unless dry-run for cleaner output)
    let spinner = if cli.dry_run {
        None
    } else {
        Some(Spinner::new("thinking..."))
    };

    let command = match generate_command(provider, model_ref, &request, &context) {
        Ok(cmd) => {
            if let Some(s) = spinner {
                s.stop();
            }
            cmd
        }
        Err(e) => {
            if let Some(s) = spinner {
                s.stop();
            }
            eprintln!("\x1b[1;31merror:\x1b[0m {}", e);
            std::process::exit(1);
        }
    };

    // Handle output based on mode
    if cli.dry_run {
        print_command(&command);
        return;
    }

    if cli.yes {
        // Auto-run mode - show command then run
        eprintln!("  \x1b[1;32m❯\x1b[0m \x1b[1m{}\x1b[0m", command);
        let exit_code = execute_command(&command);
        std::process::exit(exit_code);
    }

    // Interactive confirmation
    match confirm_command(&command) {
        ConfirmResult::Yes => {
            let exit_code = execute_command(&command);
            std::process::exit(exit_code);
        }
        ConfirmResult::Edit(edited) => {
            let exit_code = execute_command(&edited);
            std::process::exit(exit_code);
        }
        ConfirmResult::No => {
            std::process::exit(0);
        }
    }
}
