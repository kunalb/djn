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
use llm::{build_prompt, generate_command, Provider};

const ZSH_INIT: &str = r#"x() {
    local last_exit=$?
    if [[ $# -eq 0 ]]; then
        echo "Usage: x <natural language request>" >&2
        return 1
    fi
    local histfile="/tmp/x-hist-$$"
    command x --last-exit="$last_exit" --hist-file="$histfile" "$@"
    local ret=$?
    if [[ -f "$histfile" ]]; then
        local cmd=$(<"$histfile")
        [[ -n "$cmd" ]] && print -s "$cmd"
        rm -f "$histfile"
    fi
    return $ret
}"#;

const BASH_INIT: &str = r#"x() {
    local last_exit=$?
    if [[ $# -eq 0 ]]; then
        echo "Usage: x <natural language request>" >&2
        return 1
    fi
    local histfile="/tmp/x-hist-$$"
    command x --last-exit="$last_exit" --hist-file="$histfile" "$@"
    local ret=$?
    if [[ -f "$histfile" ]]; then
        local cmd=$(<"$histfile")
        [[ -n "$cmd" ]] && history -s "$cmd"
        rm -f "$histfile"
    fi
    return $ret
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

    // Handle --config flag
    if cli.config {
        if let Err(e) = Config::edit() {
            eprintln!("\x1b[31merror:\x1b[0m {}", e);
            std::process::exit(1);
        }
        return;
    }

    let config = Config::load();

    // Gather context
    let context = Context::gather(cli.last_exit, cli.tmux);

    // Debug: show context and prompt
    if cli.context {
        if cli.request.is_empty() {
            // Just show context (full, not truncated)
            eprintln!("\x1b[1m=== Context ===\x1b[0m\n");
            eprintln!("{}", context.format_for_display());
        } else {
            // Show full prompt that would be sent
            let request = cli.request_string();
            eprintln!("\x1b[1m=== Prompt ===\x1b[0m\n");
            eprintln!("{}", build_prompt(&request, &context));
        }
        return;
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

    // Build spinner message with provider/model info
    let model_display = model_ref
        .map(|m| format!("{}/{}", provider.display_name(), m))
        .unwrap_or_else(|| provider.display_name().to_string());

    // Generate command with optional refinement loop
    let mut request = cli.request_string();
    let mut prev_command: Option<String> = None;

    loop {
        // Build the full request including refinement context
        let full_request = if let Some(ref prev) = prev_command {
            format!("{}\n\nPrevious command: {}", request, prev)
        } else {
            request.clone()
        };

        // Show spinner while generating (unless dry-run for cleaner output)
        let spinner = if cli.dry_run {
            None
        } else {
            Some(Spinner::new(&model_display))
        };

        let command = match generate_command(provider, model_ref, &full_request, &context) {
            Ok(cmd) => {
                if let Some(s) = spinner {
                    let _ = s.stop();
                }
                cmd
            }
            Err(e) => {
                if let Some(s) = spinner {
                    s.stop_error();
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
            // Auto-run mode - show command and run
            eprintln!("│ \x1b[1m{}\x1b[0m", command);
            eprintln!("└");
            eprintln!();
            write_hist_file(&cli.hist_file, &command);
            let exit_code = execute_command(&command);
            std::process::exit(exit_code);
        }

        // Interactive confirmation
        match confirm_command(&command) {
            ConfirmResult::Yes => {
                write_hist_file(&cli.hist_file, &command);
                let exit_code = execute_command(&command);
                std::process::exit(exit_code);
            }
            ConfirmResult::Edit(edited) => {
                write_hist_file(&cli.hist_file, &edited);
                let exit_code = execute_command(&edited);
                std::process::exit(exit_code);
            }
            ConfirmResult::No => {
                std::process::exit(0);
            }
            ConfirmResult::Refine(instructions) => {
                // Update request with refinement and loop
                prev_command = Some(command);
                request = instructions;
            }
        }
    }
}

fn write_hist_file(path: &Option<String>, command: &str) {
    if let Some(p) = path {
        let _ = std::fs::write(p, command);
    }
}
