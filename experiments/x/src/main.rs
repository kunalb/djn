mod cli;
mod config;
mod context;
mod executor;
mod history;
mod llm;

use clap::Parser;

use cli::Cli;
use config::Config;
use context::Context;
use executor::{confirm_command, execute_command, print_command, ConfirmResult, Spinner};
use history::{History, HistoryEntry, Outcome};
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

    // Open history database (non-fatal if it fails)
    let history = History::open().ok();

    // Generate session ID for tracking refinements
    let session_id = uuid::Uuid::new_v4().to_string();

    // Generate command with optional refinement loop
    let mut request = cli.request_string();
    let mut prev_command: Option<String> = None;
    let mut prev_generation_id: Option<i64> = None;

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

        let (command, duration_ms) = match generate_command(provider, model_ref, &full_request, &context) {
            Ok(cmd) => {
                let duration = spinner.map(|s| s.stop()).unwrap_or_default();
                (cmd, duration.as_millis() as u64)
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

        // Helper to record history
        let record = |outcome: Outcome, edited: Option<&str>, exit_code: Option<i32>| {
            if let Some(ref h) = history {
                let _ = h.record(&HistoryEntry {
                    cwd: context.cwd.clone(),
                    request: request.clone(),
                    provider: provider.display_name().to_string(),
                    model: model_ref.map(|s| s.to_string()),
                    duration_ms,
                    command: command.clone(),
                    outcome,
                    edited_command: edited.map(|s| s.to_string()),
                    session_id: session_id.clone(),
                    refinement_of: prev_generation_id,
                    exit_code,
                });
            }
        };

        if cli.yes {
            // Auto-run mode - show command and run
            eprintln!("│ \x1b[1m{}\x1b[0m", command);
            eprintln!("└");
            eprintln!();
            write_hist_file(&cli.hist_file, &command, context.stdin_is_pipe);
            let exit_code = execute_command(&command);
            record(Outcome::Executed, None, Some(exit_code));
            std::process::exit(exit_code);
        }

        // Interactive confirmation
        match confirm_command(&command) {
            ConfirmResult::Yes => {
                write_hist_file(&cli.hist_file, &command, context.stdin_is_pipe);
                let exit_code = execute_command(&command);
                record(Outcome::Executed, None, Some(exit_code));
                std::process::exit(exit_code);
            }
            ConfirmResult::Edit(edited) => {
                write_hist_file(&cli.hist_file, &edited, context.stdin_is_pipe);
                let exit_code = execute_command(&edited);
                record(Outcome::Edited, Some(&edited), Some(exit_code));
                std::process::exit(exit_code);
            }
            ConfirmResult::No => {
                record(Outcome::Cancelled, None, None);
                std::process::exit(0);
            }
            ConfirmResult::Refine(instructions) => {
                // Record the refinement, capturing the generation ID for linking
                if let Some(ref h) = history {
                    if let Ok(id) = h.record(&HistoryEntry {
                        cwd: context.cwd.clone(),
                        request: request.clone(),
                        provider: provider.display_name().to_string(),
                        model: model_ref.map(|s| s.to_string()),
                        duration_ms,
                        command: command.clone(),
                        outcome: Outcome::Refined,
                        edited_command: None,
                        session_id: session_id.clone(),
                        refinement_of: prev_generation_id,
                        exit_code: None,
                    }) {
                        prev_generation_id = Some(id);
                    }
                }
                // Update for next iteration
                prev_command = Some(command);
                request = instructions;
            }
        }
    }
}

fn write_hist_file(path: &Option<String>, command: &str, piped: bool) {
    if let Some(p) = path {
        // When piped, prefix with comment so user can edit but won't accidentally run
        let hist_cmd = if piped {
            format!("# piped | {}", command)
        } else {
            command.to_string()
        };
        let _ = std::fs::write(p, hist_cmd);
    }
}
