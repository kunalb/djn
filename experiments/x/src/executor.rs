use std::io::{self, Write};
use std::process::Command;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Duration;

pub enum ConfirmResult {
    Yes,
    No,
    Edit(String),
    Refine(String),
}

/// Timer that runs in a background thread showing elapsed time
pub struct Spinner {
    running: Arc<AtomicBool>,
    handle: Option<thread::JoinHandle<()>>,
    start: std::time::Instant,
    model: String,
}

impl Spinner {
    pub fn new(model: &str) -> Self {
        let running = Arc::new(AtomicBool::new(true));
        let running_clone = running.clone();
        let model_clone = model.to_string();
        let start = std::time::Instant::now();
        let start_clone = start;

        let handle = thread::spawn(move || {
            while running_clone.load(Ordering::Relaxed) {
                let elapsed = start_clone.elapsed().as_secs_f64();
                eprint!("\r- {} ({:.1}s) \x1b[K", model_clone, elapsed);
                let _ = io::stderr().flush();
                thread::sleep(Duration::from_millis(100));
            }
        });

        Spinner {
            running,
            handle: Some(handle),
            start,
            model: model.to_string(),
        }
    }

    pub fn stop(self) -> Duration {
        let elapsed = self.start.elapsed();
        self.running.store(false, Ordering::Relaxed);
        if let Some(handle) = self.handle {
            let _ = handle.join();
        }
        // Clear timer line, print box header
        eprint!("\r\x1b[K┌ {} ({:.1}s)\n", self.model, elapsed.as_secs_f64());
        let _ = io::stderr().flush();
        elapsed
    }

    pub fn stop_error(self) {
        self.running.store(false, Ordering::Relaxed);
        if let Some(handle) = self.handle {
            let _ = handle.join();
        }
        // Clear timer line, show error
        eprint!("\r\x1b[K\x1b[31merror:\x1b[0m ");
        let _ = io::stderr().flush();
    }
}

pub fn confirm_command(command: &str) -> ConfirmResult {
    // Display command in box (bold)
    eprintln!("│ \x1b[1m{}\x1b[0m", command);

    loop {
        // Prompt on same line with space for cursor
        eprint!("└ [Y/n/e] ");
        io::stderr().flush().unwrap();

        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            return ConfirmResult::No;
        }

        let trimmed = input.trim();
        match trimmed.to_lowercase().as_str() {
            "y" | "yes" | "" => {
                eprintln!(); // blank line before output
                return ConfirmResult::Yes;
            }
            "n" | "no" | "q" => {
                return ConfirmResult::No;
            }
            "e" | "edit" => {
                if let Some(edited) = edit_command(command) {
                    eprintln!(); // blank line before output
                    return ConfirmResult::Edit(edited);
                }
                // Show command again for retry
                eprintln!("│ \x1b[1m{}\x1b[0m", command);
            }
            _ => {
                // Treat as refinement instructions
                return ConfirmResult::Refine(trimmed.to_string());
            }
        }
    }
}

fn edit_command(_command: &str) -> Option<String> {
    // Clear prompt line
    eprint!("\x1b[1A\x1b[K");
    eprint!("└ ");
    io::stderr().flush().unwrap();

    let mut input = String::new();
    if io::stdin().read_line(&mut input).is_err() {
        return None;
    }

    let edited = input.trim();
    if edited.is_empty() {
        None
    } else {
        Some(edited.to_string())
    }
}

pub fn execute_command(command: &str) -> i32 {
    // Use user's shell for proper builtin support (e.g., type -a)
    let shell = std::env::var("SHELL").unwrap_or_else(|_| "sh".to_string());
    let status = Command::new(&shell)
        .arg("-c")
        .arg(command)
        .status();

    match status {
        Ok(s) => {
            let code = s.code().unwrap_or(1);
            if code != 0 {
                eprintln!("exit {}", code);
            }
            code
        }
        Err(e) => {
            eprintln!("\x1b[31merror:\x1b[0m {}", e);
            1
        }
    }
}

/// Print command for dry-run mode
pub fn print_command(command: &str) {
    println!("{}", command);
}
