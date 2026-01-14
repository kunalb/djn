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
}

/// Spinner that runs in a background thread
pub struct Spinner {
    running: Arc<AtomicBool>,
    handle: Option<thread::JoinHandle<()>>,
    start: std::time::Instant,
    message: String,
}

impl Spinner {
    pub fn new(message: &str) -> Self {
        let running = Arc::new(AtomicBool::new(true));
        let running_clone = running.clone();
        let message_clone = message.to_string();

        let handle = thread::spawn(move || {
            let frames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
            let mut i = 0;

            while running_clone.load(Ordering::Relaxed) {
                eprint!("\r\x1b[90m{} {}\x1b[0m\x1b[K", frames[i], message_clone);
                let _ = io::stderr().flush();
                i = (i + 1) % frames.len();
                thread::sleep(Duration::from_millis(80));
            }
        });

        Spinner {
            running,
            handle: Some(handle),
            start: std::time::Instant::now(),
            message: message.to_string(),
        }
    }

    pub fn stop(self) -> Duration {
        let elapsed = self.start.elapsed();
        self.running.store(false, Ordering::Relaxed);
        if let Some(handle) = self.handle {
            let _ = handle.join();
        }
        // Replace spinner with completed message
        let duration_str = format_duration(elapsed);
        eprint!("\r\x1b[90m✓ {} ({})\x1b[0m\x1b[K\n", self.message, duration_str);
        let _ = io::stderr().flush();
        elapsed
    }

    pub fn stop_error(self) {
        self.running.store(false, Ordering::Relaxed);
        if let Some(handle) = self.handle {
            let _ = handle.join();
        }
        // Replace spinner with error indicator
        eprint!("\r\x1b[90m✗ {}\x1b[0m\x1b[K\n", self.message);
        let _ = io::stderr().flush();
    }
}

fn format_duration(d: Duration) -> String {
    let ms = d.as_millis();
    if ms < 1000 {
        format!("{}ms", ms)
    } else {
        format!("{:.1}s", d.as_secs_f64())
    }
}

pub fn confirm_command(command: &str) -> ConfirmResult {
    // Display the command in a clean format
    eprintln!();
    eprintln!("  \x1b[1;32m❯\x1b[0m \x1b[1m{}\x1b[0m", command);
    eprintln!();

    loop {
        // Subtle inline prompt
        eprint!("\x1b[90mrun? [Y/n/e] \x1b[0m");
        io::stderr().flush().unwrap();

        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            return ConfirmResult::No;
        }

        match input.trim().to_lowercase().as_str() {
            "y" | "yes" | "" => {
                // Move up and clear the prompt, then show running indicator
                eprint!("\x1b[1A\x1b[K");
                eprintln!("  \x1b[1;32m❯\x1b[0m \x1b[1m{}\x1b[0m", command);
                return ConfirmResult::Yes;
            }
            "n" | "no" | "q" => {
                eprint!("\x1b[1A\x1b[K");
                eprintln!("  \x1b[90m❯ {}\x1b[0m \x1b[90m(cancelled)\x1b[0m", command);
                return ConfirmResult::No;
            }
            "e" | "edit" => {
                if let Some(edited) = edit_command(command) {
                    return ConfirmResult::Edit(edited);
                }
                // Show original again
                eprintln!();
                eprintln!("  \x1b[1;32m❯\x1b[0m \x1b[1m{}\x1b[0m", command);
                eprintln!();
            }
            _ => {
                // Clear invalid input, stay on same line
                eprint!("\x1b[1A\x1b[K");
            }
        }
    }
}

fn edit_command(command: &str) -> Option<String> {
    // Clear the prompt line and show edit mode
    eprint!("\x1b[1A\x1b[K");
    eprint!("  \x1b[1;33m❯\x1b[0m ");
    io::stderr().flush().unwrap();

    // Try to use readline-style editing by pre-filling
    // For now, simple input with the original shown
    eprintln!("\x1b[90m(editing, enter new command or empty to cancel)\x1b[0m");
    eprint!("  \x1b[1;33m❯\x1b[0m ");
    io::stderr().flush().unwrap();

    let mut input = String::new();
    if io::stdin().read_line(&mut input).is_err() {
        return None;
    }

    let edited = input.trim();
    if edited.is_empty() {
        None
    } else {
        // Show what will run
        eprint!("\x1b[1A\x1b[K");
        eprintln!("  \x1b[1;32m❯\x1b[0m \x1b[1m{}\x1b[0m", edited);
        Some(edited.to_string())
    }
}

pub fn execute_command(command: &str) -> i32 {
    let status = Command::new("sh")
        .arg("-c")
        .arg(command)
        .status();

    match status {
        Ok(s) => {
            let code = s.code().unwrap_or(1);
            if code != 0 {
                eprintln!("\x1b[90mexit {}\x1b[0m", code);
            }
            code
        }
        Err(e) => {
            eprintln!("\x1b[1;31mfailed: {}\x1b[0m", e);
            1
        }
    }
}

/// Print command for dry-run mode (outputs to stdout for shell wrapper to capture)
pub fn print_command(command: &str) {
    println!("{}", command);
}
