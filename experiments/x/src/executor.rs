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
    model: String,
}

impl Spinner {
    pub fn new(model: &str) -> Self {
        let running = Arc::new(AtomicBool::new(true));
        let running_clone = running.clone();
        let model_clone = model.to_string();

        let handle = thread::spawn(move || {
            let frames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
            let mut i = 0;

            while running_clone.load(Ordering::Relaxed) {
                eprint!("\r\x1b[90m┌ {} {}\x1b[0m\x1b[K", model_clone, frames[i]);
                let _ = io::stderr().flush();
                i = (i + 1) % frames.len();
                thread::sleep(Duration::from_millis(80));
            }
        });

        Spinner {
            running,
            handle: Some(handle),
            start: std::time::Instant::now(),
            model: model.to_string(),
        }
    }

    pub fn stop(self) -> Duration {
        let elapsed = self.start.elapsed();
        self.running.store(false, Ordering::Relaxed);
        if let Some(handle) = self.handle {
            let _ = handle.join();
        }
        // Show completed state
        let duration_str = format_duration(elapsed);
        eprint!("\r\x1b[90m┌ {} ({})\x1b[0m\x1b[K\n", self.model, duration_str);
        let _ = io::stderr().flush();
        elapsed
    }

    pub fn stop_error(self) {
        self.running.store(false, Ordering::Relaxed);
        if let Some(handle) = self.handle {
            let _ = handle.join();
        }
        eprint!("\r\x1b[90m┌ {} \x1b[31m✗\x1b[0m\x1b[K\n", self.model);
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
    // Display command in box
    eprintln!("\x1b[90m│\x1b[0m \x1b[1m{}\x1b[0m", command);

    loop {
        // Prompt on same line with space for cursor
        eprint!("\x1b[90m└\x1b[0m \x1b[90m[Y/n/e]\x1b[0m ");
        io::stderr().flush().unwrap();

        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            return ConfirmResult::No;
        }

        match input.trim().to_lowercase().as_str() {
            "y" | "yes" | "" => {
                return ConfirmResult::Yes;
            }
            "n" | "no" | "q" => {
                return ConfirmResult::No;
            }
            "e" | "edit" => {
                if let Some(edited) = edit_command(command) {
                    return ConfirmResult::Edit(edited);
                }
                // Show command again for retry
                eprintln!("\x1b[90m│\x1b[0m \x1b[1m{}\x1b[0m", command);
            }
            _ => {
                // Clear line and retry
                eprint!("\x1b[1A\x1b[K");
            }
        }
    }
}

fn edit_command(_command: &str) -> Option<String> {
    // Clear prompt line
    eprint!("\x1b[1A\x1b[K");
    eprint!("\x1b[90m└\x1b[0m ");
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
            eprintln!("\x1b[31merror:\x1b[0m {}", e);
            1
        }
    }
}

/// Print command for dry-run mode
pub fn print_command(command: &str) {
    println!("{}", command);
}
