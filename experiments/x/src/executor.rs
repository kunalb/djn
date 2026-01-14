use std::io::{self, Write};
use std::process::Command;

pub enum ConfirmResult {
    Yes,
    No,
    Edit(String),
}

pub fn confirm_command(command: &str) -> ConfirmResult {
    // Display the command
    println!("\n\x1b[1;36m{}\x1b[0m\n", command);

    loop {
        print!("\x1b[1m[y]es / [n]o / [e]dit > \x1b[0m");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            return ConfirmResult::No;
        }

        match input.trim().to_lowercase().as_str() {
            "y" | "yes" | "" => return ConfirmResult::Yes,
            "n" | "no" => return ConfirmResult::No,
            "e" | "edit" => {
                if let Some(edited) = edit_command(command) {
                    return ConfirmResult::Edit(edited);
                }
                // If editing failed, show the command again
                println!("\n\x1b[1;36m{}\x1b[0m\n", command);
            }
            _ => {
                println!("Please enter y, n, or e");
            }
        }
    }
}

fn edit_command(command: &str) -> Option<String> {
    print!("\x1b[1mEdit command:\x1b[0m ");
    io::stdout().flush().unwrap();

    // Simple inline editing - print the command and let user type a new one
    // For a more sophisticated approach, we could use a readline library
    println!("\x1b[90m(current: {})\x1b[0m", command);
    print!("> ");
    io::stdout().flush().unwrap();

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
        Ok(s) => s.code().unwrap_or(1),
        Err(e) => {
            eprintln!("\x1b[1;31mFailed to execute: {}\x1b[0m", e);
            1
        }
    }
}

/// Print command for dry-run mode (outputs to stdout for shell wrapper to capture)
pub fn print_command(command: &str) {
    println!("{}", command);
}
