use std::env;
use std::fs;
use std::io::IsTerminal;
use std::path::Path;
use std::process::Command;

#[derive(Debug, Default)]
pub struct Context {
    pub cwd: String,
    pub git_branch: Option<String>,
    pub git_repo: Option<String>,
    pub history: Vec<String>,
    pub last_exit: Option<i32>,
    pub tmux_content: Option<String>,
    pub stdin_is_pipe: bool,
}

impl Context {
    pub fn gather(last_exit: Option<i32>) -> Self {
        let cwd = env::current_dir()
            .map(|p| p.display().to_string())
            .unwrap_or_else(|_| ".".to_string());

        let (git_branch, git_repo) = Self::get_git_info();
        let history = Self::get_shell_history(5);
        let tmux_content = Self::get_tmux_content();
        let stdin_is_pipe = !std::io::stdin().is_terminal();

        Context {
            cwd,
            git_branch,
            git_repo,
            history,
            last_exit,
            tmux_content,
            stdin_is_pipe,
        }
    }

    fn get_git_info() -> (Option<String>, Option<String>) {
        // Get current branch
        let branch = Command::new("git")
            .args(["branch", "--show-current"])
            .output()
            .ok()
            .filter(|o| o.status.success())
            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
            .filter(|s| !s.is_empty());

        // Get repo name from remote or directory
        let repo = Command::new("git")
            .args(["rev-parse", "--show-toplevel"])
            .output()
            .ok()
            .filter(|o| o.status.success())
            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
            .and_then(|path| {
                Path::new(&path)
                    .file_name()
                    .map(|n| n.to_string_lossy().to_string())
            });

        (branch, repo)
    }

    fn get_shell_history(count: usize) -> Vec<String> {
        let history_file = dirs::home_dir()
            .map(|h| h.join(".zsh_history"))
            .filter(|p| p.exists());

        let Some(path) = history_file else {
            return Vec::new();
        };

        let Ok(content) = fs::read_to_string(&path) else {
            return Vec::new();
        };

        // Parse zsh history format (handles extended history format)
        content
            .lines()
            .rev()
            .filter_map(|line| {
                // zsh extended history format: : timestamp:0;command
                if line.starts_with(':') {
                    line.split_once(';').map(|(_, cmd)| cmd.to_string())
                } else {
                    Some(line.to_string())
                }
            })
            .filter(|cmd| !cmd.trim().is_empty())
            .take(count)
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .collect()
    }

    fn get_tmux_content() -> Option<String> {
        // Check if we're in tmux
        if env::var("TMUX").is_err() {
            return None;
        }

        // Capture the current pane content (visible area + some scrollback)
        Command::new("tmux")
            .args(["capture-pane", "-p", "-S", "-50"])
            .output()
            .ok()
            .filter(|o| o.status.success())
            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
            .filter(|s| !s.is_empty())
    }

    pub fn format_for_prompt(&self) -> String {
        self.format_internal(true)
    }

    pub fn format_for_display(&self) -> String {
        self.format_internal(false)
    }

    fn format_internal(&self, truncate: bool) -> String {
        let mut parts = Vec::new();

        parts.push(format!("Working directory: {}", self.cwd));

        if let (Some(repo), Some(branch)) = (&self.git_repo, &self.git_branch) {
            parts.push(format!("Git repo: {} (branch: {})", repo, branch));
        }

        if !self.history.is_empty() {
            parts.push(format!("Recent commands:\n{}", self.history.join("\n")));
        }

        if let Some(exit) = self.last_exit {
            parts.push(format!("Last command exit code: {}", exit));
        }

        if self.stdin_is_pipe {
            parts.push("Stdin: Data is being piped in - generate a command that reads from stdin".to_string());
        }

        if let Some(tmux) = &self.tmux_content {
            let content = if truncate && tmux.len() > 2000 {
                // Truncate at char boundary, not byte boundary
                let mut end = 2000;
                while !tmux.is_char_boundary(end) && end > 0 {
                    end -= 1;
                }
                format!("{}...(truncated)", &tmux[..end])
            } else {
                tmux.clone()
            };
            parts.push(format!("Terminal content:\n{}", content));
        }

        parts.join("\n\n")
    }
}
