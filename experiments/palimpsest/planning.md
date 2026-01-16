# Palimpsest Implementation Plan

## Phase 1: Core Infrastructure

1. **Project Setup**
   - Initialize Cargo project with dependencies
   - ratatui, crossterm, git2, clap

2. **Git Module** (`src/git.rs`)
   - Load file history with `git log --follow`
   - Get file content at specific commit
   - Generate diff between adjacent commits
   - Track renames via git2's diff options

3. **App State** (`src/app.rs`)
   - Current file path
   - List of commits touching the file
   - Current position in history
   - View mode (normal / diff)

## Phase 2: TUI

4. **UI Rendering** (`src/ui.rs`)
   - Header: file path, current commit info
   - Main area: file content or diff view
   - Footer: navigation hints
   - Syntax highlighting (later)

5. **Navigation**
   - Left/Right: move through history
   - Adjacent commits show diff automatically
   - q: quit

## Data Structures

```rust
struct Commit {
    hash: String,
    message: String,
    author: String,
    date: DateTime,
    file_path: String,  // may differ due to renames
}

struct App {
    file_path: String,
    commits: Vec<Commit>,
    current_index: usize,
    content_cache: HashMap<String, String>,
}
```

## Key Behaviors

- On startup: load all commits for file with --follow
- Display current version's content
- When moving left/right to adjacent commit, show diff
- Cache file contents to avoid repeated git operations
