# Palimpsest

A TUI for exploring the git history of individual files.

## Project Overview

**Name**: palimpsest (a manuscript page that has been written on, erased, and reused)

**Purpose**: Browse historical versions of a single file through git history with an intuitive horizontal timeline interface.

## Core Features

- Open a specific file and view its git history
- Horizontal scroll through commits (left = back in time, right = forward)
- Diff view for adjacent commits showing changes clearly
- Follow file renames/moves through git history

## Tech Stack

- **Language**: Rust
- **TUI**: ratatui + crossterm
- **Git**: git2-rs

## Architecture

```
src/
  main.rs          - Entry point, CLI args
  app.rs           - Application state and event loop
  ui.rs            - Rendering logic
  git.rs           - Git history/diff operations
  history.rs       - File history navigation
```

## Key Patterns

- Keep state management simple and centralized in App struct
- Separate git operations from UI rendering
- Use git2's follow functionality for tracking renames

## Build & Run

```bash
cargo build --release
./target/release/palimpsest <file_path>
```

## Development Notes

- Focus on single-file exploration first
- Prioritize readability of diffs over feature completeness
