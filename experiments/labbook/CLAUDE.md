# Lab - Development Guide

## Overview

Lab is a CLI tool for capturing experiment snapshots alongside code repositories. See `DESIGN.md` for the full specification.

## Architecture

Loosely coupled modules with dependencies flowing downward only:

```
main.rs → commands → {config, git, experiment, artifacts, editor}
```

### Core Modules (independent, no cross-dependencies)

- **config** - Load/save `.lab/config.yaml`, validation
- **git** - Git operations via shell (status, commit, branch detection)
- **experiment** - Experiment structs, `meta.yaml` serialization, ID generation
- **artifacts** - File copying, glob pattern matching
- **editor** - `$EDITOR` invocation, change detection

### Command Modules (compose core modules)

Each command in `src/commands/` implements one CLI subcommand.

## Principles

1. **Simplicity over features** - Do one thing well, no premature abstraction
2. **Explicit over implicit** - Clear data flow, no magic
3. **Test everything** - Unit tests for core, e2e bash tests for workflows
4. **Fail loudly** - Clear error messages, no silent failures
5. **Loose coupling** - Core modules must not import from each other

## Git Integration

Shell out to `git` rather than using libgit2:
- Simpler, fewer dependencies
- Respects user's git config and credentials
- Use `--porcelain` flags when parsing output for stability

## Error Handling

Use `anyhow` for error propagation. Provide context with `.context()`:

```rust
fs::read_to_string(&path)
    .with_context(|| format!("failed to read config: {}", path.display()))?;
```

## Development Workflow

1. Write/modify code
2. `cargo clippy` - fix all warnings
3. `cargo test` - unit tests must pass
4. `cargo build && ./tests/e2e/test_workflow.sh` - e2e tests must pass
5. Make a small, coherent commit
6. Repeat

## Compiler & Lints

All code must compile with zero warnings. In `Cargo.toml`:

```toml
[lints.rust]
unsafe_code = "forbid"

[lints.clippy]
all = "warn"
pedantic = "warn"
```

Fix all clippy warnings before committing.

## Testing Strategy

### Unit Tests

In each module, test pure functions and edge cases:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_experiment_id_generation() {
        // ...
    }
}
```

### E2E Tests

Bash scripts in `tests/e2e/` exercise the full CLI workflow:

```bash
cargo build
./tests/e2e/test_workflow.sh
```

The bash tests should:
- Create a temporary git repo
- Run `lab init`, `lab open`, etc.
- Assert on file contents and command outputs
- Clean up after themselves

## Commit Guidelines

- Each commit is one coherent change
- Tests must pass before committing
- Format: `module: description`
  - `config: add yaml validation`
  - `git: use --porcelain for status parsing`
  - `e2e: test snapshot workflow`
  - `cli: add --name flag to open command`

## Dependencies

Minimal external dependencies:

```toml
[dependencies]
clap = { version = "4", features = ["derive"] }
serde = { version = "1", features = ["derive"] }
serde_yaml = "0.9"
anyhow = "1"
chrono = { version = "0.4", features = ["serde"] }
glob = "0.3"
```

Avoid adding dependencies unless strictly necessary. Justify any new dependency.

## Project Structure

```
src/
  main.rs              # CLI entry, clap setup
  lib.rs               # Re-exports for integration tests
  config.rs            # .lab/config.yaml
  git.rs               # Git shell operations
  experiment.rs        # Experiment data, meta.yaml
  artifacts.rs         # File copying, globs
  editor.rs            # $EDITOR handling
  commands/
    mod.rs
    init.rs
    open.rs
    status.rs
    snapshot.rs
    add.rs
    edit.rs
    close.rs
    list.rs

tests/
  e2e/
    test_workflow.sh   # Full workflow test
    helpers.sh         # Shared test utilities
```

## Implementation Phases

### Phase 1: Core Workflow
- `lab init` - Setup `.lab/` config, initialize lab folder
- `lab open [name]` - Create experiment, open editor
- `lab status` - Show current experiment
- `lab snapshot` - Record git state + artifacts
- `lab add <files>` - Add artifacts
- `lab edit` - Edit notes
- `lab close` - Close experiment
- `lab list` - Simple list (fzf later)

### Phase 2: Enhanced Features
- `lab view` - Web server for browsing (design incrementally)
- fzf integration for `lab list`
- Additional filtering/search

## Common Patterns

### Reading Config

```rust
let config = Config::load()?;  // looks for .lab/config.yaml
```

### Git Operations

```rust
let status = git::status()?;  // uses --porcelain
if status.is_dirty() {
    // ...
}
```

### Committing to Lab Repo

```rust
git::commit_in_repo(&lab_dir, &format!("open: {}", experiment_id))?;
```
