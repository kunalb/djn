# Lab - Development Guide

## Overview

Lab is a CLI tool for capturing experiment snapshots alongside code repositories. See `DESIGN.md` for the full specification.

## Architecture

Two binaries sharing core modules:

```
lab (CLI)      → commands → {config, git, experiment, artifacts, editor}
lab-view (Web) → routes   → {config, git, experiment, artifacts, render, github}
```

### Core Modules (shared, independent, no cross-dependencies)

- **config** - Load/save `.lab/config.yaml`, validation
- **git** - Git operations via shell (status, commit, branch detection)
- **experiment** - Experiment structs, `meta.yaml` serialization, ID generation
- **artifacts** - File copying, glob pattern matching
- **editor** - `$EDITOR` invocation, change detection

### CLI Command Modules

Each command in `src/commands/` implements one CLI subcommand.

### View Modules (lab-view only)

- **routes** - axum HTTP handlers
- **render** - Markdown rendering, syntax highlighting
- **github** - GitHub remote detection, commit/diff URL generation
- **templates/** - askama HTML templates

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

### Shared (both binaries)

```toml
[dependencies]
clap = { version = "4", features = ["derive"] }
serde = { version = "1", features = ["derive"] }
serde_yaml = "0.9"
anyhow = "1"
chrono = { version = "0.4", features = ["serde"] }
glob = "0.3"
```

### View-only (lab-view binary)

```toml
axum = "0.7"
tokio = { version = "1", features = ["rt-multi-thread", "macros"] }
askama = "0.12"
askama_axum = "0.4"
syntect = "5"
pulldown-cmark = "0.10"
tower-http = { version = "0.5", features = ["fs"] }
```

Avoid adding dependencies unless strictly necessary. Justify any new dependency.

## Project Structure

```
src/
  main.rs              # CLI entry (lab binary)
  lib.rs               # Shared modules re-exported
  config.rs            # .lab/config.yaml
  git.rs               # Git shell operations
  experiment.rs        # Experiment data, meta.yaml
  artifacts.rs         # File copying, globs
  editor.rs            # $EDITOR handling
  commands/            # CLI commands
    mod.rs
    init.rs
    open.rs
    status.rs
    snapshot.rs
    add.rs
    edit.rs
    close.rs
    list.rs
  view/                # lab-view binary
    main.rs            # Server entry, CLI args
    routes.rs          # axum handlers
    render.rs          # Markdown, syntax highlighting
    github.rs          # GitHub URL detection
    templates/
      base.html        # Layout with htmx, pico.css
      list.html        # Experiment table
      detail.html      # Single experiment view
      compare.html     # Multi-experiment comparison
      partials/
        artifact.html  # Artifact rendering by type
        diff.html      # Git diff display
        snapshot.html  # Snapshot row

tests/
  e2e/
    test_workflow.sh   # Full workflow test
    helpers.sh         # Shared test utilities
```

## Implementation Phases

### Phase 1: Core Workflow ✓ (Complete)
- `lab init` - Setup `.lab/` config, initialize lab folder
- `lab open [name]` - Create experiment, open editor
- `lab status` - Show current experiment
- `lab snapshot` - Record git state + artifacts
- `lab add <files>` - Add artifacts
- `lab edit` - Edit notes
- `lab close` - Close experiment
- `lab list` - Simple list

### Phase 2: Web View (`lab-view` binary)
- Server setup with axum, CLI args (--port, --lab-dir)
- List view: experiment table, sorting, filtering
- Detail view: notes, snapshots, artifacts
- Compare view: multi-experiment table
- Artifact rendering by type (images, code, text)
- Git diff display (shell out to git)
- GitHub integration (detect remote, link to commits)
- htmx for interactivity without JS framework

### Phase 3: Enhanced Features (Future)
- fzf integration for `lab list`
- Image diff slider in compare view
- Export comparison to markdown

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

## View Patterns (lab-view)

### Route Handler

```rust
async fn list_experiments(
    State(state): State<AppState>,
    Query(params): Query<ListParams>,
) -> impl IntoResponse {
    let experiments = experiment::list_all(&state.lab_dir)?;
    let template = ListTemplate { experiments, params };
    Html(template.render()?)
}
```

### Askama Template

```rust
#[derive(Template)]
#[template(path = "list.html")]
struct ListTemplate {
    experiments: Vec<Experiment>,
    params: ListParams,
}
```

### Rendering Artifacts

```rust
pub fn render_artifact(name: &str, content: &[u8]) -> ArtifactHtml {
    match extension(name) {
        "png" | "jpg" | "gif" => ArtifactHtml::Image {
            base64: base64::encode(content)
        },
        "yaml" | "json" | "toml" => ArtifactHtml::Code {
            html: highlight_code(content, name)
        },
        "md" => ArtifactHtml::Markdown {
            html: render_markdown(content)
        },
        _ => ArtifactHtml::Download { size: content.len() },
    }
}
```

### GitHub Detection

```rust
pub fn detect_github_remote(repo_path: &Path) -> Option<GitHubRepo> {
    let url = git_remote_url(repo_path)?;
    // Parse git@github.com:user/repo.git or https://github.com/user/repo
    parse_github_url(&url)
}

pub fn commit_url(gh: &GitHubRepo, sha: &str) -> String {
    format!("https://github.com/{}/{}/commit/{}", gh.owner, gh.repo, sha)
}
```

### htmx Patterns

```html
<!-- Sortable column -->
<th hx-get="/list?sort=date&order=desc" hx-target="#experiments">Date</th>

<!-- Filter form -->
<select hx-get="/list" hx-target="#experiments" name="tag">
  <option value="">All tags</option>
</select>

<!-- Lazy-load diff -->
<button hx-get="/diff/001/abc..def" hx-target="#diff-content">
  View diff
</button>
```
