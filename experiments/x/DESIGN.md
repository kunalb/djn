# x - Design Document

This document outlines the architecture, design decisions, and implementation details of the `x` command-line tool.

## Overview

`x` is a terminal utility that translates natural language requests into shell commands using LLM backends. It prioritizes speed, simplicity, and a good user experience.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                           main.rs                                │
│  - CLI parsing (clap)                                           │
│  - Orchestration and control flow                               │
│  - Shell init script generation                                 │
└─────────────────────────────────────────────────────────────────┘
         │              │              │              │
         ▼              ▼              ▼              ▼
┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐
│   cli.rs    │ │  config.rs  │ │ context.rs  │ │   llm.rs    │
│             │ │             │ │             │ │             │
│ Argument    │ │ TOML config │ │ Shell hist  │ │ Provider    │
│ definitions │ │ loading     │ │ Git info    │ │ abstraction │
│             │ │ XDG paths   │ │ Tmux capture│ │ CLI spawning│
└─────────────┘ └─────────────┘ └─────────────┘ └─────────────┘
                                                       │
                                                       ▼
                                              ┌─────────────────┐
                                              │  executor.rs    │
                                              │                 │
                                              │ Timer/spinner   │
                                              │ Confirmation UI │
                                              │ Command exec    │
                                              └─────────────────┘
```

## Key Design Decisions

### 1. Delegate to Existing CLIs

Instead of making direct API calls, `x` spawns existing CLI tools (claude, gemini, codex). This provides:

- **No API key management** - users already have CLIs configured
- **Automatic updates** - CLI tools handle model updates
- **Reduced complexity** - no HTTP client, auth, or retry logic
- **Smaller binary** - minimal dependencies

Trade-off: Requires users to have at least one CLI installed.

### 2. Shell Wrapper for History Integration

The binary cannot modify parent shell history directly. Solution:

1. Shell wrapper function (via `x --init zsh`)
2. Binary writes executed command to temp file (`/tmp/x-hist-$$`)
3. Wrapper reads file and uses `print -s` (zsh) or `history -s` (bash)

This ensures both `x <request>` and the generated command appear in history.

### 3. Structured Command Output

LLMs may output explanatory text along with commands. The prompt asks models to wrap commands in `<cmd>...</cmd>` tags:

```
I'll list files sorted by size.
<cmd>ls -lhS</cmd>
```

The parser extracts the **last** `<cmd>` tag from the output. This handles agentic CLIs (like Claude Code) that may iterate and produce multiple command suggestions before settling on a final one.

Fallbacks for models that don't use tags:
- Markdown code blocks (```bash ... ```)
- Inline backticks (`command`)
- Raw output (last resort)

### 4. Context Capture

To generate relevant commands, `x` captures:

| Context | Source | Purpose |
|---------|--------|---------|
| Working directory | `pwd` | Path-aware commands |
| Git branch/repo | `git` commands | Repo-aware suggestions |
| Shell history | `~/.zsh_history` | "fix last command" |
| Exit code | Shell wrapper | Error context |
| Piped stdin | `stdin.is_terminal()` | Generate stdin-reading commands |
| Tmux content | `tmux capture-pane` | Visible errors/output (opt-in) |

Tmux capture is opt-in via `-t/--tmux` flag to avoid capturing sensitive terminal content by default. Content is truncated to 2000 chars for the prompt (full content shown with `--context`).

### 5. User's Shell for Execution

Commands run via `$SHELL -c` instead of `sh -c`. This ensures:

- Shell builtins work correctly (`type`, `source`, etc.)
- User's shell configuration is respected
- Aliases and functions are available (when appropriate)

### 6. Interactive Refinement Loop

The confirmation prompt accepts natural language refinements:

```
┌ gemini (1.2s)
│ ls
└ [Y/n/e/...] sort by size
┌ gemini (0.8s)
│ ls -lhS
└ [Y/n/e/...] y
```

Non-y/n/e input triggers regeneration with the feedback and previous command as context.

## File Structure

```
experiments/x/
├── Cargo.toml          # Dependencies and metadata
├── build.rs            # Git hash and timestamp for version
├── src/
│   ├── main.rs         # Entry point and orchestration
│   ├── cli.rs          # Clap argument definitions
│   ├── config.rs       # TOML config loading
│   ├── context.rs      # Shell/git/tmux/stdin context capture
│   ├── executor.rs     # Timer, confirmation UI, execution
│   └── llm.rs          # Provider abstraction and CLI spawning
└── tests/
    └── integration.rs  # End-to-end tests
```

## Provider Implementation

Each provider has specific handling:

### Claude
- CLI: `claude --print <prompt> [--model <model>]`
- Model aliases: `opus`, `sonnet`, `haiku` → full model IDs
- Output: Direct stdout capture

### Gemini
- CLI: `gemini [-m <model>]` with prompt on stdin
- Stdin piping required for non-interactive mode
- Captures both stdout and stderr for errors
- Tool execution disabled via custom settings (`~/.config/x/gemini/settings.json`) to prevent the CLI from executing commands directly

### Codex (OpenAI)
- CLI: `codex exec --skip-git-repo-check -o <file> <prompt>`
- Output to temp file (avoids stdout duplication issues)
- `--skip-git-repo-check` for non-repo directories

## UI Components

### Timer Display
```
- gemini (0.5s)     # While generating
┌ gemini (1.2s)     # When complete
│ ls -la            # Command (bold)
└ [Y/n/e/...]           # Prompt
```

- Live timer updates every 100ms
- Standard colors throughout (bold only for command)
- Box drawing characters for visual structure

### Error Display
```
error: gemini exited with code 1: API key not configured
```

Errors include:
- Exit code
- stderr output
- Install URLs when CLI not found

## Configuration

Config file: `~/.config/x/config.toml` (XDG compliant)

```toml
default_provider = "gemini"

[gemini]
model = "gemini-2.5-flash"

[claude]
model = "sonnet"

[openai]
model = "gpt-4o"
```

The `--config` flag creates a template if missing and opens in `$EDITOR`.

## Version String

Build script (`build.rs`) embeds:
- Package version from Cargo.toml
- Git short hash
- Build date (YYYYMMDD)

Result: `x 0.1.0 (abc1234 20260115)`

## Testing

Integration tests verify:
- `--init` output for zsh/bash
- `--context` flag behavior
- `--dry-run` produces valid commands
- No markdown in output

Tests use the actual LLM backends, so results may vary slightly.

## Binary Optimization

Release profile in workspace `Cargo.toml`:

```toml
[profile.release]
opt-level = "z"    # Size optimization
lto = true         # Link-time optimization
strip = true       # Strip symbols
panic = "abort"    # Smaller panic handling
```

## Performance Analysis

### CLI Startup Overhead (January 2026)

Benchmarking the three supported CLIs reveals significant differences in startup time:

| CLI | Implementation | Total Time | Startup Overhead | API Latency |
|-----|---------------|------------|------------------|-------------|
| **Codex** | Native Rust binary (static-pie) | ~0.8s | ~0.1s | ~0.7s |
| **Claude** | Node.js | ~4.0s | ~3.85s (96%) | ~0.15s |
| **Gemini** | Node.js | ~8.5s | ~7s+ (80%+) | ~1.5s |

### Key Findings

**Codex is fast because it's a native binary:**
```
$ file .../codex
ELF 64-bit LSB pie executable, x86-64, static-pie linked, stripped
```

The npm package includes pre-compiled Rust binaries for each platform. The Node.js wrapper (`codex.js`) simply spawns the native binary.

**Claude and Gemini are slow due to Node.js module loading:**

Using `strace -tt` to trace Claude CLI:
```
02:10:57.099  execve("claude", ...)     # Process starts
02:11:00.952  connect(..., port 443)    # First API call
```

~3.85 seconds of the ~4 second total is spent loading Node.js modules before any API call is made.

### Resource Tracking

The history database captures per-invocation metrics via `wait4()` syscall:

- `duration_ms` - Wall clock time
- `user_time_us` - User CPU time in microseconds
- `system_time_us` - System CPU time in microseconds
- `max_rss_kb` - Maximum resident set size in kilobytes

Query example:
```sql
SELECT provider,
       AVG(duration_ms) as avg_wall_ms,
       AVG(user_time_us/1000) as avg_user_ms,
       AVG(max_rss_kb/1024) as avg_rss_mb
FROM generations
GROUP BY provider;
```

### Protocol Support

Each CLI has different server/protocol capabilities:

| CLI | MCP Server | ACP Support | Notes |
|-----|-----------|-------------|-------|
| Claude | `--mcp-config` | Not yet | Can consume MCP servers |
| Gemini | `gemini mcp` | `--experimental-acp` | ACP flag available |
| Codex | `codex mcp-server` | Not yet | Can run as MCP server |

### Optimization Opportunities

1. **Keep Node.js warm** - A daemon could keep Claude/Gemini's Node.js runtime loaded, reducing startup from ~4-8s to <1s

2. **Use ACP/MCP protocols** - Instead of spawning new processes, communicate with a persistent agent server

3. **Default to Codex** - For latency-sensitive use cases, Codex's native binary is 5-10x faster

4. **Lazy provider loading** - Only import the selected provider's module

## Future Considerations

Potential enhancements not yet implemented:

- **Command caching** - Cache common requests
- **Custom prompts** - User-defined system prompts
- **Streaming output** - Show command as it generates
- **Multi-command sessions** - Conversational mode
- **Safety checks** - Warn about destructive commands
- **ACP integration** - Use Agent Control Protocol for persistent connections
