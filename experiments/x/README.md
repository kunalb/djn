# x

A fast, minimal terminal utility that uses LLMs to generate shell commands from natural language.

```
$ x create a new directory called projects and initialize a git repo in it
┌ gemini (1.2s)
│ mkdir projects && cd projects && git init
└ [Y/n/e] y

Initialized empty Git repository in /home/user/projects/.git/
```

## Features

- **Natural language to shell commands** - describe what you want, get a command
- **Multiple LLM backends** - supports Claude, Gemini, and OpenAI Codex CLIs
- **Context-aware** - captures shell history, git status, tmux content, and last exit code
- **Interactive refinement** - tweak commands with natural language feedback
- **Shell history integration** - generated commands appear in your shell history
- **Minimal and fast** - small Rust binary, no API keys to manage

## Installation

### From GitHub

```bash
cargo install --git https://github.com/kunalb/djn x
```

### From source

```bash
git clone https://github.com/kunalb/djn
cd djn/experiments/x
cargo install --path .
```

### Prerequisites

You need at least one of these CLI tools installed:

- **gemini** - Google's Gemini CLI (default)
- **claude** - Anthropic's Claude CLI
- **codex** - OpenAI's Codex CLI

## Getting Started

### 1. Set up shell integration

Add to your `~/.zshrc` or `~/.bashrc`:

```bash
eval "$(x --init zsh)"   # for zsh
eval "$(x --init bash)"  # for bash
```

This wrapper captures exit codes and adds generated commands to your shell history.

### 2. Try it out

```bash
# Basic usage
x list all files including hidden ones

# Fix the previous command
x fix my last command

# Context-aware requests
x find large files in this directory
```

### 3. Configure (optional)

Create a config file:

```bash
x --config
```

This opens `~/.config/x/config.toml` in your editor:

```toml
# Default provider: gemini, claude, or codex
default_provider = "gemini"

[gemini]
# model = "gemini-2.5-flash"    # fast (default)
# model = "gemini-2.5-pro"      # powerful

[claude]
# model = "sonnet"              # alias for latest sonnet (default)
# model = "opus"                # alias for latest opus
# model = "haiku"               # alias for latest haiku (fast)

[openai]
# model = "gpt-5-codex"         # default
# model = "gpt-5.2-codex"       # latest, most advanced
# model = "gpt-5-codex-mini"    # faster
```

## Usage

```
x [OPTIONS] <REQUEST>...
```

### Options

| Flag | Description |
|------|-------------|
| `-y, --yes` | Skip confirmation, run immediately |
| `-n, --dry-run` | Print command without running |
| `-p, --provider <NAME>` | Use specific provider (claude/gemini/codex) |
| `-m, --model <MODEL>` | Use specific model |
| `--context` | Show captured context (debug) |
| `--config` | Open config file in $EDITOR |
| `--init <SHELL>` | Print shell integration script |

### Interactive Prompt

When a command is generated, you'll see:

```
┌ gemini (1.2s)
│ ls -la
└ [Y/n/e]
```

- **y** or **Enter** - Run the command
- **n** - Cancel
- **e** - Edit the command manually
- **anything else** - Refine with natural language feedback

### Refinement Example

```
$ x list files
┌ gemini (0.8s)
│ ls
└ [Y/n/e] sort by size, largest first
┌ gemini (0.6s)
│ ls -lhS
└ [Y/n/e] y

total 52K
-rw-r--r-- 1 user user 12K Jan 15 10:00 main.rs
...
```

## Context Awareness

`x` automatically captures context to help generate better commands:

- **Working directory** - current path
- **Git info** - repo name and branch
- **Shell history** - last 5 commands
- **Exit code** - whether the last command failed
- **Tmux content** - visible terminal content (if in tmux)

Use `x --context` to see what context is captured:

```bash
x --context
```

Use `x --context <request>` to see the full prompt that would be sent:

```bash
x --context list files
```

## Tips

### Quick commands with -y

Skip confirmation for simple commands:

```bash
x -y show disk usage
```

### Use with pipes

```bash
x --dry-run find rust files | xargs wc -l
```

### Switch providers on the fly

```bash
x -p claude write a complex bash script
x -p codex optimize this for loop
```

### Fix errors

If a command fails, just ask to fix it:

```bash
$ gcc main.c
main.c:10: error: expected ';' before '}'
$ x fix it
┌ gemini (0.9s)
│ gcc main.c -fsyntax-only  # or suggests the actual fix
```

## License

MIT
