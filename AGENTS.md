# Repository Guidelines

## Project Structure & Module Organization
The Python agent code lives under `djn/`; `__main__.py` exposes the CLI stub while `tmux.py` wraps tmux popups that agents can trigger. Native helpers sit in `src/`, currently `tmux.c`, which mirrors the popup behaviour for embedding in other runtimes. Use the top-level `README.md` for product framing and add any new docs alongside the modules they describe so future contributors can discover them quickly.

## Build, Test, and Development Commands
Create an isolated environment with `uv venv .venv` and activate it before installing anything else. Install local tooling once via `uv pip install --python .venv/bin/python ruff isort`. Run the stub for a smoke test with `python -m djn`. To experiment with the tmux helper during development, invoke the popup directly via `tmux display-popup -EE -x5% -y5% -w80% -h40%`. Compile the native helper with warnings enabled to catch regressions early: `mkdir -p build && gcc -Wall -Wextra -O2 src/tmux.c -o build/tmux-popup`.

## Coding Style & Naming Conventions
Follow PEP 8 with 4-space indentation for Python modules; keep functions and variables in `snake_case` and prefer concise, action-oriented function names (`popup`, `record_transcript`). Mirror the existing two-space indentation style in the C helper and place braces on the same line as function declarations. Inline any constant that is referenced only once; promote it to a shared helper only when reuse exists. Add short docstrings or comments only where they clarify non-obvious behaviour, such as subprocess invocations or tmux flag choices. When introducing tooling, document it in the PR and provide configuration files in-repo.

## Testing Guidelines
There is no automated suite yet, so add tests alongside new features. Prefer `pytest`, placing modules under `tests/` to mirror the source structure (e.g., `tests/test_tmux.py`). Name tests `test_<behavior>` and isolate tmux subprocess calls behind helper functions so they can be mocked. Run `pytest -q` locally before opening a PR, and include manual verification notes when behaviour depends on tmux UI.

## Commit & Pull Request Guidelines
Existing commits are short, descriptive statements (e.g., `Warming up`, `Getting started`); keep subject lines under ~50 characters in the imperative mood. The repo ships a pre-commit hook (`.githooks/pre-commit`) that auto-runs Ruff format and isort on staged Python files—ensure `core.hooksPath` points to `.githooks` (`git config core.hooksPath .githooks`) so the hook fires. Group related changes together and avoid mixing refactors with behavioural edits. For pull requests, include purpose, key decisions, testing notes, and any follow-up tasks. Link related issues and attach terminal captures or screenshots whenever user-visible behaviour changes.
