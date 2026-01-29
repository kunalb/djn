# Lab - Experiment Notebook CLI

## Overview

Lab is a CLI tool for capturing experiment snapshots alongside code repositories. It maintains a separate notebook of experiments with git metadata, artifacts, and user notes. The notebook folder is itself a git repository (optionally an orphan branch of the source repo) with git-lfs for artifacts. Every lab command that modifies the notebook automatically commits, providing full history tracking.

## Directory Structure

**Source repository** (the code being experimented on):
```
myproject/
  .lab/
    config.yaml          # configuration
    current              # current experiment ID (empty if none)
  .git/
  ...
```

**Lab folder** (experiment records, separate git repo):
```
~/lab-notes/
  2024/
    01/
      29/
        001-baseline/
          meta.yaml          # auto-generated metadata
          notes.md           # user notes with frontmatter
          artifacts/
            config.yaml
            loss_curve.png
        002-dropout-test/
          ...
  .git/
  .gitattributes             # git-lfs patterns for artifacts
```

## Configuration

`.lab/config.yaml`:
```yaml
lab_dir: ~/lab-notes
repo_name: myproject

artifacts:
  - outputs/*.png
  - outputs/metrics.json
  - config.yaml

notes_template: |
  ---
  tags: []
  hypothesis:
  result:
  ---
  ## Notes

git_hook: true
```

## Experiment Metadata

`meta.yaml` (auto-generated, minimal since git tracks history):
```yaml
id: 2024/01/29/001-baseline
repo_name: myproject
source_repo: /home/user/myproject
status: open

snapshots:
  - commit: abc1234
    branch: main
    dirty: false
  - commit: def5678
    branch: main
    dirty: true
    dirty_files:
      - train.py
      - model.py

artifacts:
  - config.yaml
  - loss_curve.png
```

Note: Timestamps are not stored in meta.yaml since git commit history tracks when each change was made.

## Commands

All commands that modify the lab notebook automatically commit with a descriptive message.

### `lab init`

Interactive setup:
- Creates `.lab/` folder with `config.yaml`
- Prompts for lab folder location
- Initializes lab folder as git repo (or creates orphan branch if using same repo)
- Sets up `.gitattributes` for git-lfs on artifact patterns
- Prompts for default artifacts to capture
- Optionally installs git commit hook in source repo

### `lab open [name]`

Creates a new experiment. Fails if one is already open (use `lab close` first).

- Creates `YYYY/MM/DD/NNN-name/` in lab folder
- Copies configured artifacts to `artifacts/`
- Writes `meta.yaml` with initial git snapshot
- Writes `notes.md` from template
- Sets `.lab/current` to experiment ID
- Commits to lab repo: `"open: 001-baseline"`
- Opens `notes.md` in `$EDITOR`

### `lab status`

Shows current experiment status: ID, snapshot count, artifact list, current git dirty state.

### `lab snapshot`

Records current state:
- Appends current git state to `meta.yaml` snapshots
- Updates any changed configured artifacts
- Commits to lab repo: `"snapshot: 001-baseline"`

### `lab add <file> [file...]`

- Copies files to current experiment's `artifacts/` folder
- Updates `meta.yaml` artifact list
- Commits to lab repo: `"add: loss_curve.png to 001-baseline"`

### `lab edit`

Opens current experiment's `notes.md` in `$EDITOR`. If no experiment is open, launches `lab list` to select one. After editor closes, commits if changed: `"edit: 001-baseline"`.

### `lab close`

- Sets experiment status to `closed` in meta.yaml
- Clears `.lab/current`
- Commits to lab repo: `"close: 001-baseline"`

### `lab list`

fzf-based picker showing all experiments. Filterable by tags, date, name. Selecting an experiment shows options: open (if closed), edit, view.

### `lab view`

Starts local web server:
- Table view of all experiments with sortable columns from frontmatter
- Click row for detail view showing notes, artifacts, and git diff links
- Can render diffs between source repo commits referenced in snapshots

## Lab Notebook Git Integration

The lab folder is a git repository. Options during `lab init`:

1. **Separate repository** (default): `~/lab-notes/` as standalone repo
2. **Orphan branch**: Lab notes in `lab-notes` branch of the source repo itself

Benefits of auto-committing:
- Full history of all experiment changes
- Can see when notes were edited, artifacts updated
- Can revert accidental changes
- `git log` in lab folder shows experiment activity timeline

## Source Repo Git Integration

When `git_hook: true`, `lab init` installs `.git/hooks/prepare-commit-msg`:

```bash
#!/bin/bash
current=$(cat .lab/current 2>/dev/null)
if [ -n "$current" ]; then
  echo "" >> "$1"
  echo "Lab: $current" >> "$1"
fi
```

Commits made while an experiment is open automatically include:

```
Your commit message

Lab: 2024/01/29/001-baseline
```

## Typical Workflow

```bash
# Setup (once per repo)
lab init

# Start experimenting
lab open baseline
# → creates experiment, opens editor for hypothesis/notes
# → auto-commits to lab repo

# Work on code, run training...

lab snapshot                    # record git state + artifacts, auto-commits
lab add outputs/loss.png        # add artifact, auto-commits

# More iterations...
lab snapshot
lab edit                        # update notes, auto-commits on save

lab close                       # mark complete, auto-commits

# Later: review
lab list                        # browse experiments
lab view                        # web UI for comparison

# See experiment history
cd ~/lab-notes && git log --oneline
```
