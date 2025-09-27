#!/usr/bin/env python3

import os
import shutil
import subprocess
import sys
import uuid


def _error(message, code=1):
    print(message, file=sys.stderr)
    raise SystemExit(code)


def _window_size():
    """Return the active tmux window width and height."""
    result = subprocess.run(
        ["tmux", "display-message", "-p", "#{window_width} #{window_height}"],
        capture_output=True,
        text=True,
        check=True,
    )
    width_str, height_str = result.stdout.strip().split()
    return int(width_str), int(height_str)


def _ensure_tmux_binary():
    if shutil.which("tmux"):
        return
    _error("Error: tmux is not installed. Install tmux to continue.")


def _start_tmux_session(columns: int):
    orientation = "-h" if columns > 160 else "-v"
    session_name = f"djnai-{os.getpid()}-{uuid.uuid4().hex[:6]}"
    target = f"{session_name}:0"
    try:
        subprocess.run(["tmux", "new-session", "-d", "-s", session_name], check=True)
        subprocess.run(["tmux", "split-window", "-t", target, orientation], check=True)
        subprocess.run(["tmux", "attach-session", "-t", session_name], check=True)
    except subprocess.CalledProcessError as error:
        _error("Error: failed to start tmux session.", error.returncode)


def split_pane():
    try:
        width, _ = _window_size()
    except subprocess.CalledProcessError as error:
        _error("Error: tmux pane required. Attach to tmux before running.", error.returncode)
    orientation = "-h" if width > 160 else "-v"
    try:
        subprocess.run(["tmux", "split-window", orientation], check=True)
    except subprocess.CalledProcessError as error:
        _error("Error: tmux pane required. Attach to tmux before running.", error.returncode)


def pane_contents():
    ...


def record_transcript():
    ...


def main():
    if os.environ.get("TMUX"):
        split_pane()
        return

    _ensure_tmux_binary()

    try:
        width = shutil.get_terminal_size().columns
    except OSError:
        _error("Error: unable to detect terminal size outside tmux.")

    _start_tmux_session(width)


if __name__ == "__main__":
    main()
