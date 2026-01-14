# x - LLM command generator wrapper for zsh
# Source this file in your .zshrc: source /path/to/x.zsh

x() {
    # Capture last exit code BEFORE any commands run
    local last_exit=$?

    # Check for no arguments
    if [[ $# -eq 0 ]]; then
        echo "Usage: x <natural language request>" >&2
        echo "Examples:" >&2
        echo "  x list all docker containers" >&2
        echo "  x create a new git branch for feature login" >&2
        echo "  x fix my previous command" >&2
        return 1
    fi

    # 'command' bypasses the function and calls the binary directly
    command x --last-exit="$last_exit" "$@"
}
