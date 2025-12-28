import json
import re
import sys
from difflib import unified_diff


def _infer_replacement(language: str, indent: str, keep_newline: bool) -> str:
    if language == "python":
        body = "pass"
    elif language == "elisp":
        body = "nil"
    else:
        body = "/* implementation */"
    replacement = f"{indent}{body}"
    if keep_newline:
        replacement += "\n"
    return replacement


def _apply_replacement(text: str, start_pos: int, end_pos: int, language: str) -> str:
    start = max(0, start_pos - 1)
    end = max(0, end_pos - 1)
    if start >= end or start >= len(text) or end > len(text):
        raise ValueError("Invalid selection range.")
    line_start = text.rfind("\n", 0, start) + 1
    line_prefix = text[line_start:start]
    indent_match = re.match(r"[ \t]*", line_prefix)
    indent = indent_match.group(0) if indent_match else ""
    selection = text[start:end]
    keep_newline = selection.endswith("\n")
    replacement = _infer_replacement(language, indent, keep_newline)
    return text[:start] + replacement + text[end:]


def _make_patch(file_path: str, old_text: str, new_text: str) -> str:
    old_lines = old_text.splitlines(keepends=True)
    new_lines = new_text.splitlines(keepends=True)
    diff = unified_diff(old_lines, new_lines, fromfile=file_path, tofile=file_path)
    return "".join(diff)


def _read_request() -> dict:
    raw = sys.stdin.read()
    if not raw.strip():
        raise ValueError("Empty request.")
    return json.loads(raw)


def _write_response(payload: dict) -> None:
    sys.stdout.write(json.dumps(payload))


def main() -> int:
    try:
        req = _read_request()
        file_path = req.get("file_path") or "<buffer>"
        language = req.get("language") or "unknown"
        selection = req.get("selection") or {}
        start_pos = int(selection.get("start_pos", 0))
        end_pos = int(selection.get("end_pos", 0))
        buffer_text = req.get("buffer_text")
        if buffer_text is None:
            raise ValueError("Missing buffer_text.")
        new_text = _apply_replacement(buffer_text, start_pos, end_pos, language)
        patch = _make_patch(file_path, buffer_text, new_text)
        if not patch:
            raise ValueError("No changes detected.")
        _write_response(
            {
                "ok": True,
                "patch": patch,
                "rationale": "Replace selected comment with a placeholder implementation.",
                "metadata": {"warnings": []},
            }
        )
        return 0
    except Exception as exc:  # noqa: BLE001 - MVP error path
        _write_response(
            {"ok": False, "error": {"code": "BACKEND_ERROR", "message": str(exc)}}
        )
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
