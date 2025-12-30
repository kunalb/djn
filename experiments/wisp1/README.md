# Wisp 1

This is the first (of hopefully many) experiments as I try to figure out how to incorporate ai into my workflows. Wisp 1 is aimed at converting an inline comment into code using $model.

## 2025-12-28
- Implementation using codex.

## Usage
Wisp now talks to the LAMP JSON-RPC server over a Unix socket.

- Start the LAMP server (for example): `LAMP_UDS=/tmp/lamp.sock lamp`
- Set OpenAI-compatible env vars for LAMP: `OPENAI_API_KEY`, optionally `OPENAI_BASE_URL`
- Load `wisp.el` in Emacs and run `M-x wisp-apply-comment`

## Smoke test
Run a batch smoke test for the Emacs side:

```sh
emacs -Q --batch -l wisp.el -l scripts/wisp-smoke.el
```

The live backend is provided by the LAMP JSON-RPC server.
