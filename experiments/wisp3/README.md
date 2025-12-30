# Wisp 3

Wisp 3 mirrors the Wisp MVP flow, but calls the LAMP JSON-RPC server via
`acp.el`.

## Usage
- Start the LAMP server: `LAMP_UDS=/tmp/lamp.sock lamp`
- Set `OPENAI_API_KEY` (and optionally `OPENAI_BASE_URL`) for LAMP.
- Ensure `acp.el` is installed (MELPA) and `nc` is available.
- Load `wisp.el` in Emacs and run `M-x wisp3-apply-comment`.

## Smoke test
Run a batch smoke test for the Emacs side:

```sh
emacs -Q --batch -l wisp.el -l scripts/wisp-smoke.el
```
