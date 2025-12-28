# Wisp 1

This is the first (of hopefully many) experiments as I try to figure out how to incorporate ai into my workflows. Wisp 1 is aimed at converting an inline comment into code using $model.

## 2025-12-28
- Implementation using codex.

## Smoke test
Run a batch smoke test for the Emacs side:

```sh
emacs -Q --batch -l wisp.el -l scripts/wisp-smoke.el
```

Run a batch smoke test against the real backend:

```sh
emacs -Q --batch -l wisp.el -l scripts/wisp-smoke-backend.el
```
