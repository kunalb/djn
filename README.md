# Djn


Djn (_djinn_) is a collection of utilities to apply LLMs in the terminal primarily aimed for my personal use, quirks and taste. They tie together to make LLM use ergonomic, and automatically gather context on my behalf.


## Projects:

### Lamp (home of the djinn)
Central server to maintain all state: collect context from different services, including clipboard, recent edits from emacs, browser windows, terminal commands & buffers; uses a live small model to determine next actions.
- Should serve as an LSP, chat client, zsh auto complete, etc
- Must maintain state, understand costs of using different llms, etc.


### Extensions
- ZSH Autocomplete
- Terminal & Web chat interfaces
- Timeline tracing
- LSP
- Browser extension to communicate with it
