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


## TODO:
- build out the server implementation; api
  - add_context(tag, ...)
  - [chat proxy api, flesh this out after checking upstream docs]

- implement the interfaces for
  - lsp
  - mcp server -- so claude/gemini can talk to it too
  - zsh autocomplete

- need this to be self hosting:
  - this is mostly boilerplate that should be very LLMable
  - I need to set up things enough that it's self-hosted
	- figure out how much/little ai is needed
	- and figure out the taste behind the different tools I need for this to work

- the interfaces I'd like to have
  - chat on the terminal with active context of files open
	- have it remember the previous sessions and discussions
	- support for getting tool call context
  - then add support for inline suggestions / live clippy with a small model

- imagining
  - I can get the tiny llm to club context for me based on all the things I'm multitasking on
  - including remote sessions, emacs editing sessions, and trigger things I need
  - add in hooks to generate what I need it to do
