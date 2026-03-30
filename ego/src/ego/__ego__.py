import code
import os
import sys

from contextlib import contextmanager
from enum import Enum
from functools import cache

from openai import OpenAI

# TODO Implement tools for executing python by providing repl tools
# TODO Then start executing the loop allowing the agent to do more things
# TODO Then add extra functions to allow for recursion and child agents

def hookable(fn):
    ...

def prompt(q):
    result = client().responses.create(
        model="qwen/qwen3-coder:free",
        tools=[],
        instructions=f"""You are a coding agent running in a Python REPL.

You can execute code to interact with the repl and get output results.
        """,
        input=[],
    )
    print(result)


class ReplState(Enum):
    START = "start"
    AGENT = "agent"


class EgoRepl(code.InteractiveConsole):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._state: ReplState = ReplState.START
        self._tools = [
            {
                "type": "function",
                "name": "runsource",
                "description": "Run code in the Python REPL",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "code": {
                            "type": "string",
                            "description": "Code to execute",
                        }
                    },
                    "required": ["code"],
                }
            }
        ]


    def runsource(self, source, filename="<input>", symbol="single"):
        sys.ps1 = "> "

        if source.startswith(";"):
            return prompt(source[1:])

        return super().runsource(source, filename, symbol)

    def interact(self, banner="ego: Extensible aGent Orchestrator", exitmsg="Bye!"):
        return super().interact(banner, exitmsg)

    @cache
    def _client(self) -> OpenAI:
        return OpenAI(
            api_key=os.environ.get("OPENROUTER_API_KEY"),
            base_url="http://openrouter.ai/api/v1/",
        )

    def _set_prompt(self):
        ...
