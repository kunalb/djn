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
        model="claude-opus-4-6",
        tools=tools,
        instructions=f"""You are a coding agent running in a Python REPL.

can execute code to interact with the repl and get output results. The tools
ed in are simply python functions you can execute in the repl and maintain
e on your behalf.

actual user request is available through the global variable context, which
n object of type Context. Use `context.get()` in the repl to access context.
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
            api_key=os.environ.get("PROXY_API_KEY"),
            base_url="http://localhost:8317/v1/",
        )

    def _set_prompt(self):
        ...
