import code
import readline
import sys
import os

from functools import cache
from types import SimpleNamespace


from openai import OpenAI




class __ego__:

    @staticmethod
    @cache
    def client() -> OpenAI:
        return OpenAI(api_key=os.environ.get("PROXY_API_KEY"),
                      base_url="http://localhost:8317/v1/")

    @staticmethod
    def hookable(fn):
        ...

    @staticmethod
    def prompt(q):
        result = __ego__.client().responses.create(
            model="claude-opus-4-6",
            instructions="You are a coding agent running in a Python REPL",
            input=q,
        )
        print(result.output_text)

    class EgoRepl(code.InteractiveConsole):

        def runsource(self, source, filename="<input>", symbol="single"):
            if source.startswith(";"):
                return __ego__.prompt(source[1:])

            return super().runsource(source, filename, symbol)

        def interact(self, banner="", exitmsg=""):
            print("ego: Extensible aGent Orchestrator")
            return super().interact(banner, exitmsg)


def main():
    sys.ps1 = "> "
    sys.ps2 = " "
    __ego__.EgoRepl(globals()).interact()


if __name__ == "__main__":
    main()
