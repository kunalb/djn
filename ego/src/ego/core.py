"""
Scratch attempt
"""

import asyncio
import io
import json
import os

from contextlib import redirect_stdout, redirect_stderr
from pprint import pprint

from openai import AsyncOpenAI


def openrouter_client() -> AsyncOpenAI:
    return AsyncOpenAI(
        api_key=os.environ.get("OPENROUTER_KEY"),
        base_url="https://openrouter.ai/api/v1/",
    )


async def make_ego() -> Ego:
    ego = Ego()
    await ego.add_agent(
        "nemotron",
        "nvidia/nemotron-3-super-120b-a12b:free",
        openrouter_client(),
    )
    return ego


class Context:

    def __init__(self):
        self.msgs = []
        self.responses = []

    def record_msg(self, name, msg):
        self.msgs.append((name, msg))

    def record_response(self, name, response):
        self.responses.append((name, response))


class Ego:

    def __init__(self):
        self._agents = {}
        self._context = Context()

    async def add_agent(self, name: str, model, client: AsyncOpenAI):
        self._agents[name] = agent(self, model, client)
        await self._agents[name].asend(None)

    async def msg(self, name: str, msg: str):
        # Update this to actually handle a full loop of interactions
        self._context.record_msg(name, msg)

        msgs = [{"role": "user", "content": msg}]
        replies = []

        while msgs:
            response = await self._agents[name].asend(msgs)
            self._context.record_response(name, response)
            msgs = []

            print(f"<Status>{response.status}</Status>")
            for item in response.output:
                match item.type:
                    case "reasoning":
                        print("<Reasoning>")
                        for row in item.content:
                            print(row.text, end="")
                        print("</Reasoning>")
                    case "message":
                        for row in item.content:
                            print(row.text)
                    case "function_call":
                        args = json.loads(item.arguments)
                        print("<Tool>")
                        print(f"{item.name}(**{{")
                        for k, v in args.items():
                            print(f'  "{k}": "{v}"')
                        print("}})")
                        print("</Tool>")

                        assert item.name == "exec"
                        with redirect_stdout(io.StringIO()) as tool_stdout, redirect_stderr(io.StringIO()) as tool_stderr:
                            try:
                                exec(args["source"], globals=globals(), locals={})
                            except Exception:
                                ...

                        if tool_stdout.getvalue():
                            print("<StdOut>")
                            print(tool_stdout.getvalue(), end="")
                            print("</StdOut>")

                        if tool_stderr.getvalue():
                            print("<StdErr>")
                            print(tool_stderr.getvalue(), end="")
                            print("</StdErr>")

                        msgs.append({
                            "type": "function_call_output",
                            "call_id": item.call_id,
                            "output": "stdout: " + tool_stdout.getvalue() + "\nstderr: " + tool_stderr.getvalue(),
                        })
                    case _:
                        pprint(item)

        return response

    async def list_tools(self):
        return [
            {
                "type": "function",
                "name": "exec",
                "description": "Run python code in the current environment",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "source": "string",
                        "description": "Code to execute",
                    },
                    "required": ["source"],
                },
            },
        ]


async def agent(ego: Ego, model: str, client: AsyncOpenAI):
    response = None
    previous_response_id = None

    all_msgs = []

    while True:
        msgs = yield response
        all_msgs.extend(msgs)
        print("Calling agent...")
        response = await client.responses.create(
            instructions="""You are an agent running in a Python REPL.
You can execute code to interact with the repl and get results.""",
            model=model,
            input=all_msgs,
            previous_response_id=previous_response_id,
            tools=await ego.list_tools(),
        )
        previous_response_id = response.id
        all_msgs.extend(response.output)
