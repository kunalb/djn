"""
Scratch attempt
"""

import asyncio
import json
import os

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
        response = await self._agents[name].asend(msg)
        self._context.record_response(name, response)

        for item in response.output:
            match item.type:
                case "reasoning":
                    print("<Reasoning>")
                    for row in item.content:
                        print(row.text, end="")
                    print("</Reasoning>")
                case "message":
                    for row in item.content:
                        print(row.text, end="")
                case "function_call":
                    args = json.loads(item.arguments)
                    print("<Tool>")
                    print(f"{item.name}(**{args})")
                    print("</Tool>")

                    assert item.name == "exec"
                    exec(args["source"], globals=globals(), locals={})
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
    msgs = []

    while True:
        msg = yield response
        msgs.append({"role": "user", "content": msg})
        response = await client.responses.create(
            instructions="""You are an agent running in a Python REPL.
You can execute code to interact with the repl and get results.""",
            model=model,
            input=msgs,
            previous_response_id=previous_response_id,
            tools=await ego.list_tools(),
        )
        previous_response_id = response.id
