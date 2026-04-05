import asyncio
import code
import readline
import sys


from ego.repl import Repl
from ego.core import make_ego


async def launch_repl():
    ego = await make_ego()
    repl = Repl(ego)
    repl.interact()


def main():
    asyncio.run(launch_repl())


if __name__ == "__main__":
    main()
