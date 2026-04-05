import datetime
import asyncio
import code
import os
import sys

from concurrent.futures import Future
from contextlib import contextmanager
from enum import Enum
from functools import cache
from threading import Thread

from ego.core import Ego


def _loop():
    loop_future = Future[asyncio.AbstractEventLoop]()
    shutdown = asyncio.Event()

    async def _run():
        loop_future.set_result(asyncio.get_event_loop())
        await shutdown.wait()

    thread = Thread(target=lambda: asyncio.run(_run()), daemon=True)
    thread.start()
    loop = loop_future.result()

    result = None
    try:
        while True:
            task = yield result
            if task is None:
                break
            result = asyncio.run_coroutine_threadsafe(task, loop).result()
    finally:
        shutdown.set()
        # thread.join()


class Repl(code.InteractiveConsole):

    def __init__(self, ego: Ego, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._loop = _loop()
        self._loop.send(None)
        self._ego = ego
        sys.ps1 = "> "

    def runsource(self, source, filename="<input>", symbol="single"):
        if source.startswith(","):
            self._loop.send(self._ego.msg("nemotron", source[1:]))
            return False

        return super().runsource(source, filename, symbol)

    def interact(self, banner="ego: Extensible aGent Orchestrator", exitmsg="Bye!"):
        return super().interact(banner, exitmsg)
