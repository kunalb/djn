"""
Loop for handling an AI conversation and interaction.
"""


from openai import OpenAI
from threading import Thread


class Loop:

    def __init__(self):
        self._state = []
        self._thread: Thread | None = None

    def _start():
        ...

    def msg():
        ...
