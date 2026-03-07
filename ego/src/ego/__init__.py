import code
import readline
import sys


import ego.__ego__ as __ego__


class Context:

    def __init__(self):
        self._instructions = []

    def len(self):
        return sum(len(x) for x in self._instructions)

    def instruct(self, val):
        self._instructions.append(val)

    def get(self) -> str:
        return "\n\n".join(self._instructions)


context = Context()


def main():
    __ego__.EgoRepl(globals()).interact()


if __name__ == "__main__":
    main()
