#!/usr/bin/env python3

import argparse
import subprocess


def popup():
    proc = subprocess.run(
        [
        "tmux",
        "display-popup",
        "-EE",
        "-x5%",
        "-y5%",
        "-w80%",
        "-h40%",
        ]
    )


def pane_contents():
    ...


def record_transcript():
    ...


def main():
    popup()


if __name__ == "__main__":
    main()
