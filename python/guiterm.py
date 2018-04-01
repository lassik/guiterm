#! /usr/bin/env python3

import sys

import guiterm.encoding
import guiterm.terminal


if __name__ == '__main__':
    guiterm.terminal.main(
        lambda: guiterm.encoding.read_toplevel(sys.stdin.buffer),
        lambda msg: guiterm.encoding.write_toplevel(msg, sys.stdout.buffer))
