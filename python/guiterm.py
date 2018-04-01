#! /usr/bin/env python3

import sys

import guiterm.encoding
import guiterm.terminal


if __name__ == '__main__':
    guiterm.terminal.create_view(
        guiterm.encoding.read_toplevel(sys.stdin.buffer))
    guiterm.terminal.main()
