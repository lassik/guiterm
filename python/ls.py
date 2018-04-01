#! /usr/bin/env python3

import argparse
import os
import sys

import guiterm.encoding


if __name__ == '__main__':
    ap = argparse.ArgumentParser()
    ap.add_argument('dir', nargs='?', default='.')
    args = ap.parse_args()
    names = os.listdir(args.dir)
    guiterm.encoding.write_toplevel(
        {"_": "window", "title": "ls",
         "child": {
             "_": "tree",
             "columns": [{"title": "Filename"}],
             "items": [[name] for name in names]}},
        sys.stdout.buffer)
