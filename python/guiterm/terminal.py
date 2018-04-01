import sys

import gi
gi.require_version('Gtk', '3.0')
from gi.repository import Gtk

import guiterm.encoding


def create_button(x):
    btn = Gtk.Button()
    btn.set_label(x.get('title', ''))
    return btn


def create_window(x):
    win = Gtk.Window()
    win.set_title(x.get('title', ''))
    win.connect('delete-event', Gtk.main_quit)
    win.connect('destroy', Gtk.main_quit)
    xchild = x.get('child')
    if xchild:
        win.add(create_view(xchild))
    win.show_all()
    return win


def create_view(view):
    return {
        'button': create_button,
        'window': create_window,
    }[view['_']](view)


def main():
    Gtk.main()
