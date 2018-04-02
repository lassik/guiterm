import sys
import xml.etree.ElementTree as ET

import gi
gi.require_version('Gtk', '3.0')
from gi.repository import Gtk

import guiterm.encoding


def menu_rec(action_group, parent, nextid, xmenu):
    for xmenuitem in xmenu:
        nextid += 1
        action = 'menu{}'.format(nextid)
        title = xmenuitem.get('title')
        xsubmenu = xmenuitem.get('menu')
        if xmenuitem.get('_') == 'separator':
            ET.SubElement(parent, 'separator')
        elif not xsubmenu:
            ET.SubElement(parent, 'menuitem', action=action)
            action_group.add_action(Gtk.Action(
                action, title, title, Gtk.STOCK_NEW))
        else:
            submenu = ET.SubElement(parent, 'menu', action=action)
            action_group.add_action(Gtk.Action(action, title, None, None))
            nextid = menu_rec(action_group, submenu, nextid, xsubmenu)
    return nextid


def create_menu_bar(xwin):
    menu_bar, accel_group = None, None
    xmenu = xwin.get('menu', [])
    if xmenu:
        action_group = Gtk.ActionGroup('my_actions')
        xml_ui = ET.Element('ui')
        xml_menubar = ET.SubElement(xml_ui, 'menubar', name='MenuBar')
        menu_rec(action_group, xml_menubar, 0, xmenu)
        ui_manager = Gtk.UIManager()
        ui_manager.add_ui_from_string(ET.tostring(xml_ui, encoding='unicode'))
        ui_manager.insert_action_group(action_group)
        menu_bar = ui_manager.get_widget('/MenuBar')
        accel_group = ui_manager.get_accel_group()
    return menu_bar, accel_group


def create_button(x):
    btn = Gtk.Button()
    btn.set_label(x.get('title', ''))
    return btn


def create_box(x):
    orientation = {
        'ltr': Gtk.Orientation.HORIZONTAL,
        'rtl': Gtk.Orientation.HORIZONTAL,
        'ttb': Gtk.Orientation.VERTICAL,
        'btt': Gtk.Orientation.VERTICAL,
    }[x.get('direction', 'ttb')]
    box = Gtk.Box(orientation=orientation, spacing=6)
    for xcontrol in x.get('controls'):
        box.pack_start(create_view(xcontrol), True, True, 0)
    return box


def create_progress(x):
    bar = Gtk.ProgressBar()
    percent = max(0, min(100, int(x.get('percent', 0))))
    bar.set_fraction(0.01 * percent)
    return bar


def create_tree(x):
    xcolumns = x.get('columns', [])
    coltypes = [str] * len(xcolumns)
    store = Gtk.ListStore(*coltypes)
    for xitem in x.get('items', []):
        store.append(xitem)
    tree = Gtk.TreeView(store)
    for i, xcolumn in enumerate(xcolumns):
        tree.append_column(
            Gtk.TreeViewColumn(
                xcolumn.get('title', ''), Gtk.CellRendererText(), text=i))
    return tree


def create_window(x):
    menu_bar, accel_group = create_menu_bar(x)
    win = Gtk.Window()
    win.set_title(x.get('title', ''))
    win.connect('delete-event', Gtk.main_quit)
    win.connect('destroy', Gtk.main_quit)
    if accel_group:
        win.add_accel_group(accel_group)
    topbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL)
    if menu_bar:
        topbox.pack_start(menu_bar, False, False, 0)
    topbox.pack_start(create_box(x), False, False, 0)
    win.add(topbox)
    win.show_all()
    return win


def create_view(view):
    return {
        'box': create_box,
        'button': create_button,
        'progress': create_progress,
        'tree': create_tree,
        'window': create_window,
    }[view['_']](view)


def main(readfunc, writefunc):
    create_view(readfunc())
    Gtk.main()
