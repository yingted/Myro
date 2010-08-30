import System
import Gtk

def _(text): return text

def Array(*list):
    Type = type(list[0])
    dimensions = [len(list)]
    return System.Array.CreateInstance(Type, *dimensions)

def add_action(group, aid, text, tooltip, image, function):
    action = Gtk.Action(aid, text, tooltip, image)
    action.Activated += function
    group.Add(action)

