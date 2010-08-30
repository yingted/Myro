# Setup environment:
import sys, os
directory, filename = os.path.split(__file__)
lib_directory = os.path.join(directory, "lib")
sys.path.insert(0, lib_directory)

# Load the necessary Mono libraries:
import clr
clr.AddReference("gtk-sharp")
clr.AddReference("pango-sharp")
clr.AddReference("IronPython.dll")
clr.AddReference("IronPython.Modules.dll")
clr.AddReference("Microsoft.Scripting")
clr.AddReference("glib-sharp")
#clr.AddReference("MyTextView.dll")

# Bring .NET References into IronPython scope:
import Gtk
import GLib

# Import pure-Python modules:
import traceback

# Pyjama modules:
from editor import EditorWindow
from shell import ShellWindow

# Setup Runtime environment:
def handle_exception(e):
    print e.__class__.__name__

#GLib.ExceptionManager.UnhandledException += handle_exception

# Define local functions and classes
from utils import _

class PyjamaProject(object):
    def __init__(self):
        # make the shell first:
        self.shell = ShellWindow(self)
        #self.editor = EditorWindow(self)

    def on_run(self, obj, event):
        doc = self.get_current_doc()
        if doc and doc.textview.HasFocus:
            print "notebook has focus!"
            text = doc.textview.Buffer.Text
            doc.execute(text, "Loading file...")
        else:
            print "give command focus"
            self.command_pane.textview.HasFocus = True
            text = self.command_pane.textview.Buffer.Text
            doc.execute(text)

# Let's start!
Gtk.Application.Init()
#------------------------------
try:
    pw = PyjamaProject()
except:
    traceback.print_exc()
    sys.exit()
#------------------------------
Gtk.Application.Run()
