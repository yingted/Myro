# Setup environment:
import sys, os
lib_directory = os.path.abspath("IronPython/ipy2")
sys.path.insert(0, lib_directory)

# Load the necessary Mono libraries:
import clr
clr.AddReference("gtk-sharp")
clr.AddReference("pango-sharp")
clr.AddReference("IronPython.dll")
clr.AddReference("IronPython.Modules.dll")
#clr.AddReference("IronRuby.dll")
#clr.AddReference("IronRuby.Libraries.dll")
clr.AddReference("Microsoft.Scripting")
clr.AddReference("glib-sharp")

# Bring .NET References into IronPython scope:
import Gtk
import GLib

# Import pure-Python modules:
import traceback

# Setup Runtime environment:
def handle_exception(e):
    print e.__class__.__name__

#GLib.ExceptionManager.UnhandledException += handle_exception

# Define local functions and classes
from utils import _

class PyjamaProject(object):
    def __init__(self, argv):
        self.shell = None
        self.editor = None
        request_shell = False
        request_editor = False
        files = []
        for arg in argv:
            if arg == "--shell":
                request_shell = True
            elif arg == "--editor":
                request_editor = True
            else:
                files.append(arg)
                request_editor = True
        if files == []:
            request_shell = True
        if request_editor:
            from editor import EditorWindow
            self.editor = EditorWindow(self, files)
        if request_shell:
            from shell import ShellWindow
            self.shell = ShellWindow(self)

    def on_run(self, obj, event):
        doc = self.get_current_doc()
        if doc and doc.textview.HasFocus:
            text = doc.textview.Buffer.Text
            doc.execute(text, "Loading file...")
        else:
            self.command_pane.textview.HasFocus = True
            text = self.command_pane.textview.Buffer.Text
            doc.execute(text)

# Let's start!
Gtk.Application.Init()
#------------------------------
try:
    pw = PyjamaProject(sys.argv[1:])
except:
    traceback.print_exc()
    sys.exit()
#------------------------------
Gtk.Application.Run()
