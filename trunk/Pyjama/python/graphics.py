import clr
clr.AddReference("Graphics.dll")
clr.AddReference("gtk-sharp")
clr.AddReference("gdk-sharp")         
clr.AddReference("gnome-sharp")
clr.AddReference("System")          
import Gtk
import System
import Gnome
import Gdk
Gtk.Application.Init()

from Graphics import *
