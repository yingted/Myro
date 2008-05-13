import clr
clr.AddReference("Gtk-sharp")
import Gtk
Gtk.Application.Init()

def makePicture():
	w = Gtk.Window("Title")
	b = Gtk.Button("Click me!")
	w.Add(b)
	w.ShowAll()
