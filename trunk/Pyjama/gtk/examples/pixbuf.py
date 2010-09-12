import clr
clr.AddReference("gtk-sharp")
clr.AddReference("gdk-sharp")
clr.AddReference("Mono.Cairo")
import Gtk
import Gdk
import Cairo

filename = "/home/dblank/Desktop/blankenship.jpg"
_pixbuf = Gdk.Pixbuf(filename)
_pixbuf.HasAlpha
format = Cairo.Format.Rgb24

#surface = Cairo.Surface.CreateForImage(format, _pixbuf.Width, _pixbuf.Height)

surface = Cairo.ImageSurface(format, _pixbuf.Width, _pixbuf.Height)


from Graphics import *
init()
#win = Window()

class MyWindow(Gtk.Window):
    def __init__(self, title):
        super(MyWindow, self).__init__()
        self.Add(Gtk.DrawingArea())
        Gtk.Application.Invoke(self.update)

    def update(self, obj, event):
        self.ShowAll()

class MyCanvas(Gtk.DrawingArea):
    #def __init__(self):
    #    self.ExposeEvent += self.render
    #    pass
    #def OnExposeEvent(self, event):
        #Gtk.Application.Invoke(self.update)
    #    print event

    def update(self, obj, args):
        print "expose!", obj, args


win = MyWindow("")
