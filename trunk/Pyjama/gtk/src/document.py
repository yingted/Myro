import Gtk
import os

class ScrolledWindow(Gtk.ScrolledWindow):
    """
    Wrapper so that we can keep track of additional items.
    """

class Document(object):
    def __init__(self, filename, project):
        if filename:
            filename = os.path.abspath(filename)
        self.filename = filename
        self.project = project
        if self.filename:
            self.title = os.path.basename(self.filename)
            self.language = "python"
        else:
            self.title = "New Script"
            self.language = "python"
        self.tab = Gtk.HBox()
        label = Gtk.Label(self.title)
        #self.tab.WidthRequest = 150
        #label.Ellipsize = Pango.EllipsizeMode.End
        label.Show()
        self.tab.PackStart(label)
        button = Gtk.Button()
        button.Relief = Gtk.ReliefStyle.None
        img = Gtk.Image(Gtk.Stock.Close, Gtk.IconSize.Menu)
        img.Show()
        button.Add(img)
        button.Show()
        button.Clicked += lambda obj, event: \
            self.project.editor.on_close_tab(self.widget)
        self.tab.PackEnd(button)
        self.widget = ScrolledWindow()
        self.widget.document = self
        self.textview = Gtk.TextView()
        self.widget.Add(self.textview)
        self.textview.Editable = True
        self.textview.WrapMode = Gtk.WrapMode.Word
        self.textview.AcceptsTab = True
        self.textview.Show()
        self.widget.Show()
        if self.filename and os.path.exists(self.filename):
            self.textview.Buffer.Text = "".join(file(self.filename).readlines())
        # start = self.textview.Buffer.StartIter
        # end = self.textview.Buffer.EndIter
        # (found,  mstart, mend) = start.ForwardSearch("def ", 
        #    Gtk.TextSearchFlags.TextOnly, end)
        # if found:
        #     self.textview.Buffer.SelectRange(mstart, mend)
        
    def get_text(self):
        return self.textview.Buffer.Text
