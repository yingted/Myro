import Gtk

class ScrolledWindow(Gtk.ScrolledWindow):
    """
    Wrapper so that we can keep track of additional items.
    """

class Document(object):
    def __init__(self, filename, editor, shell):
        self.filename = filename
        self.editor = editor
        self.shell = shell
        if self.filename:
            self.title = self.filename
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
        button.Clicked += lambda obj, event: self.editor.on_close_tab(self.widget)
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
