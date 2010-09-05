import Gtk, Pango
import os

class MyScrolledWindow(Gtk.ScrolledWindow):
    """
    Wrapper so that we can keep track of additional items.
    """

class PlainDocument(object):
    def __init__(self, filename, project):
        if filename:
            filename = os.path.abspath(filename)
        self.filename = filename
        self.project = project
        if self.filename:
            self.title = os.path.basename(self.filename)
            self.language = "python"
        else:
            self.title = "New Python Script"
            self.language = "python"
        self.make_tab()
        self.make_widget()
        if self.filename and os.path.exists(self.filename):
            self.open()

    def search(self):
        pass
        # start = self.textview.Buffer.StartIter
        # end = self.textview.Buffer.EndIter
        # (found,  mstart, mend) = start.ForwardSearch("def ", 
        #    Gtk.TextSearchFlags.TextOnly, end)
        # if found:
        #     self.textview.Buffer.SelectRange(mstart, mend)
        
    def make_tab(self):
        self.tab = Gtk.HBox()
        self.label = Gtk.Label(self.title)
        #self.tab.WidthRequest = 150
        #label.Ellipsize = Pango.EllipsizeMode.End
        self.label.Show()
        self.tab.PackStart(self.label)
        button = Gtk.Button()
        button.Relief = Gtk.ReliefStyle.None
        img = Gtk.Image(Gtk.Stock.Close, Gtk.IconSize.Menu)
        img.Show()
        button.Add(img)
        button.Show()
        button.Clicked += lambda obj, event: \
            self.project.editor.on_close_tab(self.widget)
        self.tab.PackEnd(button)

    def make_widget(self):
        self.widget = MyScrolledWindow()
        self.widget.document = self
        self.textview = Gtk.TextView()
        self.textview.ModifyFont(
            Pango.FontDescription.FromString("Monospace 10"))
        self.widget.Add(self.textview)
        self.textview.Editable = True
        self.textview.WrapMode = Gtk.WrapMode.Word
        self.textview.AcceptsTab = True
        self.textview.Show()
        self.widget.Show()

    def get_text(self):
        return self.textview.Buffer.Text

    def open(self):
        self.textview.Buffer.Text = "".join(file(self.filename).readlines())

    def save(self):
        """
        """
        if not self.filename:
            self.save_as()
        if self.filename:
            fp = open(self.filename, "w")
            fp.write(self.get_text())
            fp.close()
            return True
        return False

    def save_as(self):
        retval = False
        fc = Gtk.FileChooserDialog("Enter the file to save",
                                   self.project.editor.window,
                                   Gtk.FileChooserAction.Save,
                                   "Cancel", Gtk.ResponseType.Cancel,
                                   "Save", Gtk.ResponseType.Accept)
        if (fc.Run() == int(Gtk.ResponseType.Accept)):
            self.filename = fc.Filename
            self.title = os.path.basename(self.filename)
            self.label.Text = self.title
            self.language = "python"
            retval = True
        fc.Destroy()
        return retval

try:
    import clr
    clr.AddReference("gtksourceview2-sharp")
    import GtkSourceView
    class Document(PlainDocument):
        def make_widget(self):
            self.widget = MyScrolledWindow()
            self.widget.document = self
            self.lang_manager = GtkSourceView.SourceLanguageManager()
            self.textview = GtkSourceView.SourceView()
            self.textview.ShowLineNumbers =True
            self.textview.ModifyFont(
                Pango.FontDescription.FromString("Monospace 10"))
            self.textview.Buffer.Language = self.lang_manager.GetLanguage(
                self.language)
            self.widget.Add(self.textview)
            self.textview.Editable = True
            self.textview.WrapMode = Gtk.WrapMode.Word
            self.textview.AcceptsTab = True
            self.textview.Show()
            self.widget.Show()
except:
    Document = PlainDocument
