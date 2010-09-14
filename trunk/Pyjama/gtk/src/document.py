import clr
clr.AddReference("gtk-sharp")
clr.AddReference("pango-sharp")
import Gtk
import Pango
import os

class MyScrolledWindow(Gtk.ScrolledWindow):
    """
    Wrapper so that we can keep track of additional items.
    """

class BaseDocument(object):
    def __init__(self, filename, project, language="python"):
        self.filename = filename
        if filename:
            filename = os.path.abspath(filename)
        self.project = project
        self.language = language
        if self.filename:
            self.title = os.path.basename(self.filename)
        else:
            self.title = "New %s Script" % self.language.title()
        self.make_tab()
        self.make_widget()
        if self.filename and os.path.exists(self.filename):
            self.open()

    def grab_focus(self):
        self.textview.GrabFocus()

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

    def on_modified(self, obj, event):
        if self.get_dirty():
            self.label.Text = "*%s" % self.title
        else:
            self.label.Text = self.title

    def get_dirty(self):
        return self.textview.Buffer.Modified

    def make_widget(self):
        self.widget = MyScrolledWindow()
        self.widget.document = self
        self.textview = Gtk.TextView()
        self.textview.Buffer.ModifiedChanged += self.on_modified
        self.textview.ModifyFont(
            Pango.FontDescription.FromString("Monospace 10"))
        self.widget.Add(self.textview)
        self.textview.Editable = True
        self.textview.WrapMode = Gtk.WrapMode.Word
        self.textview.AcceptsTab = True
        self.textview.Show()
        self.widget.Show()
        self.textview.GrabFocus()

    def get_text(self):
        return self.textview.Buffer.Text

    def open(self):
        self.textview.Buffer.Text = "".join(file(self.filename).readlines())
        self.textview.GrabFocus()

    def save(self):
        """
        """
        if not self.filename:
            self.save_as()
        if self.filename:
            fp = open(self.filename, "w")
            fp.write(self.get_text())
            fp.close()
            self.textview.Buffer.Modified = False
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
            self.on_change_file()
            retval = True
        fc.Destroy()
        return retval

    def on_change_file(self):
        # FIXME: get this from languages
        if self.filename.endswith(".ss"):
            self.language = "scheme"
        elif self.filename.endswith(".py"):
            self.language = "python"
        elif self.filename.endswith(".dnh"):
            self.language = "dinah"
        elif self.filename.endswith(".rb"):
            self.language = "ruby"
        else:
            self.language = "python"


try:
    import clr
    clr.AddReference("gtksourceview2-sharp")
    import GtkSourceView
    class Document(BaseDocument):
        def make_widget(self):
            self.widget = MyScrolledWindow()
            self.widget.document = self
            self.lang_manager = GtkSourceView.SourceLanguageManager()
            self.textview = GtkSourceView.SourceView()
            self.textview.Buffer.ModifiedChanged += self.on_modified
            self.textview.ShowLineNumbers =True
            self.textview.InsertSpacesInsteadOfTabs = True
            self.textview.IndentWidth = 4
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
            self.textview.GrabFocus()

        def on_change_file(self):
            super(Document, self).on_change_file()
            self.textview.Buffer.Language = self.lang_manager.GetLanguage(
                self.language)

except:
    Document = BaseDocument

# How to list relevant items out of a reference:
# [getattr(clr.References[2], x) for x in dir(clr.References[2]) if type(getattr(clr.References[2], x)) is type]
