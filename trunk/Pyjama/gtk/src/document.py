import Gtk, Pango
import clr
clr.AddReference('gdk-sharp')
clr.AddReference("Myro.dll")
import Myro, System, Gdk
import os, types

target_table = System.Array[Gtk.TargetEntry](
    [Gtk.TargetEntry("STRING", 0, 2635),
     Gtk.TargetEntry ("text/plain", 0, 2635),
     ])

def make_statement(*args, **kwargs):
    print args
    indent = kwargs["indent"] if "indent" in kwargs else False
    statement = Gtk.VBox()
    hbox = Gtk.HBox()
    if not isinstance(args[0], (list, tuple)):
        img = Gtk.Image('gtk-dnd', Gtk.IconSize.Button)
        if indent:
            img.WidthRequest = 50
            img.Xalign = 1
        hbox.PackStart(img, False, True, 0)
    for arg in args:
        if arg in [".", "(", ")", ","]:
            hbox.PackStart(Gtk.Label(arg), False, True, 0)
        elif isinstance(arg, (list, tuple)):
            block = make_block(arg[0], *arg[1:])
            hbox.PackStart(block, False, True, 0)
        elif isinstance(arg, Gtk.Widget):
            hbox.PackStart(arg, False, True, 0)
        elif isinstance(arg, basestring):
            entry = Gtk.Entry(repr(arg))
            entry.WidthChars = 7
            hbox.PackStart(entry, False, True, 0)
        elif isinstance(arg, int): # FIXME: use Gtk.SpinButton
            entry = Gtk.Entry(str(arg))
            entry.WidthChars = 3
            hbox.PackStart(entry, False, True, 0)
        elif isinstance(arg, float):
            entry = Gtk.Entry(str(arg))
            entry.WidthChars = 5
            hbox.PackStart(entry, False, True, 0)
        elif isinstance(arg, (types.MethodType, type, 
                              types.BuiltinFunctionType)):
            hbox.PackStart(Gtk.Label(arg.__name__), False, True, 0)
        else:
            print "type:", args, type(args)
    statement.PackStart(hbox, False, True, 0)
    statement.PackStart(Gtk.HSeparator(), True, True, 0)
    return statement

def make_block(title, *args, **kwargs):
    indent = kwargs["indent"] if "indent" in kwargs else False
    parallel = kwargs["parallel"] if "parallel" in kwargs else False
    block = Gtk.VBox()
    expander = Gtk.Expander(title)
    expander.Expanded = True
    if parallel:
        box = Gtk.HBox()
    else:
        box = Gtk.VBox()
    expander.Add(box)
    for arg in args:
        if isinstance(arg, Gtk.Widget): 
            space = Gtk.HBox()
            img = Gtk.Image('gtk-dnd-multiple', Gtk.IconSize.Button)
            img.WidthRequest = 50
            img.Xalign = 1
            Gtk.Drag.DestSet(img, 
                             Gtk.DestDefaults.All, 
                             target_table, 
                             Gdk.DragAction.Move)
            space.PackStart(img, False, True, 0)
            space.PackStart(arg, False, True, 0)
            hbox = space
        else:
            hbox = make_statement(*arg, indent=True)
        box.PackStart(hbox, False, True, 0)
        if parallel:
            box.PackStart(Gtk.VSeparator(), False, True, 0)
    block.PackStart(expander, True, True, 0)
    block.PackStart(Gtk.HSeparator(), True, True, 0)
    return block

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

class DinahDocument(PlainDocument):
    def make_widget(self):
        self.widget = MyScrolledWindow()
        self.widget.document = self
        self.hpaned = Gtk.HPaned()
        self.treeview = Gtk.TreeView()
        self.treeview.Model = self.make_store()

        column = Gtk.TreeViewColumn()
        column.Title = "Module"
        self.treeview.AppendColumn(column)
        cell = Gtk.CellRendererText()
        column.PackStart(cell, True)
        column.AddAttribute(cell, "text", 0)
        self.layout = Gtk.VBox()
        hbox = make_block("Do together:",
                          ["wait()"], 
                          ["play()"], parallel=True)
        self.layout.PackStart(hbox, False, True, 0)
        hbox = make_block("Do in order:", 
                          (Myro, ".", Myro.forward, "(", 1, ",", 1, ")"),
                          (Myro, ".", Myro.init, "(", "COM1", ",", 0, ")"),
                          (Myro, ".", Myro.backward, "(", 1, ",", 1, ")"),
                          (Myro, ".", Myro.backward, "(", 1, ",", 1, ")"),
                          make_block("Do together:",
                                     ["wait()"], ["play()"], parallel=True),
                          (Myro, ".", Myro.init, "(", "COM1", ",", 0, ")"),
                          (Myro, ".", Myro.backward, "(", 1, ",", 1, ")"),
                          (Myro, ".", Myro.backward, "(", 1, ",", 1, ")"),
                          (Myro, ".", Myro.init, "(", "COM1", ",", 0, ")"),
                          (Myro, ".", Myro.backward, "(", 1, ",", 1, ")"),
                          (Myro, ".", Myro.backward, "(", 1, ",", 1, ")"),
                          )
        self.layout.PackStart(hbox, False, True, 0)
        hbox = make_block("Do together:", 
                          [Myro, ".", Myro.backward, "(", 1, ")"],
                          [Myro, ".", Myro.forward, "(", 1, ")"],
                          [Myro, ".", Myro.backward, "(", 1, ")"],
                          [Myro, ".", Myro.forward, "(", 1, ")"],
                          [Myro, ".", Myro.backward, "(", 1, ")"],
                          [Myro, ".", Myro.forward, "(", 1, ")"],
                          parallel=True)
        self.layout.PackStart(hbox, False, True, 0)

        #Gtk.Layout(Gtk.Adjustment(0, 0, 100, 1, 10, 10), 
        #           Gtk.Adjustment(0, 0, 100, 1, 10, 10))
        self.widget.AddWithViewport(self.hpaned)
        self.hpaned.Add1(self.treeview)
        self.hpaned.Add2(self.layout)
        self.treeview.Show()
        self.hpaned.Show()
        self.layout.Show()
        self.widget.ShowAll()

    def make_store(self):
        store = Gtk.TreeStore(str)
        module = store.AppendValues("Control")
        store.AppendValues(module, "Do number of times:");
        store.AppendValues(module, "Do for each:");
        store.AppendValues(module, "Do in order:");
        store.AppendValues(module, "Do together:");
        store.AppendValues(module, "Do while:");
        store.AppendValues(module, "If:");
        module = store.AppendValues("Myro")
        store.AppendValues(module, ".forward()");
        store.AppendValues(module, ".backward()");
        store.AppendValues(module, ".beep()");
        store.AppendValues(module, ".init()");
        module = store.AppendValues("Dinah")
        store.AppendValues(module, ".wait()");
        store.AppendValues(module, ".beep()");
        store.AppendValues(module, ".random()");
        return store
 
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
