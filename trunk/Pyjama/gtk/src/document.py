import Gtk, Pango
import clr
clr.AddReference('gdk-sharp')
clr.AddReference("Myro.dll")
import Myro, System, Gdk
import os, types

blue = Gdk.Color(70, 227, 207)
purple = Gdk.Color(227, 70, 207)
orange = Gdk.Color(243, 111, 11)
statement_color = blue
block_color = orange

# Formats target will accept:
target_table = System.Array[Gtk.TargetEntry]([
        Gtk.TargetEntry("application/x-dinah", 0, 0),
        ])

# Formats source provides:
source_table = System.Array[Gtk.TargetEntry]([
        Gtk.TargetEntry("application/x-dinah", 0, 0),
        ])

def color_code(color):
    r = int(color.Red/float(2**16) * 255)
    g = int(color.Green/float(2**16) * 255)
    b = int(color.Blue/float(2**16) * 255)
    return "#%02X%02X%02X" % (r, g, b)

def handleDragDataReceived(obj, args):
    print "dnd received", obj, args
    success = False
    source = Gtk.Drag.GetSourceWidget(args.Context)
    data = System.Text.Encoding.UTF8.GetString(
        args.SelectionData.Data)
    if args.Info == 0:
        print "application/x-dinah drop:", data
        success = True
    Gtk.Drag.Finish(args.Context, success, False, args.Time)

def handleSourceDragDataGet(obj, args):
    # DragDropGetArgs: args
    print "dnd get", obj, args
    targets = args.Context.Targets
    data = "ok"
    packed = System.Text.Encoding.UTF8.GetBytes(data)
    args.SelectionData.Set(targets[0], 8, packed)

def make_drag_drop(name, color):
    box = Gtk.EventBox()
    img = Gtk.Image(name, Gtk.IconSize.Button)
    box.Add(img)
    img.Xalign = 1
    img.Yalign = 0
    # Set item up as a drag source:
    Gtk.Drag.SourceSet(box,
                       Gdk.ModifierType.Button1Mask,
                       source_table, 
                       Gdk.DragAction.Copy | Gdk.DragAction.Move)
    box.DragDataGet += Gtk.DragDataGetHandler(handleSourceDragDataGet)
    return box

def process_widgets(layout):
    retval = []
    for widget in layout:
        if isinstance(widget, BlockWidget):
            retval.extend([("Block", widget.type, process_widgets(widget))])
        elif isinstance(widget, StatementWidget):
            retval.extend([("Statement", widget._class, 
                            process_widgets(widget))])
        elif isinstance(widget, Gtk.Entry):
            retval.extend([('Entry', widget.Text)])
        elif isinstance(widget, Gtk.Box):
            retval.extend(process_widgets(widget))
        elif isinstance(widget, Gtk.Expander):
            retval.extend(process_widgets(widget))
        elif isinstance(widget, Gtk.EventBox):
            retval.extend(process_widgets(widget))
    return retval

def process_list(layout, items, parallel=False):
    for item in items:
        if isinstance(item, Block):
            box = process_block(item)
        elif isinstance(item, Statement):
            box = process_statement(item)
        else:
            print "error!"
        layout.PackStart(box, False, True, 0)
        if parallel:
            layout.PackStart(Gtk.VSeparator(), False, True, 0)
        else:
            layout.PackStart(Gtk.HSeparator(), False, True, 0)

class StatementWidget(Gtk.EventBox):
    def set_data(self, statement):
        self._class = statement._class
        self._method = statement._method
        self.args = statement.args
        self.kwargs = statement.kwargs

def process_statement(statement):
    enclosure = StatementWidget()
    enclosure.set_data(statement)
    enclosure.ModifyBg(Gtk.StateType.Normal, statement_color)
    vbox = Gtk.VBox()
    enclosure.Add(vbox)
    # Set item up as a drop target:
    Gtk.Drag.DestSet(enclosure, 
                     Gtk.DestDefaults.All, 
                     target_table, 
                     Gdk.DragAction.Copy | Gdk.DragAction.Move)
    enclosure.DragDataReceived += Gtk.DragDataReceivedHandler(handleDragDataReceived)
    hbox = Gtk.HBox()
    img = make_drag_drop('gtk-dnd', statement_color) 
    hbox.PackStart(img, False, True, 0)
    
    label = Gtk.Label("%s.%s(" % (statement._class, statement._method))
    hbox.PackStart(label, False, True, 0)
    for arg in statement.args:
        if isinstance(arg, basestring):
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
        else:
            print "type:", args, type(args)
        # FIXME: add comma
    label = Gtk.Label(")")
    hbox.PackStart(label, False, True, 0)
    vbox.PackStart(hbox, False, True, 0)
    return enclosure

class BlockWidget(Gtk.EventBox):
    def set_data(self, block):
        self.type = block.type
        self.statements = block.statements[:]
        self.parallel = block.parallel

def process_block(block):
    enclosure = BlockWidget()
    enclosure.set_data(block)
    enclosure.ModifyBg(Gtk.StateType.Normal, block_color)
    vbox = Gtk.VBox()
    enclosure.Add(vbox)
    # Set item up as a drop target:
    Gtk.Drag.DestSet(enclosure, 
                     Gtk.DestDefaults.All, 
                     target_table, 
                     Gdk.DragAction.Copy | Gdk.DragAction.Move)
    enclosure.DragDataReceived += Gtk.DragDataReceivedHandler(handleDragDataReceived)
    expander_row = Gtk.HBox()
    img = make_drag_drop('gtk-dnd-multiple', block_color)
    expander_row.PackStart(img, False, True, 0)
    expander = Gtk.Expander(block.type)
    # Set item up as a drop target:
    Gtk.Drag.DestSet(expander, 
                     Gtk.DestDefaults.All, 
                     target_table, 
                     Gdk.DragAction.Copy | Gdk.DragAction.Move)
    expander.DragDataReceived += Gtk.DragDataReceivedHandler(handleDragDataReceived)
    expander.Expanded = True
    expander_row.PackStart(expander, True, True, 0)
    if block.parallel:
        box = Gtk.HBox()
    else:
        box = Gtk.VBox()
    expander.Add(box)
    process_list(box, block.statements, parallel=block.parallel)
    end = Gtk.EventBox()
    #block.ModifyBg(Gtk.StateType.Normal, block_color)
    label = Gtk.Label("End of %s" % block.type)
    label.Xalign = 0
    end.Add(label)
    Gtk.Drag.DestSet(end, 
                     Gtk.DestDefaults.All, 
                     target_table, 
                     Gdk.DragAction.Copy | Gdk.DragAction.Move)
    end.DragDataReceived += Gtk.DragDataReceivedHandler(handleDragDataReceived)
    box.PackStart(end, True, True, 0)
    vbox.PackStart(expander_row, True, True, 0)
    vbox.PackStart(Gtk.HSeparator(), True, True, 0)
    return enclosure

class Block(object):
    def __init__(self, block_type, *statements, **kwargs):
        self.type = block_type
        self.statements = statements[:]
        self.parallel = kwargs["parallel"] if "parallel" in kwargs else False

class Statement(object):
    def __init__(self, _class, _method, *args, **kwargs):
        self._class = _class
        self._method = _method
        self.args = args
        self.kwargs = kwargs

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
        self.treeview.GrabFocus()
        self.treeview.Model = self.make_store()

        column = Gtk.TreeViewColumn()
        column.Title = "Module"
        self.treeview.AppendColumn(column)
        cell = Gtk.CellRendererText()
        # Set item up as a drag source:
        self.treeview.EnableModelDragSource(
            Gdk.ModifierType.Button1Mask,
            source_table, 
            Gdk.DragAction.Copy | Gdk.DragAction.Move)
        self.treeview.DragDataGet += Gtk.DragDataGetHandler(handleSourceDragDataGet)
        column.PackStart(cell, True)
        column.AddAttribute(cell, "markup", 0)
        self.layout = Gtk.VBox()
        block = Gtk.EventBox()
        #block.ModifyBg(Gtk.StateType.Normal, block_color)
        label = Gtk.Label("Start of Dinah Script. Drag and Drop from the Module list on left.")
        label.Xalign = 0
        block.Add(label)
        Gtk.Drag.DestSet(block, 
                         Gtk.DestDefaults.All, 
                         target_table, 
                         Gdk.DragAction.Copy | Gdk.DragAction.Move)
        block.DragDataReceived += Gtk.DragDataReceivedHandler(handleDragDataReceived)
        self.layout.PackStart(block, False, True, 0)
        
        top_level = [Block("Do together:", 
                           Statement("Myro", "forward", 1),
                           Statement("Myro", "forward", 1, .5),
                           parallel=True),
                     Block("Do in order:", 
                           Statement("Myro", "forward", 1, 1),
                           Statement("Myro", "init", "COM1", 0),
                           Statement("Myro", "backward", 1, 1),
                           Statement("Myro", "backward", 1, 1),
                           Block("Do together:",
                                 Statement("Myro", "init", "COM1", 0),
                                 Statement("Myro", "backward", 1),
                                 Statement("Myro", "backward", 1, 1),
                                 Statement("Myro", "init", "COM1", 0),
                                 Statement("Myro", "backward", 1, 1),
                                 Statement("Myro", "backward", 1, 1),
                                 parallel=True
                                 )
                           ),
                     Block("Do together:", 
                           Statement("Myro", "backward", 1),
                           Statement("Myro", "forward", 1, ),
                           Statement("Myro", "backward", 1, ),
                           Statement("Myro", "forward", 1, ),
                           Statement("Myro", "backward", 1, ),
                           Statement("Myro", "forward", 1, ),
                           parallel=True),
                     ]
        process_list(self.layout, top_level)

        print process_widgets(self.layout)

        block = Gtk.EventBox()
        label = Gtk.Label("--- End of script ---")
        label.Yalign = 0
        label.Xalign = 0
        block.Add(label)
        Gtk.Drag.DestSet(block, 
                         Gtk.DestDefaults.All, 
                         target_table, 
                         Gdk.DragAction.Copy | Gdk.DragAction.Move)
        block.DragDataReceived += Gtk.DragDataReceivedHandler(handleDragDataReceived)
        self.layout.PackStart(block, True, True, 0)

        #Gtk.Layout(Gtk.Adjustment(0, 0, 100, 1, 10, 10), 
        #           Gtk.Adjustment(0, 0, 100, 1, 10, 10))
        self.widget.AddWithViewport(self.hpaned)
        self.hpaned.Add1(self.treeview)
        self.hpaned.Add2(self.layout)
        self.treeview.Show()
        self.hpaned.Show()
        self.layout.Show()
        self.widget.ShowAll()

    def grab_focus(self):
        self.treeview.GrabFocus()

    def make_store(self):
        store = Gtk.TreeStore(str)
        module = store.AppendValues("<b>Control</b>")
        for x in ["Do number of times:", "Do for each:", "Do in order:",
                  "Do together:", "Do while:", "If:"]:
            store.AppendValues(module, '<span bgcolor="%s">%s</span>' % 
                               (color_code(block_color), x))
        module = store.AppendValues("<b>Myro</b>")
        for x in ["forward", "backward", "init", "beep"]:
            store.AppendValues(module, '<span bgcolor="%s">.%s()</span>' % 
                               (color_code(statement_color), x))
        module = store.AppendValues("<b>Dinah</b>")
        for x in ["wait", "beep", "random", "ask"]:
            store.AppendValues(module, '<span bgcolor="%s">.%s()</span>' % 
                               (color_code(statement_color), x))
        return store

    def open(self):
        pass

    def get_text(self):
        return ""
 
    def save(self):
        pass

    def save_as(self):
        pass

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
except:
    Document = PlainDocument

# How to list relevant items out of a reference:
# [getattr(clr.References[2], x) for x in dir(clr.References[2]) if type(getattr(clr.References[2], x)) is type]
