import clr
clr.AddReference('gtk-sharp')
clr.AddReference('gdk-sharp')
clr.AddReference('pango-sharp')
clr.AddReference('Myro.dll')
clr.AddReference('Graphics.dll')
import Gtk
import Gdk
import Pango
import System
from utils import Language
from document import BaseDocument, MyScrolledWindow
import Myro
import Graphics

blue = Gdk.Color(70, 227, 207)
purple = Gdk.Color(227, 70, 207)
orange = Gdk.Color(243, 111, 11)
statement_color = blue
block_color = orange

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

def accepts(item):
    """
    Create a target table base on parameters.
    """
    # Formats target will accept:
    print "making accepts for", item
    target_table = System.Array[Gtk.TargetEntry]([
            Gtk.TargetEntry("application/x-dinah", 0, 0),
            ])
    return target_table

def provides(item):
    """
    Create a source table base on parameters.
    """
    # Formats source provides:
    print "making provides for", item
    source_table = System.Array[Gtk.TargetEntry]([
            Gtk.TargetEntry("application/x-dinah", 0, 0),
            ])
    return source_table

def make_drag_drop(name, item, color):
    box = Gtk.EventBox()
    img = Gtk.Image(name, Gtk.IconSize.Button)
    box.Add(img)
    img.Xalign = 1
    img.Yalign = 0
    # Set item up as a drag source:
    Gtk.Drag.SourceSet(box,
                       Gdk.ModifierType.Button1Mask,
                       provides(item), 
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
    statement.widget = enclosure
    enclosure.set_data(statement)
    enclosure.ModifyBg(Gtk.StateType.Normal, statement_color)
    vbox = Gtk.VBox()
    enclosure.Add(vbox)
    # Set item up as a drop target:
    Gtk.Drag.DestSet(enclosure, 
                     Gtk.DestDefaults.All, 
                     accepts(statement), 
                     Gdk.DragAction.Copy | Gdk.DragAction.Move)
    enclosure.DragDataReceived += Gtk.DragDataReceivedHandler(handleDragDataReceived)
    hbox = Gtk.HBox()
    img = make_drag_drop('gtk-dnd', statement, statement_color) 
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
    block.widget = enclosure
    enclosure.set_data(block)
    enclosure.ModifyBg(Gtk.StateType.Normal, block_color)
    vbox = Gtk.VBox()
    enclosure.Add(vbox)
    # Set item up as a drop target:
    Gtk.Drag.DestSet(enclosure, 
                     Gtk.DestDefaults.All, 
                     accepts(block), 
                     Gdk.DragAction.Copy | Gdk.DragAction.Move)
    enclosure.DragDataReceived += Gtk.DragDataReceivedHandler(handleDragDataReceived)
    expander_row = Gtk.HBox()
    img = make_drag_drop('gtk-dnd-multiple', block, block_color)
    expander_row.PackStart(img, False, True, 0)
    expander = Gtk.Expander(block.type)
    # Set item up as a drop target:
    Gtk.Drag.DestSet(expander, 
                     Gtk.DestDefaults.All, 
                     accepts(block), 
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
                     accepts(block), 
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
        self.widget = None

class Statement(object):
    def __init__(self, _class, _method, *args, **kwargs):
        self._class = _class
        self._method = _method
        self.args = args
        self.kwargs = kwargs
        self.widget = None

class DinahDocument(BaseDocument):
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
            provides(self.treeview), 
            Gdk.DragAction.Copy | Gdk.DragAction.Move)
        self.treeview.DragDataGet += Gtk.DragDataGetHandler(self.treeviewHandleSourceDragDataGet)
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
                         accepts(block), 
                         Gdk.DragAction.Copy | Gdk.DragAction.Move)
        block.DragDataReceived += Gtk.DragDataReceivedHandler(handleDragDataReceived)
        self.layout.PackStart(block, False, True, 0)
        
        #top_level = [] # read and parse from file

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
                         accepts(block), 
                         Gdk.DragAction.Copy | Gdk.DragAction.Move)
        block.DragDataReceived += Gtk.DragDataReceivedHandler(self.endHandleDragDataReceived)
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
        store = Gtk.TreeStore(str, object)
        #### Control
        module = store.AppendValues("<b>Control</b>", None)
        for block in [
            Block("Do times:"), 
            Block("Do for each:"), 
            Block("Do in order:"),
            Block("Do together:"), 
            Block("Do while:"), 
            Block("If:"),
            ]:
            store.AppendValues(module, '<span bgcolor="%s">%s</span>' % 
                               (color_code(block_color), block.type), block)
        # #### Dinah (utils)
        # module = store.AppendValues("<b>Dinah</b>")
        # for x in ["import", "wait", "beep", "random", "ask"]:
        #     store.AppendValues(module, '<span bgcolor="%s">.%s()</span>' % 
        #                        (color_code(statement_color), x))
        # #### Myro (will come from dll)
        # module = store.AppendValues("<b>Myro</b>")
        # for x in ["forward", "backward", "init", "beep"]:
        #     store.AppendValues(module, '<span bgcolor="%s">.%s()</span>' % 
        #                        (color_code(statement_color), x))

        # module = store.AppendValues("<b>Graphics</b>")
        # for x in ["Window", "Picture", "Polygon", "Line", "Group", 
        #           "Arrow", "Point"]:
        #     store.AppendValues(module, '<span bgcolor="%s">.%s()</span>' % 
        #                        (color_code(statement_color), x))

        # module = store.AppendValues("<b>Window members</b>")
        # for x in ["mode", "animate_step_time", ]:
        #     store.AppendValues(module, '<span bgcolor="%s">.%s()</span>' % 
        #                        (color_code(statement_color), x))

        # module = store.AppendValues("<b>Members</b>")
        # for x in ["mode", "animate_step_time", ]:
        #     store.AppendValues(module, '<span bgcolor="%s">.%s()</span>' % 
        #                        (color_code(statement_color), x))

        return store

    def open(self):
        pass

    def get_text(self):
        return ""
 
    def save(self):
        pass

    def save_as(self):
        pass

    def treeviewHandleSourceDragDataGet(self, obj, args):
        # DragDropGetArgs: args
        print "dnd get", obj, args
        targets = args.Context.Targets
        selected, treeiter = obj.Selection.GetSelected()
        item = obj.Model.GetValue(treeiter, 1)
        if item:
            data = item.type # 0- text, 1-object
            packed = System.Text.Encoding.UTF8.GetBytes(data)
            args.SelectionData.Set(targets[0], 8, packed)

    def endHandleDragDataReceived(self, obj, args):
        print "Put at end"
        bytes = args.SelectionData.Data
        data = System.Text.Encoding.UTF8.GetString(bytes)
        print "is it new or a move?", data

class Dinah(Language):
    def get_engine_class(self):
        return None

    def get_document_class(self):
        return DinahDocument

def register_language():
    return Dinah("dinah", "dnh")
