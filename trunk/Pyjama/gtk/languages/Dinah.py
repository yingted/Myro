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

class StatementWidget(Gtk.EventBox):
    def set_data(self, statement):
        self._class = statement._class
        self._method = statement._method
        self.args = statement.args
        self.kwargs = statement.kwargs

class BlockWidget(Gtk.EventBox):
    def set_data(self, block):
        self.type = block.type
        self.statements = block.statements[:]
        self.parallel = block.parallel

class Block(object):
    def __init__(self, block_type, *statements, **kwargs):
        self.type = block_type
        self.statements = statements[:]
        self.parallel = kwargs["parallel"] if "parallel" in kwargs else False
        self.create = kwargs["create"] if "create" in kwargs else False
        self.drops_go = kwargs["drops_go"] if "drops_go" in kwargs else "before"
        self.widget = None

class Statement(object):
    def __init__(self, _class, _method, *args, **kwargs):
        self._class = _class
        self._method = _method
        self.type = ".%s()" % self._method
        self.args = args
        self.kwargs = kwargs
        self.create = kwargs["create"] if "create" in kwargs else False
        self.drops_go = kwargs["drops_go"] if "drops_go" in kwargs else "before"
        self.widget = None

class DinahDocument(BaseDocument):
    def make_widget(self):
        self.layouts = {}
        self.lookup = {}
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
            self.provides(self.treeview), 
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
                         self.accepts(block), 
                         Gdk.DragAction.Copy | Gdk.DragAction.Move)
        self.layouts[block] = self.layout
        block.DragDataReceived += Gtk.DragDataReceivedHandler(self.afterHandleDragDataReceived)
        self.layout.PackStart(block, False, True, 0)
        # initial stuff here
        block = Gtk.EventBox()
        label = Gtk.Label("--- End of script ---")
        label.Yalign = 0
        label.Xalign = 0
        block.Add(label)
        Gtk.Drag.DestSet(block, 
                         Gtk.DestDefaults.All, 
                         self.accepts(block), 
                         Gdk.DragAction.Copy | Gdk.DragAction.Move)
        self.layouts[block] = self.layout
        block.DragDataReceived += Gtk.DragDataReceivedHandler(self.endHandleDragDataReceived)
        self.layout.PackEnd(block, True, True, 0)

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
            Block("Do times:", create=True), 
            Block("Do for each:", create=True), 
            Block("Do in order:", create=True),
            Block("Do together:", create=True), 
            Block("Do while:", create=True), 
            Block("If:", create=True),
            ]:
            store.AppendValues(module, '<span bgcolor="%s">%s</span>' % 
                               (color_code(block_color), block.type), block)

        #### Imports
        module = store.AppendValues("<b>Import</b>", None)
        for statement in [
            Statement("import", "Graphics", create=True), 
            Statement("import", "Myro", create=True), 
            ]:
            store.AppendValues(module, '<span bgcolor="%s">%s</span>' % 
                               (color_code(statement_color), statement.type), statement)

        #### Graphics
        module = store.AppendValues("<b>Graphics</b>", None)
        for statement in [
            Statement("Graphics", "Window", "Title", create=True), 
            Statement("Graphics", "Picture", "filename", create=True), 
            Statement("Graphics", "Point", 0, 0, create=True),
            Statement("Graphics", "Line", 
                      Statement("Graphics", "Point", 0, 0), 
                      Statement("Graphics", "Point", 1, 1), create=True), 
            ]:
            store.AppendValues(module, '<span bgcolor="%s">%s</span>' % 
                               (color_code(statement_color), statement.type), statement)

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
        self.process_list(self.layout, top_level)
        self.layout.ShowAll()

    def get_text(self):
        return ""
 
    def save(self):
        print self.process_widgets(self.layout)

    def save_as(self):
        print self.process_widgets(self.layout)

    def treeviewHandleSourceDragDataGet(self, obj, args):
        # DragDropGetArgs: args
        #print "dnd get", obj, args
        targets = args.Context.Targets
        selected, treeiter = obj.Selection.GetSelected()
        item = obj.Model.GetValue(treeiter, 1)
        if item:
            data = "Create: %s" % item.type # 0- text, 1-object
            self.lookup[data] = item
            packed = System.Text.Encoding.UTF8.GetBytes(data)
            args.SelectionData.Set(targets[0], 8, packed)

    def beforeHandleDragDataReceived(self, obj, args):
        self.handleDragDataReceived(obj, args, "before")

    def afterHandleDragDataReceived(self, obj, args):
        self.handleDragDataReceived(obj, args, "after")

    def endHandleDragDataReceived(self, obj, args):
        self.handleDragDataReceived(obj, args, "end")

    def startHandleDragDataReceived(self, obj, args):
        self.handleDragDataReceived(obj, args, "start")

    def handleDragDataReceived(self, obj, args, where):
        bytes = args.SelectionData.Data
        data = System.Text.Encoding.UTF8.GetString(bytes)
        if data in self.lookup:
            item = self.lookup[data]
            if item.create:
                # Where to add? get container from item?
                layout = self.layouts[obj]
                widgets = self.process_list(layout, [item])
                #where = obj.pjobj.drops_go, but not for Gtk widgets
                position = layout.ChildGetProperty(obj, "position").Val
                for widget in widgets:
                    # 0 to pos, negative at end
                    if where == "before":
                        layout.ReorderChild(widget, position - 1) 
                    elif where == "after":
                        layout.ReorderChild(widget, position + 1) 
                    elif where == "end":
                        layout.ReorderChild(widget, -1) 
                    elif where == "start":
                        layout.ReorderChild(widget, 0) 
                layout.ShowAll()
            else:
                print "Move!"
                # Need to know if within same parent, or diff parents
        else:
            print "Huh?"

    def handleSourceDragDataGet(self, obj, args):
        # DragDropGetArgs: args
        #print "dnd get", obj, args
        targets = args.Context.Targets
        data = "ok"
        packed = System.Text.Encoding.UTF8.GetBytes(data)
        args.SelectionData.Set(targets[0], 8, packed)

    def accepts(self, item):
        """
        Create a target table base on parameters.
        """
        # Formats target will accept:
        #print "making accepts for", item
        target_table = System.Array[Gtk.TargetEntry]([
                Gtk.TargetEntry("application/x-dinah", 0, 0),
                ])
        return target_table

    def provides(self, item):
        """
        Create a source table base on parameters.
        """
        # Formats source provides:
        #print "making provides for", item
        source_table = System.Array[Gtk.TargetEntry]([
                Gtk.TargetEntry("application/x-dinah", 0, 0),
                ])
        return source_table

    def make_drag_drop(self, name, item, color):
        box = Gtk.EventBox()
        img = Gtk.Image(name, Gtk.IconSize.Button)
        img.Show()
        box.Add(img)
        img.Xalign = 1
        img.Yalign = 0
        # Set item up as a drag source:
        Gtk.Drag.SourceSet(box,
                           Gdk.ModifierType.Button1Mask,
                           self.provides(item), 
                           Gdk.DragAction.Copy | Gdk.DragAction.Move)
        box.DragDataGet += Gtk.DragDataGetHandler(self.handleSourceDragDataGet)
        return box

    def process_widgets(self, layout):
        retval = []
        for widget in layout:
            if isinstance(widget, BlockWidget):
                retval.extend([("Block", widget.type, self.process_widgets(widget))])
            elif isinstance(widget, StatementWidget):
                retval.extend([("Statement", widget._class, 
                                self.process_widgets(widget))])
            elif isinstance(widget, Gtk.Entry):
                retval.extend([('Entry', widget.Text)])
            elif isinstance(widget, Gtk.Box):
                retval.extend(self.process_widgets(widget))
            elif isinstance(widget, Gtk.Expander):
                retval.extend(self.process_widgets(widget))
            elif isinstance(widget, Gtk.EventBox):
                retval.extend(self.process_widgets(widget))
        return retval

    def process_list(self, layout, items, parallel=False):
        retval = []
        for item in items:
            if isinstance(item, Block):
                box = self.process_block(item, layout)
            elif isinstance(item, Statement):
                box = self.process_statement(item, layout)
            else:
                print "error!"
            layout.PackStart(box, False, True, 0)
            if parallel:
                layout.PackStart(Gtk.VSeparator(), False, True, 0)
            else:
                layout.PackStart(Gtk.HSeparator(), False, True, 0)
            retval.append(box)
        return retval

    def process_statement(self, statement, layout):
        enclosure = StatementWidget()
        enclosure.pjobj = statement
        statement.widget = enclosure
        enclosure.set_data(statement)
        enclosure.ModifyBg(Gtk.StateType.Normal, statement_color)
        vbox = Gtk.VBox()
        enclosure.Add(vbox)
        # Set item up as a drop target:
        Gtk.Drag.DestSet(enclosure, 
                         Gtk.DestDefaults.All, 
                         self.accepts(statement), 
                         Gdk.DragAction.Copy | Gdk.DragAction.Move)
        if layout:
            self.layouts[enclosure] = layout
            enclosure.DragDataReceived += Gtk.DragDataReceivedHandler(self.beforeHandleDragDataReceived)
        hbox = Gtk.HBox()
        if layout:
            img = self.make_drag_drop('gtk-dnd', statement, statement_color) 
            hbox.PackStart(img, False, True, 0)

        label = Gtk.Label("%s.%s(" % (statement._class, statement._method))
        hbox.PackStart(label, False, True, 0)
        for count in range(len(statement.args)):
            arg = statement.args[count]
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
                print "process_statement, expressions:", arg, type(arg)
                box = self.process_statement(arg, None)
                hbox.PackStart(box, False, True, 0)
            if count < len(statement.args) - 1:
                comma = Gtk.Label(", ")
                hbox.PackStart(comma, False, True, 0)

        label = Gtk.Label(")")
        hbox.PackStart(label, False, True, 0)
        vbox.PackStart(hbox, False, True, 0)
        return enclosure

    def process_block(self, block, layout):
        enclosure = BlockWidget()
        block.widget = enclosure
        enclosure.pjobj = block
        enclosure.set_data(block)
        enclosure.ModifyBg(Gtk.StateType.Normal, block_color)
        vbox = Gtk.VBox()
        enclosure.Add(vbox)
        # Set item up as a drop target:
        Gtk.Drag.DestSet(enclosure, 
                         Gtk.DestDefaults.All, 
                         self.accepts(block), 
                         Gdk.DragAction.Copy | Gdk.DragAction.Move)
        expander_row = Gtk.HBox()
        img = self.make_drag_drop('gtk-dnd-multiple', block, block_color)
        expander_row.PackStart(img, False, True, 0)
        expander = Gtk.Expander(block.type)
        # Set item up as a drop target:
        Gtk.Drag.DestSet(expander, 
                         Gtk.DestDefaults.All, 
                         self.accepts(block), 
                         Gdk.DragAction.Copy | Gdk.DragAction.Move)
        expander.Expanded = True
        expander_row.PackStart(expander, True, True, 0)
        if block.parallel:
            box = Gtk.HBox()
        else:
            box = Gtk.VBox()
        expander.Add(box)
        self.process_list(box, block.statements, parallel=block.parallel)
        end = Gtk.EventBox()
        #block.ModifyBg(Gtk.StateType.Normal, block_color)
        label = Gtk.Label("End of %s" % block.type)
        label.Xalign = 0
        end.Add(label)
        Gtk.Drag.DestSet(end, 
                         Gtk.DestDefaults.All, 
                         self.accepts(block), 
                         Gdk.DragAction.Copy | Gdk.DragAction.Move)
        self.layouts[enclosure] = expander.Child
        enclosure.DragDataReceived += Gtk.DragDataReceivedHandler(self.startHandleDragDataReceived)
        self.layouts[expander] = expander.Child
        expander.DragDataReceived += Gtk.DragDataReceivedHandler(self.startHandleDragDataReceived)
        self.layouts[end] = expander.Child
        end.DragDataReceived += Gtk.DragDataReceivedHandler(self.endHandleDragDataReceived)
        box.PackEnd(end, True, True, 0)
        vbox.PackStart(expander_row, True, True, 0)
        vbox.PackStart(Gtk.HSeparator(), True, True, 0)
        return enclosure

class Dinah(Language):
    def get_engine_class(self):
        return None

    def get_document_class(self):
        return DinahDocument

def register_language():
    return Dinah("dinah", "dnh")
