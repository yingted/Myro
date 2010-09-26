import clr
clr.AddReference('gtk-sharp')
clr.AddReference('gdk-sharp')
clr.AddReference('pango-sharp')
clr.AddReference('Microsoft.Scripting')
clr.AddReference('Microsoft.Scripting.Core')
clr.AddReference("System.Core")
#import System.Linq.Expressions as Expressions
import Gtk
import Gdk
import Pango
import System
import Microsoft.Scripting.Ast as Ast
from utils import Language
from document import BaseDocument, MyScrolledWindow
from engine import Engine
import glob

# color names
blue = Gdk.Color(70, 227, 207)
purple = Gdk.Color(227, 70, 207)
orange = Gdk.Color(243, 111, 11)
red = Gdk.Color(243, 50, 50)
pink = Gdk.Color(255, 200, 200)
green = Gdk.Color(50, 243, 50)
white = Gdk.Color(200, 200, 200)

def color_markup(color):
    r = int(color.Red/float(2**16) * 255)
    g = int(color.Green/float(2**16) * 255)
    b = int(color.Blue/float(2**16) * 255)
    return "#%02X%02X%02X" % (r, g, b)

def make_expression_label(exp):
    if exp.type in ["ctor",]:
        return "%s.%s(" % (exp.module, exp._class)
    elif exp.type in ["var",]:
        return "%s" % (exp.name,)
    elif exp.type in ["value",]:
        if exp.value_type == "Boolean":
            return "["
        else:
            return "[%s:" % exp.value_type
    elif exp.type in ["property",]:
        return "%s.%s" % (exp.instance, exp.name)
    elif exp.type in ["method",]:
        return "%s.%s(" % (exp.instance, exp.name)
    elif exp.type in ["set variable"]:
        return ""
    elif exp.type in ["set property"]:
        return ""
    elif exp.type in ["import"]:
        return "import"
    elif exp.type in ["type",]:
        return "[%s]" % (exp.name, )
    elif exp.type in ["block",]:
        return "%s" % (exp.block_type,)
    else:
        return "???"

def make_treeview_text(exptype, **kwargs):
    if exptype == "block":
        return kwargs['block_type']
    elif exptype == "ctor":
        return "%s()" % kwargs['_class']
    elif exptype == "var":
        return "<i>%s</i> - %s" % (kwargs['name'], kwargs['_class'])
    elif exptype == "value":
        return "[%s]" % kwargs['value_type']
    elif exptype == "property":
        return ".%s" % kwargs['name']
    elif exptype == "method":
        return ".%s()" % kwargs['name']
    else:
        return exptype

def make_color(exptype, **kwargs):
    if exptype in ["ctor",]:
        return white
    elif exptype in ["var",]:
        return white
    elif exptype in ["value",]:
        return white
    elif exptype in ["property",]:
        return blue
    elif exptype in ["import", "set variable", "set property"]:
        return pink
    elif exptype in ["type",]:
        return red
    elif exptype in ["method",]:
        return green
    elif exptype in ["block",]:
        return orange
    else:
        return white

class Entry(object):
    def __init__(self, default, callback):
        self.default = default
        self.callback = callback

class MyEventBox(Gtk.EventBox):
    """
    So as to add an ID.
    """

class ExpressionWidget(Gtk.EventBox):
    def set_data(self, expression):
        self.id = str(id(self))
        self.pobj = expression

class BlockWidget(Gtk.EventBox):
    def set_data(self, block):
        self.id = str(id(self))
        self.pobj = block

class Block(object):
    def __init__(self, block_type, *statements, **kwargs):
        self.pid = str(id(self))
        self.type = "block"
        self.block_type = block_type
        self.text = make_treeview_text(self.type, block_type=self.block_type)
        self.color = make_color(self.type, block_type=self.block_type)
        self.statements = statements[:]
        self.parallel = kwargs["parallel"] if "parallel" in kwargs else False
        self.drops_go = kwargs["drops_go"] if "drops_go" in kwargs else "before"
        self.widget = None

class Expression(object):
    def __init__(self, exptype, *args, **kwargs):
        self.pid = str(id(self))
        self.type = exptype
        self.separator = kwargs["separator"] if "separator" in kwargs else ","
        self.end = kwargs["end"] if "end" in kwargs else ""
        self.text = make_treeview_text(self.type, **kwargs)
        self.color = make_color(self.type, **kwargs)
        self.drops_go = kwargs["drops_go"] if "drops_go" in kwargs else "before"
        self.members = kwargs["members"] if "members" in kwargs else []
        self.widget = None
        self.args = args
        self.kwargs = kwargs
    def __getattr__(self, attr):
        if attr in self.kwargs:
            return self.kwargs[attr]
        raise AttributeError("no such attr: '%s'" % attr)

E = Expression

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
        #block.ModifyBg(Gtk.StateType.Normal, block.color)
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
            Block("Do times:"), 
            Block("Do for each:"), 
            Block("Do in order:"),
            Block("Do together:"), 
            Block("Do while:"), 
            Block("If:"),
            ]:
            store.AppendValues(module, '<span bgcolor="%s">%s</span>' % 
                               (color_markup(block.color), block.text), block)

        #### Variable
        self.variables = store.AppendValues("<b>Variables</b>", None)

        #### Values
        module = store.AppendValues("<b>Direct Values</b>", None)
        for value in [
            Expression("value", 0, value_type="Integer", end="]"), 
            Expression("value", 0.0, value_type="Floating point", end="]"), 
            Expression("value", "", value_type="String", end="]"), 
            Expression("value", "", value_type="Filename", end="]"), 
            Expression("value", True, value_type="Boolean", end="]"), 
            ]:
            store.AppendValues(module, '<span bgcolor="%s">%s</span>' % 
                               (color_markup(value.color), value.text), 
                               value)

        #### Imports
        module = store.AppendValues("<b>Statements</b>", None)
        for statement in [
            Expression("import", 
                       Entry("", callback=self.import_module_cb),
                       ), 
            Expression("set variable", 
                       Entry("", callback=self.define_variable_cb),
                       ":=", 
                       E("type", name="Expression"), 
                       separator=""), 
            Expression("set property", 
                       E("type", name="Property"), 
                       ":=", 
                       E("type", name="Expression"), 
                       separator=""), 
            ]:
            store.AppendValues(module, '<span bgcolor="%s">%s</span>' % 
                               (color_markup(statement.color), statement.text), 
                               statement)

        # #### Dinah (utils)
        # module = store.AppendValues("<b>Dinah</b>")
        # for x in ["import", "wait", "beep", "random", "ask"]:
        #     store.AppendValues(module, '<span bgcolor="%s">.%s()</span>' % 
        #                        (color_code(expression_color), x))
        # #### Myro (will come from dll)
        # module = store.AppendValues("<b>Myro</b>")
        # for x in ["forward", "backward", "init", "beep"]:
        #     store.AppendValues(module, '<span bgcolor="%s">.%s()</span>' % 
        #                        (color_code(expression_color), x))

        # module = store.AppendValues("<b>Graphics</b>")
        # for x in ["Window", "Picture", "Polygon", "Line", "Group", 
        #           "Arrow", "Point"]:
        #     store.AppendValues(module, '<span bgcolor="%s">.%s()</span>' % 
        #                        (color_code(expression_color), x))

        # module = store.AppendValues("<b>Window members</b>")
        # for x in ["mode", "animate_step_time", ]:
        #     store.AppendValues(module, '<span bgcolor="%s">.%s()</span>' % 
        #                        (color_code(expression_color), x))

        # module = store.AppendValues("<b>Members</b>")
        # for x in ["mode", "animate_step_time", ]:
        #     store.AppendValues(module, '<span bgcolor="%s">.%s()</span>' % 
        #                        (color_code(expression_color), x))

        return store

    def define_variable_cb(self, widget, event):
        result = widget.Text
        return self.define_variable(widget, result)

    def define_variable(self, widget, result):
        store = self.treeview.Model
        exp = None
        if result == "win":
            exp = Expression("var", name="win",
                             _class="Graphics.Window",
                             members=[
                    Expression("property", name="animate_step_time"), 
                    Expression("property", name="title"),
                    Expression("property", name="mode"),
                    ], 
                             )
        elif result == "pic":
            exp = Expression("var", name="pic", 
                             _class="Graphics.Picture", 
                             members=[
                    Expression("method", 
                               E("type", name="Graphics.Window"), 
                               name="draw", end=")"), 
                    Expression("method", 
                               E("type", name="Integer"), 
                               E("type", name="Integer"), 
                               name="move", end=")"), 
                    Expression("method", 
                               E("type", name="Integer"), 
                               name="rotate", end=")"), 
                    Expression("method", 
                               E("type", name="Integer"), 
                               E("type", name="Integer"), 
                               name="move_to", end=")"), 
                    Expression("method", 
                               E("type", name="Integer"), 
                               name="rotate_to", end=")"), 
                    ],
                             )
        if exp:
            _class = store.AppendValues(self.variables,
                                        '<span bgcolor="%s">%s</span>' % 
                                        (color_markup(exp.color), exp.text), 
                                        exp)
            for member in exp.members:
                member.instance = exp.name
                store.AppendValues(_class, 
                                   '<span bgcolor="%s">%s</span>' % 
                                   (color_markup(member.color), member.text), 
                                   member)
            widget.CanFocus = False
        return True

    def import_module_cb(self, widget, event):
        result = widget.Text
        return self.import_module(widget, result)
        

    def find_module(self, module_name):
        for assembly in clr.References:
            #print assembly
            for t in assembly.GetTypes():
                if module_name in t.Name:
                    return assembly
        return None

    def import_module(self, widget, module_name):
        store = self.treeview.Model
        module = self.find_module(module_name)
        if module: # gets assembly from references
            #print "Found module", module
            # Graphics
            module_path = store.AppendValues("<b>%s</b>" % module_name, None)
            #print "Getting classes:"
            for _class in module.GetTypes():
                #print "   _class:", _class.Name
                # Graphics, Window, Button, Point, etc.
                exps = []
                # color_map
                for attr in _class.GetMethods():
                    #print "      attr:", attr.Name
                    try:
                        method = _class.GetMethod(attr.Name)
                    except:
                        #print "skipping ambiguous name", attr.Name
                        pass
                    if (attr.IsSpecialName and 
                        attr.IsPublic and 
                        not attr.Name.startswith("_") and 
                        (attr.Name.lower() == attr.Name)):
                        #print attr.Name
                        # eg, module_name is "Graphics", attr is:
                        # color_map, color_rgb, color_names, init
                        params = method.GetParameters()
                        pexps = []
                        for parameter in params:
                            pexps.append(
                                Expression("type", 
                                           name=parameter.ParameterType.Name))
                        e = Expression("method", *pexps, 
                                       module=module_name,
                                       name=attr.Name, 
                                       end=")")
                        exps.append(e)
                #for attr in module.GetFields():
                #    if not attr.Name.startswith("_") and (attr.Name.lower() == 
                #                                          attr.Name):
                #        e = Expression("property", name=attr.Name
                #                       name=attr.Name, end=")")
                #        exps.append(e)
                # Now, add the methods to the tree:
                #print "exps:", exps
                for exp in exps:
                    _class = store.AppendValues(module_path, 
                                                '<span bgcolor="%s">%s</span>' % 
                                                (color_markup(exp.color), exp.text), 
                                                exp)
                    for member in exp.members:
                        store.AppendValues(_class, 
                                           '<span bgcolor="%s">%s</span>' % 
                                           (color_markup(member.color), member.text), 
                                           member)
            self.treeview.ShowAll()
            widget.CanFocus = False
            return True 
        # if module_name == "Graphics":
        #     module = store.AppendValues("<b>Graphics</b>", None)
        #     for exp in [
        #         Expression("ctor", E("type", name="String"), 
        #                    _class="Window",
        #                    end=")",
        #                    ),
        #         Expression("ctor", E("type", name="Filename"), 
        #                    _class="Picture", 
        #                    end=")",
        #                    ), 
        #         Expression("ctor", 
        #                    E("type", name="Integer"), 
        #                    E("type", name="Integer"), 
        #                    _class="Point",
        #                    end=")",
        #                    ),
        #         Expression("ctor", 
        #                    Expression("type", name="Graphics.Point"), 
        #                    Expression("type", name="Graphics.Point"), 
        #                    _class="Line", 
        #                    end=")",
        #                    ), 
        #         ]:
        #         exp.module = 'Graphics'
        #         _class = store.AppendValues(module, 
        #                                     '<span bgcolor="%s">%s</span>' % 
        #                                     (color_markup(exp.color), exp.text), 
        #                                     exp)
        #         for member in exp.members:
        #             member.module = 'Graphics'
        #             member._class = exp._class
        #             store.AppendValues(_class, 
        #                                '<span bgcolor="%s">%s</span>' % 
        #                                (color_markup(member.color), member.text), 
        #                                member)
        # elif module_name == "Myro":
        #     module = store.AppendValues("<b>Myro</b>", None)
        #     widget.CanFocus = False
        #     self.treeview.ShowAll()
        #     return True 
        return False # Gtk needs boolean?

    def open(self):
        # top_level = [Block("Do together:", 
        #                    Expression("Myro", "forward", 1),
        #                    Expression("Myro", "forward", 1, .5),
        #                    parallel=True),
        #              Block("Do in order:", 
        #                    Expression("Myro", "forward", 1, 1),
        #                    Expression("Myro", "init", "COM1", 0),
        #                    Expression("Myro", "backward", 1, 1),
        #                    Expression("Myro", "backward", 1, 1),
        #                    Block("Do together:",
        #                          Expression("Myro", "init", "COM1", 0),
        #                          Expression("Myro", "backward", 1),
        #                          Expression("Myro", "backward", 1, 1),
        #                          Expression("Myro", "init", "COM1", 0),
        #                          Expression("Myro", "backward", 1, 1),
        #                          Expression("Myro", "backward", 1, 1),
        #                          parallel=True
        #                          )
        #                    ),
        #              Block("Do together:", 
        #                    Expression("Myro", "backward", 1),
        #                    Expression("Myro", "forward", 1, ),
        #                    Expression("Myro", "backward", 1, ),
        #                    Expression("Myro", "forward", 1, ),
        #                    Expression("Myro", "backward", 1, ),
        #                    Expression("Myro", "forward", 1, ),
        #                    parallel=True),
        #              ]
        # self.process_list(self.layout, top_level)
        self.layout.ShowAll()

    def get_text(self):
        return self.layout
 
    def save(self):
        #print self.process_widgets(self.layout)
        pass

    def save_as(self):
        #print self.process_widgets(self.layout)
        pass

    def treeviewHandleSourceDragDataGet(self, obj, args):
        #print "treeviewHandleSourceDragDataGet!"
        # DragDropGetArgs: args
        #print "dnd get", obj, args
        targets = args.Context.Targets
        selected, treeiter = obj.Selection.GetSelected()
        item = obj.Model.GetValue(treeiter, 1) # 0- text, 1-object
        if item:
            data = "create:%s" % item.pid
            self.lookup[data] = item
            self.lookup[item.pid] = item
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

    def typeDragDataReceived(self, obj, args):
        # obj is the dropped upon label to be replaced
        #print "Received onto a type place holder!"
        #print obj, str(id(obj))
        if str(id(obj)) in self.layouts:
            #print "Yes, label's layout found!"
            layout = self.layouts[str(id(obj))]
            bytes = args.SelectionData.Data
            data = System.Text.Encoding.UTF8.GetString(bytes)
            if data in self.lookup:
                layout.Remove(obj)
                #print "Yes, item to drop found!"
                item = self.lookup[data]
                if data.startswith("create:"):
                    direct_entry_exp = self.process_list(layout, [item], icons=False)
                    layout.ModifyBg(Gtk.StateType.Normal, item.color)
                    layout.Parent.ModifyBg(Gtk.StateType.Normal, item.color)
                    layout.Parent.Parent.ModifyBg(Gtk.StateType.Normal, item.color)
                    layout.ShowAll()
                else:
                    print "Copy reference to variable here?"
            else:
                print "drop item not found"
        else:
            print "label not found"

    def typeEnclosureDragDataReceived(self, obj, args):
        # FIXME: remove item [Integer] in the related hbox
        # obj is the dropped upon label to be replaced
        #print "Received onto a type place holder!"
        #print obj, str(id(obj))
        if str(id(obj)) in self.layouts:
            #print "Yes, label's layout found!"
            layout = self.layouts[str(id(obj))]
            bytes = args.SelectionData.Data
            data = System.Text.Encoding.UTF8.GetString(bytes)
            if data in self.lookup:
                layout.Remove(obj)
                #print "Yes, item to drop found!"
                item = self.lookup[data]
                if data.startswith("create:"):
                    direct_entry_exp = self.process_list(layout, [item], icons=False)
                    layout.ModifyBg(Gtk.StateType.Normal, item.color)
                    layout.Parent.ModifyBg(Gtk.StateType.Normal, item.color)
                    layout.Parent.Parent.ModifyBg(Gtk.StateType.Normal, item.color)
                    layout.ShowAll()
                else:
                    print "Copy reference to variable here?"
            else:
                print "drop item not found"
        else:
            print "label not found"

    def handleDragDataReceived(self, obj, args, where):
        #print "Received!"
        bytes = args.SelectionData.Data
        data = System.Text.Encoding.UTF8.GetString(bytes)
        if data in self.lookup:
            item = self.lookup[data]
            if data.startswith("create:"):
                # Where to add? get container, and use "where"
                # obj is the item we are dropping onto:
                layout = self.layouts[obj]
                # construct the new dropped expression/statement:
                widgets = self.process_list(layout, [item])
                #self.lookup[widgets[0].widget.id] = widgets[0].widget
                # put it in right place:
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
            else: # Move!
                print "Move '%s'" % data
                # Need to know if within same parent, or diff parents?
        else:
            print "unknown object: '%s'" % data
            print self.lookup

    def handleSourceDragDataGet(self, obj, args):
        #print "handleSourceDragDropGet!"
        # DragDropGetArgs: args
        #print "dnd get", obj, args
        targets = args.Context.Targets
        data = obj.id
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

    def make_drag_drop(self, name, item, color, widget_id):
        box = MyEventBox()
        box.id = widget_id
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
                retval.extend([("Block", widget.pobj.block_type, self.process_widgets(widget))])
            elif isinstance(widget, ExpressionWidget):
                retval.extend([("Expression", widget.pobj.type, 
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

    def process_list(self, layout, items, parallel=False, icons=True):
        retval = []
        for item in items:
            if isinstance(item, Block):
                box = self.process_block(item, layout, icons)
            elif isinstance(item, Expression):
                box = self.process_expression(item, layout, icons)
            else:
                raise Exception("unknown item: '%s'" % item)
            layout.PackStart(box, False, True, 0)
            if parallel:
                layout.PackStart(Gtk.VSeparator(), False, True, 0)
            else:
                layout.PackStart(Gtk.HSeparator(), False, True, 0)
            retval.append(box)
        return retval

    def process_expression(self, expression, layout, icons=True):
        enclosure = ExpressionWidget()
        expression.widget = enclosure
        enclosure.set_data(expression)
        self.lookup[enclosure.id] = expression
        enclosure.ModifyBg(Gtk.StateType.Normal, expression.color)
        vbox = Gtk.VBox()
        enclosure.Add(vbox)
        hbox = Gtk.HBox()
        # Set item up as a drop target:
        if expression.type == "type":
            Gtk.Drag.DestSet(enclosure, 
                             Gtk.DestDefaults.All, 
                             self.accepts("accepts a specific type, or general Expression"), 
                             Gdk.DragAction.Copy | Gdk.DragAction.Move)
            self.layouts[str(id(enclosure))] = hbox
            enclosure.DragDataReceived += Gtk.DragDataReceivedHandler(
                self.typeDragDataReceived)
        elif layout:
            Gtk.Drag.DestSet(enclosure, 
                             Gtk.DestDefaults.All, 
                             self.accepts(expression), 
                             Gdk.DragAction.Copy | Gdk.DragAction.Move)
            self.layouts[enclosure] = layout
            enclosure.DragDataReceived += Gtk.DragDataReceivedHandler(
                self.beforeHandleDragDataReceived)
        if layout and icons:
            img = self.make_drag_drop('gtk-dnd', expression, expression.color, 
                                      enclosure.id) 
            hbox.PackStart(img, False, True, 0)

        ### FIXME: on expression "type", the whole area needs to be connected
        ### to this handler:
        label = Gtk.Label(make_expression_label(expression))
        if expression.type == "type":
            Gtk.Drag.DestSet(label, 
                             Gtk.DestDefaults.All, 
                             self.accepts("accepts a specific type, or general Expression"), 
                             Gdk.DragAction.Copy | Gdk.DragAction.Move)
            self.layouts[str(id(label))] = hbox
            label.DragDataReceived += Gtk.DragDataReceivedHandler(
                self.typeEnclosureDragDataReceived)
        ###
        hbox.PackStart(label, False, True, 0)
        for count in range(len(expression.args)):
            arg = expression.args[count]
            if arg == ":=":
                label = Gtk.Label("<=")
                hbox.PackStart(label, False, True, 0)
            elif isinstance(arg, Entry):
                entry = Gtk.Entry(arg.default)
                entry.WidthChars = 7
                entry.FocusOutEvent += arg.callback
                hbox.PackStart(entry, False, True, 0)
            elif isinstance(arg, basestring):
                entry = Gtk.Entry(arg)
                entry.WidthChars = 7
                hbox.PackStart(entry, False, True, 0)
            elif isinstance(arg, bool):
                entry = Gtk.CheckButton("Boolean")
                entry.Active = arg
                hbox.PackStart(entry, False, True, 0)
            elif isinstance(arg, int): # maybe use Gtk.SpinButton?
                entry = Gtk.Entry(str(arg))
                entry.WidthChars = 3
                hbox.PackStart(entry, False, True, 0)
            elif isinstance(arg, float):
                entry = Gtk.Entry(str(arg))
                entry.WidthChars = 5
                hbox.PackStart(entry, False, True, 0)
            else:
                #print "process_expression, statements:", arg, type(arg)
                box = self.process_expression(arg, None)
                hbox.PackStart(box, False, True, 0)
            if count < len(expression.args) - 1:
                comma = Gtk.Label(expression.separator)
                hbox.PackStart(comma, False, True, 0)

        if expression.end:
            end = Gtk.Label(expression.end)
            hbox.PackStart(end, False, True, 0)

        vbox.PackStart(hbox, False, True, 0)
        return enclosure

    def process_block(self, block, layout, icons=True):
        enclosure = BlockWidget()
        block.widget = enclosure
        enclosure.set_data(block)
        self.lookup[enclosure.id] = block
        enclosure.ModifyBg(Gtk.StateType.Normal, block.color)
        vbox = Gtk.VBox()
        enclosure.Add(vbox)
        # Set item up as a drop target:
        Gtk.Drag.DestSet(enclosure, 
                         Gtk.DestDefaults.All, 
                         self.accepts(block), 
                         Gdk.DragAction.Copy | Gdk.DragAction.Move)
        expander_row = Gtk.HBox()
        if icons:
            img = self.make_drag_drop('gtk-dnd-multiple', block, block.color,
                                      enclosure.id)
            expander_row.PackStart(img, False, True, 0)
        expander = Gtk.Expander(block.block_type)
        # Set item up as a drop target:
        Gtk.Drag.DestSet(expander, 
                         Gtk.DestDefaults.All, 
                         self.accepts(block), 
                         Gdk.DragAction.Copy | Gdk.DragAction.Move)
        expander.Expanded = True
        expander_row.PackStart(expander, True, True, 0)
        # Add other items to expander_row, if needed:
        extras = None
        if block.block_type == "Do times:":
            extras = self.process_expression(Expression("type", name="Integer"), expander_row)
        elif block.block_type == "Do for each:":
            extras = self.process_expression(
                Expression("set variable", 
                           Entry("", callback=self.define_variable_cb),
                           ":=", 
                           E("type", name="Expression"), 
                           separator=""), 
                expander_row)
        elif block.block_type == "Do in order:":
            pass
        elif block.block_type == "Do together:":
            pass
        elif block.block_type == "Do while:":
            extras = self.process_expression(Expression("type", name="Boolean"), expander_row)
        elif block.block_type == "If:":
            extras = self.process_expression(Expression("type", name="Boolean"), expander_row)
        else:
            raise AttributeError("unknown block_type: '%s'" % block.block_type)
        if extras:
            expander_row.PackStart(extras, True, True, 0)
        if block.parallel:
            box = Gtk.HBox()
        else:
            box = Gtk.VBox()
        expander.Add(box)
        self.process_list(box, block.statements, parallel=block.parallel)
        end = Gtk.EventBox()
        #block.ModifyBg(Gtk.StateType.Normal, block.color)
        label = Gtk.Label("End of %s" % block.type)
        label.Xalign = 0
        end.Add(label)
        Gtk.Drag.DestSet(end, 
                         Gtk.DestDefaults.All, 
                         self.accepts(block), 
                         Gdk.DragAction.Copy | Gdk.DragAction.Move)
        self.layouts[enclosure] = layout
        enclosure.DragDataReceived += Gtk.DragDataReceivedHandler(self.beforeHandleDragDataReceived)
        self.layouts[expander] = layout
        expander.DragDataReceived += Gtk.DragDataReceivedHandler(self.beforeHandleDragDataReceived)
        self.layouts[end] = expander.Child
        end.DragDataReceived += Gtk.DragDataReceivedHandler(self.endHandleDragDataReceived)
        box.PackEnd(end, True, True, 0)
        vbox.PackStart(expander_row, True, True, 0)
        vbox.PackStart(Gtk.HSeparator(), True, True, 0)
        return enclosure

class DinahEngine(Engine):
    def __init__(self, manager):
        super(DinahEngine, self).__init__(manager, "dinah")
        self.text_based = False

    def setup(self):
        super(DinahEngine, self).setup()
        for file in glob.glob("modules/*.dll"):
            #path, dll_name = os.path.split(file)
            clr.AddReference(file)

    def execute(self, layout):
        #print layout
        pass
        #program = Ast.Utils.Lambda(type(object), "Test")
        #statements = []
        #clr.AddReference("System.Core")
        #import Microsoft.Scripting.Ast as Ast
        #import System.Linq
        #dir(System.Linq.Expressions)
        #n = program.Variable(int, "n")

    def execute_file(self, filename):
        self.stdout.write("Run filename '%s'!\n" % filename)

class Dinah(Language):
    def get_engine_class(self):
        return DinahEngine

    def get_document_class(self):
        return DinahDocument

def register_language():
    return Dinah("dinah", "dnh")
