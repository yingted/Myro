import Gtk

class Window(object):
    def make_gui(self, menu, toolbar):
        self.menubar = Gtk.MenuBar()
        for text, items in menu:
            submenu = Gtk.Menu()
            accel_group = Gtk.AccelGroup()
            self.window.AddAccelGroup(accel_group)
            for row in items:
                if row is None:
                    menuitem = Gtk.SeparatorMenuItem()
                else:
                    subtext, img, accel, function = row
                    if img is None:
                        menuitem = Gtk.MenuItem(subtext)
                    else:
                        menuitem = Gtk.ImageMenuItem(img, accel_group)
                    menuitem.Activated += function
                    if accel:
                        key, mod = Gtk.Accelerator.Parse(accel)
                        menuitem.AddAccelerator("activate", accel_group, 
                                                key, mod, 
                                                Gtk.AccelFlags.Visible)
                submenu.Append(menuitem)
            menuitem = Gtk.MenuItem(text)
            menuitem.Submenu = submenu
            self.menubar.Append(menuitem)
        # ---------------------
        self.toolbar = Gtk.Toolbar()
        self.toolbar.ToolbarStyle = Gtk.ToolbarStyle.Icons
        i = 0
        for (img, funtion) in toolbar:
            if img is None:
                tool_item = Gtk.SeparatorToolItem()
            else:
                tool_item = Gtk.ToolButton(img)
                tool_item.Clicked += function
            self.toolbar.Insert(tool_item, i)
            i += 1
