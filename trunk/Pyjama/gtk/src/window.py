import Gtk

class Window(object):
    def make_gui(self, menu):
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

        newtb = Gtk.ToolButton(Gtk.Stock.New)
        newtb.Clicked += self.on_new_file
        opentb = Gtk.ToolButton(Gtk.Stock.Open)
        opentb.Clicked += self.on_open_file
        savetb = Gtk.ToolButton(Gtk.Stock.Save)
        savetb.Clicked += self.on_save_file
        sep = Gtk.SeparatorToolItem()
        quittb = Gtk.ToolButton(Gtk.Stock.Quit)
        quittb.Clicked += self.on_quit

        self.toolbar.Insert(newtb, 0)
        self.toolbar.Insert(opentb, 1)
        self.toolbar.Insert(savetb, 2)
        self.toolbar.Insert(sep, 3)
        self.toolbar.Insert(quittb, 4)

