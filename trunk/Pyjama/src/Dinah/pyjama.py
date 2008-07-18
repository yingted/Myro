#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk
import gtk.glade

class Pyjama:

    '''This is the the class for the main window for Pyjama.'''

    def hideItem(self,widget,data=None):
        self.wTree.get_widget(widget).hide()

    def __init__(self):
        '''Find the file and uses it.'''
        self.gladefile="pyjama.glade"
        self.wTree=gtk.glade.XML(self.gladefile)
        self.window=self.wTree.get_widget("Pyjama")

        #Sets up both closing the window and using the quit menu item to exit.
        exitDic = {"on_Pyjama_destroy": gtk.main_quit, "on_quit_activate": gtk.main_quit}
        self.wTree.signal_autoconnect(exitDic)

        #Switches the interpreter and the code window.
        #Can be set up using turning pack start on and off, though it isn't working right now.

        #makes dodragbutton a source for dnd
        self.wTree.get_widget("dodragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("doscript",0,0)],gtk.gdk.ACTION_COPY)

        #makes whendragbutton a source for dnd
        self.wTree.get_widget("whendragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("whenscript",0,1)],gtk.gdk.ACTION_COPY)

        #makes givendragbutton a source for dnd
        self.wTree.get_widget("givendragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("givenscript",0,2)],gtk.gdk.ACTION_COPY)

        #makes whengivendragbutton a source for dnd
        self.wTree.get_widget("whengivendragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("whengivenscript",0,3)],gtk.gdk.ACTION_COPY)

        #makes mainvbox a destination for dnd of 4 scripts
        self.wTree.get_widget("mainvbox").drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP, [("doscript",0,0),("whenscript",0,1),("givenscript",0,2),("whengivenscript",0,3)], gtk.gdk.ACTION_COPY)

        #sets up drop signals between scripts and mainvbox
        scriptDic={"on_mainvbox_drag_drop": self.on_mainvbox_drag_drop,"on_playbutton_clicked": self.runProgram}
        self.wTree.signal_autoconnect(scriptDic)

        #makes print a source for dnd
        self.wTree.get_widget("printdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("print",0,4)],gtk.gdk.ACTION_COPY)

    def on_mainvbox_drag_drop(self,source,context,x,y,time):
        outerBox=self.wTree.get_widget("mainvbox")
        global innerVBox
        if context.targets==["doscript"]:
            innerBox=gtk.VBox(True,3)
            outerBox.add(innerBox)
            doLabel=gtk.Label(" do: ")
            innerBox.pack_start(doLabel,False,False,0)
            innerVBox=gtk.VBox(True,1)
            innerBox.pack_start(innerVBox,False,False,0)
            endDo=gtk.HBox(True,2)
            endDoLabel=gtk.Label(" end script, return ")
            endDo.pack_start(endDoLabel,False,False,0)
            endDoReturn=gtk.combo_box_entry_new_text()
            endDoReturn.append_text("None")
            endDoReturn.append_text("<variables>")
            endDoReturn.append_text("<parameters>")
            endDoReturn.set_active(0)
            endDo.pack_start(endDoReturn,False,False,0)
            innerBox.pack_start(endDo,False,False,0)
            self.window.show_all()
        elif context.targets==["whenscript"]:
            innerBox=gtk.VBox(True,3)
            outerBox.add(innerBox)
            whenBox=gtk.HBox(True,3)
            whenLabel1=gtk.Label(" when ")
            whenBox.pack_start(whenLabel1)
            whenEvent=gtk.combo_box_entry_new_text()
            whenBox.pack_start(whenEvent)
            whenLabel2=gtk.Label(" , do ")
            whenBox.pack_start(whenLabel2)
            innerBox.pack_start(whenBox)
            innerVBox=gtk.VBox(True,1)
            innerBox.pack_start(innerVBox)
            endWhen=gtk.HBox(True,2)
            endWhenLabel=gtk.Label(" end script, return ")
            endWhen.pack_start(endWhenLabel,False,False,0)
            endWhenReturn=gtk.combo_box_entry_new_text()
            endWhenReturn.append_text("None")
            endWhenReturn.append_text("<variables>")
            endWhenReturn.append_text("<parameters>")
            endWhenReturn.set_active(0)
            endWhen.pack_start(endWhenReturn,False,False,0)
            innerBox.pack_start(endWhen,False,False,0)
            self.window.show_all()
        elif context.targets==["givenscript"]:
            innerBox=gtk.VBox(True,3)
            outerBox.add(innerBox)
            givenBox=gtk.HBox(True,3)
            givenLabel1=gtk.Label(" given ")
            givenBox.pack_start(givenLabel1)
            givenParams=gtk.combo_box_entry_new_text()
            givenBox.pack_start(givenParams)
            givenLabel2=gtk.Label(" , do ")
            givenBox.pack_start(givenLabel2)
            innerBox.pack_start(givenBox)
            innerVBox=gtk.VBox(True,1)
            innerBox.pack_start(innerVBox)
            endGiven=gtk.HBox(True,2)
            endGivenLabel=gtk.Label(" end script, return ")
            endGiven.pack_start(endGivenLabel,False,False,0)
            endGivenReturn=gtk.combo_box_entry_new_text()
            endGivenReturn.append_text("None")
            endGivenReturn.append_text("<variables>")
            endGivenReturn.append_text("<parameters>")
            endGivenReturn.set_active(0)
            endGiven.pack_start(endGivenReturn,False,False,0)
            innerBox.pack_start(endGiven,False,False,0)
            self.window.show_all()
        elif context.targets==["whengivenscript"]:
            innerBox=gtk.VBox(True,4)
            outerBox.add(innerBox)
            whenBox=gtk.HBox(True,3)
            whenLabel1=gtk.Label(" when ")
            whenBox.pack_start(whenLabel1)
            whenEvent=gtk.combo_box_entry_new_text()
            whenBox.pack_start(whenEvent)
            whenLabel2=gtk.Label(" , and ")
            whenBox.pack_start(whenLabel2)
            innerBox.pack_start(whenBox)
            givenBox=gtk.HBox(True,3)
            givenLabel1=gtk.Label(" given ")
            givenBox.pack_start(givenLabel1)
            givenParams=gtk.combo_box_entry_new_text()
            givenBox.pack_start(givenParams)
            givenLabel2=gtk.Label(" , do ")
            givenBox.pack_start(givenLabel2)
            innerBox.pack_start(givenBox)
            innerVBox=gtk.VBox(True,1)
            innerBox.pack_start(innerVBox)
            endWhengiven=gtk.HBox(True,2)
            endWhengivenLabel=gtk.Label(" end script, return ")
            endWhengiven.pack_start(endWhengivenLabel,False,False,0)
            endWhengivenReturn=gtk.combo_box_entry_new_text()
            endWhengivenReturn.append_text("None")
            endWhengivenReturn.append_text("<variables>")
            endWhengivenReturn.append_text("<parameters>")
            endWhengivenReturn.set_active(0)
            endWhengiven.pack_start(endWhengivenReturn,False,False,0)
            innerBox.pack_start(endWhengiven,False,False,0)
            self.window.show_all()
        else:
            pass
        innerVBox.drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP, [("print",0,4)], gtk.gdk.ACTION_COPY)
        innerVBox.connect("drag_drop",self.on_innerVBox_drag_drop)

    def on_innerVBox_drag_drop(self,source,context,x,y,time):
        global printString
        outerVBox2=innerVBox
        innerVBox2=gtk.VBox(True,1)
        outerVBox2.add(innerVBox2)
        printHBox=gtk.HBox(True,2)
        printLabel=gtk.Label(" print ")
        printHBox.pack_start(printLabel,False,False,0)
        printString=gtk.Entry()
        printHBox.pack_start(printString,False,False,0)
        innerVBox2.add(printHBox)
        self.window.show_all()

    def runProgram(self,widget,data=None):
        try:
            if printString:
                print printString.get_text()
            else:
                pass
        except NameError:
            pass
        

    def main(self):
        gtk.main()

if __name__ == "__main__":
    pyj=Pyjama()
    pyj.main()
