#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk
import gtk.glade
#import pango

class Pyjama:

    '''This is the the class for the main window for Pyjama.'''

    def __init__(self):
        '''Finds the file and uses it.'''
        self.gladefile="pyjama.glade"
        self.wTree=gtk.glade.XML(self.gladefile)
        self.window=self.wTree.get_widget("Pyjama")

        #Sets up both closing the window and using the quit menu item to exit.
        exitDic = {"on_Pyjama_destroy": gtk.main_quit, "on_quit_activate": gtk.main_quit}
        self.wTree.signal_autoconnect(exitDic)

        #Switches the interpreter and the code window.
        #Can probably be set up using turning pack start on and off, though it isn't working right now.
        
        #changes background color of graphics window. unfortunately, I can only get it to change to black, which doesn't help.
        self.wTree.get_widget("maindrawingarea").modify_bg(gtk.STATE_NORMAL,gtk.gdk.Color(50,50,50))
        style=gtk.Style()
        style.set_background(self.wTree.get_widget("maindrawingarea").window,gtk.STATE_NORMAL)

        #sets up drop signal over codevbox, running program, adding new script tab
        #add user-defined parameter to self.runProgram to run current tab's program
        scriptDic={"on_codevbox_drag_drop": self.codeDragDrop,"on_playbutton_clicked": self.setUpProgram,"on_dobutton_toggled":self.scriptChanger,"on_whenbutton_toggled":self.scriptChanger,"on_givenbutton_toggled":self.scriptChanger,"on_whengivenbutton_toggled":self.scriptChanger,"on_addother_clicked":self.addOther}
        self.wTree.signal_autoconnect(scriptDic)

        #makes print a source for dnd
        self.wTree.get_widget("printdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("print",0,4)],gtk.gdk.ACTION_COPY)

        #makes forward a source for dnd
        self.wTree.get_widget("forwarddragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("forward",0,5)],gtk.gdk.ACTION_COPY)

        #makes backward a source for dnd
        self.wTree.get_widget("backwarddragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("backward",0,6)],gtk.gdk.ACTION_COPY)

        #makes turn left a source for dnd
        self.wTree.get_widget("leftdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("left",0,7)],gtk.gdk.ACTION_COPY)

        #makes turn right a source for dnd
        self.wTree.get_widget("rightdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("right",0,8)],gtk.gdk.ACTION_COPY)

        #makes stop a source for dnd
        self.wTree.get_widget("stopdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("stop",0,9)],gtk.gdk.ACTION_COPY)

        #makes motors a source for dnd
        self.wTree.get_widget("motorsdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("motors",0,10)],gtk.gdk.ACTION_COPY)

        #makes wait a source for dnd
        self.wTree.get_widget("waitdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("wait",0,11)],gtk.gdk.ACTION_COPY)

        #makes current time a source for dnd
        self.wTree.get_widget("currenttimedragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("currenttime",0,12)],gtk.gdk.ACTION_COPY)

        #makes set timer a source for dnd
        self.wTree.get_widget("timerdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("settimer",0,13)],gtk.gdk.ACTION_COPY)

        #makes time elapsed a source for dnd
        self.wTree.get_widget("timeelapseddragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("elapsed",0,14)],gtk.gdk.ACTION_COPY)

        #makes ask user a source for dnd
        self.wTree.get_widget("askuserdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("askuser",0,15)],gtk.gdk.ACTION_COPY)

        #makes gamepad a source for dnd
        self.wTree.get_widget("gamepaddragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("gamepad",0,16)],gtk.gdk.ACTION_COPY)

        #makes joystick a source for dnd
        self.wTree.get_widget("joystickdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("joystick",0,17)],gtk.gdk.ACTION_COPY)

        #makes take picture a source for dnd
        self.wTree.get_widget("takepicdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("takepic",0,18)],gtk.gdk.ACTION_COPY)

        #makes show picture a source for dnd
        self.wTree.get_widget("showpicdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("showpic",0,19)],gtk.gdk.ACTION_COPY)

        #makes load picture a source for dnd
        self.wTree.get_widget("loadpicdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("loadpic",0,20)],gtk.gdk.ACTION_COPY)

        #makes save picture a source for dnd
        self.wTree.get_widget("savepicdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("savepic",0,21)],gtk.gdk.ACTION_COPY)
        
        #makes copy picture a source for dnd
        self.wTree.get_widget("copypicdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("copypic",0,22)],gtk.gdk.ACTION_COPY)
        
        #makes pixels a source for dnd
        self.wTree.get_widget("pixelsdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("pixels",0,23)],gtk.gdk.ACTION_COPY)
        
        #makes pixel a source for dnd
        self.wTree.get_widget("pixeldragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("pixel",0,24)],gtk.gdk.ACTION_COPY)
        
        #makes play sound a source for dnd
        self.wTree.get_widget("playsounddragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("playsound",0,25)],gtk.gdk.ACTION_COPY)
        
        #makes load sound a source for dnd
        self.wTree.get_widget("loadsounddragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("loadsound",0,26)],gtk.gdk.ACTION_COPY)
        
        #makes beep a source for dnd
        self.wTree.get_widget("beepdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("beep",0,27)],gtk.gdk.ACTION_COPY)
        
        #makes speak a source for dnd
        self.wTree.get_widget("speakdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("speak",0,28)],gtk.gdk.ACTION_COPY)
        
        #makes set voice a source for dnd
        self.wTree.get_widget("setvoicedragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("setvoice",0,29)],gtk.gdk.ACTION_COPY)
        
        #makes beep2 a source for dnd
        self.wTree.get_widget("beep2dragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("beep2",0,30)],gtk.gdk.ACTION_COPY)
        
        #makes change bg color a source for dnd
        self.wTree.get_widget("bgdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("bg",0,33)],gtk.gdk.ACTION_COPY)
        
        #makes draw a source for dnd
        self.wTree.get_widget("drawdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("draw",0,34)],gtk.gdk.ACTION_COPY)
        
        #makes undraw a source for dnd
        self.wTree.get_widget("undrawdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("undraw",0,35)],gtk.gdk.ACTION_COPY)
        
        #makes point a source for dnd
        self.wTree.get_widget("pointdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("point",0,36)],gtk.gdk.ACTION_COPY)
        
        #makes x a source for dnd
        self.wTree.get_widget("xdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("x",0,37)],gtk.gdk.ACTION_COPY)
        
        #makes y a source for dnd
        self.wTree.get_widget("ydragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("y",0,38)],gtk.gdk.ACTION_COPY)
        
        #makes line a source for dnd
        self.wTree.get_widget("linedragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("line",0,39)],gtk.gdk.ACTION_COPY)
        
        #makes circle a source for dnd
        self.wTree.get_widget("circledragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("circle",0,40)],gtk.gdk.ACTION_COPY)
        
        #makes rectangle a source for dnd
        self.wTree.get_widget("rectangledragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("rectangle",0,41)],gtk.gdk.ACTION_COPY)
        
        #makes oval a source for dnd
        self.wTree.get_widget("ovaldragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("oval",0,42)],gtk.gdk.ACTION_COPY)
        
        #makes polygon a source for dnd
        self.wTree.get_widget("polygondragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("polygon",0,43)],gtk.gdk.ACTION_COPY)
        
        #makes text a source for dnd
        self.wTree.get_widget("textdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("text",0,44)],gtk.gdk.ACTION_COPY)
        
        #makes image a source for dnd
        self.wTree.get_widget("imagedragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("image",0,45)],gtk.gdk.ACTION_COPY)
        
        #makes center a source for dnd
        self.wTree.get_widget("centerdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("center",0,46)],gtk.gdk.ACTION_COPY)
        
        #makes outline color a source for dnd
        self.wTree.get_widget("outlinecolordragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("outlinecolor",0,47)],gtk.gdk.ACTION_COPY)
        
        #makes fill color a source for dnd
        self.wTree.get_widget("fillcolordragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("fillcolor",0,48)],gtk.gdk.ACTION_COPY)
        
        #makes outline thickness a source for dnd
        self.wTree.get_widget("outlinethicknessdragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("thickness",0,49)],gtk.gdk.ACTION_COPY)
        
        #makes move by x, y a source for dnd
        self.wTree.get_widget("movexydragbutton").drag_source_set(gtk.gdk.BUTTON1_MASK,[("movexy",0,50)],gtk.gdk.ACTION_COPY)
        
        global listOfDraggables
        listOfDraggables=[("print",0,4),("forward",0,5),("backward",0,6),("left",0,7),("right",0,8),("stop",0,9),("motors",0,10),("wait",0,11),("currenttime",0,12),("settimer",0,13),("elapsed",0,14),("askuser",0,15),("gamepad",0,16),("joystick",0,17),("takepic",0,18),("showpic",0,19),("loadpic",0,20),("savepic",0,21),("copypic",0,22),("pixels",0,23),("pixel",0,24),("playsound",0,25),("loadsound",0,26),("beep",0,27),("speak",0,28),("setvoice",0,29),("beep2",0,30),("bg",0,33),("draw",0,34),("undraw",0,35),("point",0,36),("x",0,37),("y",0,38),("line",0,39),("circle",0,40),("rectangle",0,41),("oval",0,42),("polygon",0,43),("text",0,44),("image",0,45),("center",0,46),("outlinecolor",0,47),("fillcolor",0,48),("thickness",0,49),("movexy",0,50)]

        #makes codevbox a drag destination
        self.wTree.get_widget("codevbox").drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP, listOfDraggables,gtk.gdk.ACTION_COPY)
        #the next line is phrased wrong; fix it later
        self.wTree.get_widget("codevbox").connect("drag_drop",self.codeDragDrop)
        
        #doesn't work yet, but connect usedBlocks to contents of current codevbox, when dragged to trash can icon, delete them
        self.wTree.get_widget("deletebutton").drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP,[("usedblock",0,0)],gtk.gdk.ACTION_MOVE)

    def addOther(self,widget,data=None):
        #pops up a dialog which asks for a name for the script
        addNewScriptDialog=gtk.Dialog("Name your script",self.window,0,(gtk.STOCK_OK,gtk.RESPONSE_ACCEPT,gtk.STOCK_CANCEL,gtk.RESPONSE_CANCEL))
        global scriptNameEntry
        scriptNameEntry=gtk.Entry()
        addNewScriptDialog.action_area.pack_start(scriptNameEntry,False,False,0)
        addNewScriptDialog.connect("response",self.addOtherResponse)
        addNewScriptDialog.show_all()
        addNewScriptDialog.run()

    def addOtherResponse(self,dialog,response_id,data=None):
        newScriptLabel=gtk.Label(scriptNameEntry.get_text())
        dialog.destroy()
        #if a name is provided for the script, this creates the notebook page
        if response_id==gtk.RESPONSE_ACCEPT:
            newScript=gtk.ScrolledWindow()
            newHBox=gtk.HBox()
            newScript.add_with_viewport(newHBox)
            #toolbar.append_element is deprecated as of pygtk2.4--use toolbar.insert(item,pos) instead, using gtk.radiotoolbutton, gtk.toolitem, gtk.button. Since I'm in an old version the new way doesn't work.
            newToolbar=gtk.Toolbar()
            newHBox.pack_start(newToolbar,False,False,0)
            newToolbar.set_orientation(gtk.ORIENTATION_VERTICAL)
            newToolbar.set_style(gtk.TOOLBAR_TEXT)
            global dobutton
            dobutton=newToolbar.append_element(gtk.TOOLBAR_CHILD_RADIOBUTTON,None,"Do",None,None,None,self.scriptChanger,newToolbar)
            global whenbutton
            whenbutton=newToolbar.append_element(gtk.TOOLBAR_CHILD_RADIOBUTTON,dobutton,"When",None,None,None,self.scriptChanger,newToolbar)
            global givenbutton
            givenbutton=newToolbar.append_element(gtk.TOOLBAR_CHILD_RADIOBUTTON,dobutton,"Given",None,None,None,self.scriptChanger,newToolbar)
            whengivenbutton=newToolbar.append_element(gtk.TOOLBAR_CHILD_RADIOBUTTON,dobutton,"When\nGiven",None,None,None,self.scriptChanger,newToolbar)
            dobutton.set_active(True)
            newVBox=gtk.VBox()
            newHBox.pack_start(newVBox) #deliberately left without false,false,0 so it will take the available space completely
            #makes newScriptHBox global so it can be used to change scripts
            global newScriptHBox
            newScriptHBox=gtk.HBox()
            newVBox.pack_start(newScriptHBox,False,False,0)
            newDoLabel=gtk.Label(" do: ")
            newScriptHBox.pack_start(newDoLabel,False,False,0)
            newCodeVBox=gtk.VBox()
            newVBox.pack_start(newCodeVBox) #deliberately left without false,false,0 so it will be big enough to see
            newReturn=gtk.HBox()
            newVBox.pack_start(newReturn,False,False,0)
            newReturnLabel=gtk.Label(" end script, return: ")
            newReturn.pack_start(newReturnLabel,False,False,0)
            newReturnComboBox=gtk.combo_box_entry_new_text()
            newReturnComboBox.append_text("None")
            newReturnComboBox.append_text("<parameters>")
            newReturnComboBox.append_text("<variables>")
            newReturnComboBox.set_active(0)
            newReturn.pack_start(newReturnComboBox,False,False,0)
            self.wTree.get_widget("dinahscriptsnotebook").append_page(newScript,newScriptLabel)
            self.window.show_all()
            newCodeVBox.drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP, listOfDraggables,gtk.gdk.ACTION_COPY)
            newCodeVBox.connect("drag_drop",self.codeDragDrop)
        else:
            pass
            
    def scriptChanger(self,widget,data=None):
        #gets called twice every time the widget changes. should set it to only get called once, or to not do anything if it's already set to the right one
        #also use user-defined parameter to tell what the current page is, use that
        if self.wTree.get_widget("dinahscriptsnotebook").get_current_page()==0:
            scripthbox=self.wTree.get_widget("scripthbox")
            do=self.wTree.get_widget("dobutton")
            when=self.wTree.get_widget("whenbutton")
            given=self.wTree.get_widget("givenbutton")
        else:
            pagenum=self.wTree.get_widget("dinahscriptsnotebook").get_current_page()
            page=self.wTree.get_widget("dinahscriptsnotebook").get_nth_page(pagenum)
            for child in page.get_children():
                #want to find right child widget, use it in scriptChanger (will need to do same thing in dnd to have more than 1 extra script)
                pass
            scripthbox=newScriptHBox
            do=dobutton
            when=whenbutton
            given=givenbutton
        if do.get_active():
            for child in scripthbox.get_children():
                scripthbox.remove(child)
            dolabel=gtk.Label(" do: ")
            scripthbox.add(dolabel)
        elif when.get_active():
            for child in scripthbox.get_children():
                scripthbox.remove(child)
            whenhbox=gtk.HBox()
            scripthbox.add(whenhbox)
            whenlabel1=gtk.Label(" when: ")
            whenhbox.pack_start(whenlabel1,False,False,0)
            whencombobox=gtk.combo_box_new_text()
            whencomboboxlist=["play","space","up arrow","down arrow","left arrow","right arrow","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","<sprite>"]
            for item in whencomboboxlist:
                whencombobox.append_text(item)
            whenhbox.pack_start(whencombobox,False,False,0)
            whenlabel2=gtk.Label(" is clicked/pressed, do: ")
            whenhbox.pack_start(whenlabel2,False,False,0)
        elif given.get_active():
            for child in scripthbox.get_children():
                scripthbox.remove(child)
            givenhbox=gtk.HBox()
            scripthbox.add(givenhbox)
            givenlabel1=gtk.Label(" given: ")
            givenhbox.pack_start(givenlabel1,False,False,0)
            givencombobox=gtk.combo_box_entry_new_text()
            givencombobox.append_text("<parameter>")
            givenhbox.pack_start(givencombobox,False,False,0)
            givenlabel2=gtk.Label(" , do: ")
            givenhbox.pack_start(givenlabel2,False,False,0)
        else:
            for child in scripthbox.get_children():
                scripthbox.remove(child)
            whengivenhbox=gtk.HBox()
            scripthbox.add(whengivenhbox)
            whengivenlabel1=gtk.Label(" when: ")
            whengivenhbox.pack_start(whengivenlabel1,False,False,0)
            whengivencombobox1=gtk.combo_box_new_text()
            whengivencombobox1list=["play","space","up arrow","down arrow","left arrow","right arrow","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","<sprite>"]
            for item in whengivencombobox1list:
                whengivencombobox1.append_text(item)
            whengivenhbox.pack_start(whengivencombobox1,False,False,0)
            whengivenlabel2=gtk.Label(" is clicked/pressed, and given: ")
            whengivenhbox.pack_start(whengivenlabel2,False,False,0)
            whengivencombobox2=gtk.combo_box_entry_new_text()
            whengivencombobox2.append_text("<parameter>")
            whengivenhbox.pack_start(whengivencombobox2,False,False,0)
            whengivenlabel3=gtk.Label(" , do: ")
            whengivenhbox.pack_start(whengivenlabel3,False,False,0)
        self.window.show_all()

    def codeDragDrop(self,source,context,x,y,time):
        codevbox=source
        innerVBox=gtk.VBox()
        codevbox.pack_start(innerVBox,False,False,0)
        frame=gtk.Frame()
        innerVBox.add(frame)
        blockHBox=gtk.HBox()
        frame.add(blockHBox)
        if context.targets==["print"]:
            printLabel=gtk.Label(" print ")
            blockHBox.pack_start(printLabel,False,False,0)
            printString=gtk.Entry()
            blockHBox.pack_start(printString,False,False,0)
            innerVBox.set_name("print")
        elif context.targets==["forward"]:
            forwardLabel1=gtk.Label(" forward at speed ")
            blockHBox.pack_start(forwardLabel1,False,False,0)
            forwardSpin1=gtk.SpinButton(gtk.Adjustment(1,-1,1,.1,.5,0),0.0,1)
            blockHBox.pack_start(forwardSpin1,False,False,0)
            forwardLabel2=gtk.Label(" for ")
            blockHBox.pack_start(forwardLabel2,False,False,0)
            forwardSpin2=gtk.SpinButton(gtk.Adjustment(1,0,10,.1,.5,0),0.0,1)
            blockHBox.pack_start(forwardSpin2,False,False,0)
            forwardLabel3=gtk.Label(" seconds ")
            blockHBox.pack_start(forwardLabel3,False,False,0)
        elif context.targets==["backward"]:
            backwardLabel1=gtk.Label(" backward at speed ")
            blockHBox.pack_start(backwardLabel1,False,False,0)
            backwardSpin1=gtk.SpinButton(gtk.Adjustment(1,-1,1,.1,.5,0),0.0,1)
            blockHBox.pack_start(backwardSpin1,False,False,0)
            backwardLabel2=gtk.Label(" for ")
            blockHBox.pack_start(backwardLabel2,False,False,0)
            backwardSpin2=gtk.SpinButton(gtk.Adjustment(1,0,10,.1,.5,0),0.0,1)
            blockHBox.pack_start(backwardSpin2,False,False,0)
            backwardLabel3=gtk.Label(" seconds ")
            blockHBox.pack_start(backwardLabel3,False,False,0)
        elif context.targets==["left"]:
            leftLabel1=gtk.Label(" turn left at speed ")
            blockHBox.pack_start(leftLabel1,False,False,0)
            leftSpin1=gtk.SpinButton(gtk.Adjustment(1,-1,1,.1,.5,0),0.0,1)
            blockHBox.pack_start(leftSpin1,False,False,0)
            leftLabel2=gtk.Label(" for ")
            blockHBox.pack_start(leftLabel2,False,False,0)
            leftSpin2=gtk.SpinButton(gtk.Adjustment(1,0,10,.1,.5,0),0.0,1)
            blockHBox.pack_start(leftSpin2,False,False,0)
            leftLabel3=gtk.Label(" seconds ")
            blockHBox.pack_start(leftLabel3,False,False,0)
        elif context.targets==["right"]:
            rightLabel1=gtk.Label(" turn right at speed ")
            blockHBox.pack_start(rightLabel1,False,False,0)
            rightSpin1=gtk.SpinButton(gtk.Adjustment(1,-1,1,.1,.5,0),0.0,1)
            blockHBox.pack_start(rightSpin1,False,False,0)
            rightLabel2=gtk.Label(" for ")
            blockHBox.pack_start(rightLabel2,False,False,0)
            rightSpin2=gtk.SpinButton(gtk.Adjustment(1,0,10,.1,.5,0),0.0,1)
            blockHBox.pack_start(rightSpin2,False,False,0)
            rightLabel3=gtk.Label(" seconds ")
            blockHBox.pack_start(rightLabel3,False,False,0)
        elif context.targets==["stop"]:
            stopLabel=gtk.Label(" stop ")
            blockHBox.pack_start(stopLabel,False,False,0)
        elif context.targets==["motors"]:
            motorsLabel1=gtk.Label(" left motor at speed ")
            blockHBox.pack_start(motorsLabel1,False,False,0)
            motorsSpin1=gtk.SpinButton(gtk.Adjustment(1,-1,1,.1,.5,0),0.0,1)
            blockHBox.pack_start(motorsSpin1,False,False,0)
            motorsLabel2=gtk.Label(" and right motor at speed ")
            blockHBox.pack_start(motorsLabel2,False,False,0)
            motorsSpin2=gtk.SpinButton(gtk.Adjustment(1,-1,1,.1,.5,0),0.0,1)
            blockHBox.pack_start(motorsSpin2,False,False,0)
        elif context.targets==["wait"]:
            waitLabel1=gtk.Label(" wait ")
            blockHBox.pack_start(waitLabel1,False,False,0)
            waitSpin=gtk.SpinButton(gtk.Adjustment(1,0,10,.1,.5,0),0.0,1)
            blockHBox.pack_start(waitSpin,False,False,0)
            waitLabel2=gtk.Label(" seconds ")
            blockHBox.pack_start(waitLabel2,False,False,0)
        elif context.targets==["currenttime"]:
            currenttimeLabel=gtk.Label(" current time ")
            blockHBox.pack_start(currenttimeLabel,False,False,0)
        elif context.targets==["settimer"]:
            settimerComboBox=gtk.combo_box_new_text()
            settimerComboBox.append_text("start")
            settimerComboBox.append_text("pause")
            settimerComboBox.append_text("stop")
            settimerComboBox.append_text("reset")
            blockHBox.pack_start(settimerComboBox,False,False,0)
            settimerLabel=gtk.Label(" timer ")
            blockHBox.pack_start(settimerLabel,False,False,0)
        elif context.targets==["elapsed"]:
            elapsedLabel=gtk.Label(" time elapsed ")
            blockHBox.pack_start(elapsedLabel,False,False,0)
        elif context.targets==["askuser"]:
            askuserLabel=gtk.Label(" ask user for a ")
            blockHBox.pack_start(askuserLabel,False,False,0)
            askuserComboBox=gtk.combo_box_entry_new_text()
            askuserComboBox.append_text("number")
            askuserComboBox.append_text("string")
            askuserComboBox.append_text("yes/no")
            blockHBox.pack_start(askuserComboBox,False,False,0)
        elif context.targets==["gamepad"]:
            gamepadLabel=gtk.Label(" use gamepad ")
            blockHBox.pack_start(gamepadLabel,False,False,0)
        elif context.targets==["joystick"]:
            joystickLabel=gtk.Label(" use joystick ")
            blockHBox.pack_start(joystickLabel,False,False,0)
        elif context.targets==["takepic"]:
            takepicLabel1=gtk.Label(" take ")
            blockHBox.pack_start(takepicLabel1,False,False,0)
            takepicComboBox=gtk.combo_box_new_text()
            takepicComboBox.append_text("color")
            takepicComboBox.append_text("grayscale")
            takepicComboBox.append_text("blob")
            blockHBox.pack_start(takepicComboBox,False,False,0)
            takepicLabel2=gtk.Label(" picture ")
            blockHBox.pack_start(takepicLabel2,False,False,0)
        elif context.targets==["showpic"]:
            showpicLabel=gtk.Label(" show ")
            blockHBox.pack_start(showpicLabel,False,False,0)
            showpicComboBox=gtk.combo_box_new_text()
            showpicComboBox.append_text("<picture>")
            blockHBox.pack_start(showpicComboBox,False,False,0)
        elif context.targets==["loadpic"]:
            loadpicLabel=gtk.Label(" load ")
            blockHBox.pack_start(loadpicLabel,False,False,0)
            loadpicFileChooser=gtk.FileChooserButton("Choose a Picture File")
            blockHBox.pack_start(loadpicFileChooser,False,False,0)
        elif context.targets==["savepic"]:
            savepicLabel1=gtk.Label(" save ")
            blockHBox.pack_start(savepicLabel1,False,False,0)
            savepicComboBox=gtk.combo_box_new_text()
            savepicComboBox.append_text("<picture>")
            blockHBox.pack_start(savepicComboBox,False,False,0)
            savepicLabel2=gtk.Label(" to ")
            blockHBox.pack_start(savepicLabel2)
            savepicFileChooser=gtk.FileChooserButton("Choose a File")
            blockHBox.pack_start(savepicFileChooser,False,False,0)
        elif context.targets==["copypic"]:
            copypicLabel=gtk.Label(" copy ")
            blockHBox.pack_start(copypicLabel,False,False,0)
            copypicComboBox=gtk.combo_box_new_text()
            copypicComboBox.append_text("<picture>")
            blockHBox.pack_start(copypicComboBox,False,False,0)
        elif context.targets==["pixels"]:
            pixelsLabel=gtk.Label(" pixels of ")
            blockHBox.pack_start(pixelsLabel,False,False,0)
            pixelsComboBox=gtk.combo_box_new_text()
            pixelsComboBox.append_text("<picture>")
            blockHBox.pack_start(pixelsComboBox,False,False,0)
        elif context.targets==["pixel"]:
            pixelLabel1=gtk.Label(" pixel of ")
            blockHBox.pack_start(pixelLabel1,False,False,0)
            pixelComboBox=gtk.combo_box_new_text()
            pixelComboBox.append_text("<picture>")
            blockHBox.pack_start(pixelComboBox,False,False,0)
            pixelLabel2=gtk.Label(" at position ")
            blockHBox.pack_start(pixelLabel2,False,False,0)
            pixelSpin1=gtk.SpinButton(gtk.Adjustment(100, 0, 256, 1, 5, 0),0.0,0)
            blockHBox.pack_start(pixelSpin1,False,False,0)
            pixelLabel3=gtk.Label(" , ")
            blockHBox.pack_start(pixelLabel3,False,False,0)
            pixelSpin2=gtk.SpinButton(gtk.Adjustment(100,0,192,1,5,0),0.0,0)
            blockHBox.pack_start(pixelSpin2,False,False,0)
        elif context.targets==["playsound"]:
            playsoundLabel=gtk.Label(" play ")
            blockHBox.pack_start(playsoundLabel,False,False,0)
            playsoundComboBox=gtk.combo_box_new_text()
            playsoundComboBox.append_text("<sound>")
            blockHBox.pack_start(playsoundComboBox,False,False,0)
        elif context.targets==["loadsound"]:
            loadsoundLabel=gtk.Label(" load ")
            blockHBox.pack_start(loadsoundLabel,False,False,0)
            loadsoundFileChooser=gtk.FileChooserButton("Choose a Sound File (.wav)")
            blockHBox.pack_start(loadsoundFileChooser,False,False,0)
        elif context.targets==["beep"]:
            beepLabel1=gtk.Label(" beep at frequency ")
            blockHBox.pack_start(beepLabel1,False,False,0)
            beepSpin1=gtk.SpinButton(gtk.Adjustment(440,25,4200,10,50,0),0.0,2)
            blockHBox.pack_start(beepSpin1,False,False,0)
            beepLabel2=gtk.Label(" for ")
            blockHBox.pack_start(beepLabel2,False,False,0)
            beepSpin2=gtk.SpinButton(gtk.Adjustment(1,0,10,1,2,0),0.0,1)
            blockHBox.pack_start(beepSpin2,False,False,0)
            beepLabel3=gtk.Label(" seconds ")
            blockHBox.pack_start(beepLabel3,False,False,0)
        elif context.targets==["speak"]:
            speakLabel1=gtk.Label(" say ")
            blockHBox.pack_start(speakLabel1,False,False,0)
            speakTextEntry=gtk.Entry()
            speakTextEntry.set_text("Hello World")
            blockHBox.pack_start(speakTextEntry,False,False,0)
        elif context.targets==["setvoice"]:
            setvoiceLabel=gtk.Label(" set voice to ")
            blockHBox.pack_start(setvoiceLabel,False,False,0)
            setvoiceComboBox=gtk.combo_box_new_text()
            setvoiceComboBox.append_text("<voice>")
            blockHBox.pack_start(setvoiceComboBox,False,False,0)
        elif context.targets==["beep2"]:
            beep2Label1=gtk.Label(" beep at frequency ")
            blockHBox.pack_start(beep2Label1,False,False,0)
            beep2Spin1=gtk.SpinButton(gtk.Adjustment(440,25,4200,10,50,0),0.0,2)
            blockHBox.pack_start(beep2Spin1,False,False,0)
            beep2Label2=gtk.Label(" and frequency ")
            blockHBox.pack_start(beep2Label2,False,False,0)
            beep2Spin2=gtk.SpinButton(gtk.Adjustment(440,25,4200,10,50,0),0.0,2)
            blockHBox.pack_start(beep2Spin2,False,False,0)
            beep2Label3=gtk.Label(" for ")
            blockHBox.pack_start(beep2Label3,False,False,0)
            beep2Spin3=gtk.SpinButton(gtk.Adjustment(1,0,10,.1,.5,0),0.0,1)
            blockHBox.pack_start(beep2Spin3,False,False,0)
            beep2Label4=gtk.Label(" seconds ")
            blockHBox.pack_start(beep2Label4,False,False,0)
        elif context.targets==["bg"]:
            bgLabel=gtk.Label(" change background color to ")
            blockHBox.pack_start(bgLabel,False,False,0)
            bgColorChooser=gtk.ColorButton()
            blockHBox.pack_start(bgColorChooser,False,False,0)
        elif context.targets==["draw"]:
            drawLabel1=gtk.Label(" draw ")
            blockHBox.pack_start(drawLabel1,False,False,0)
            drawVBox1=gtk.VBox()
            drawComboBox1=gtk.combo_box_new_text()
            drawComboBox1.append_text("<image>")
            drawComboBox1.set_name("combo1")
            drawVBox1.pack_start(drawComboBox1,False,False,0)
            blockHBox.pack_start(drawVBox1,False,False,0)
            drawVBox1.drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP,[("point",0,36),("line",0,39),("circle",0,40),("rectangle",0,41),("oval",0,42),("polygon",0,43),("text",0,44),("image",0,45)],gtk.gdk.ACTION_COPY)
            drawVBox1.connect("drag_drop",self.removeAndDrop)
            innerVBox.set_name("draw")
        elif context.targets==["undraw"]:
            undrawLabel=gtk.Label(" undraw ")
            blockHBox.pack_start(undrawLabel,False,False,0)
            undrawComboBox=gtk.combo_box_new_text()
            undrawComboBox.append_text("<image>")
            blockHBox.pack_start(undrawComboBox,False,False,0)
        elif context.targets==["point"]:
            pointLabel1=gtk.Label(" point at ")
            blockHBox.pack_start(pointLabel1,False,False,0)
            pointSpin1=gtk.SpinButton(gtk.Adjustment(100,0,400,1,10),0.0,0)
            blockHBox.pack_start(pointSpin1,False,False,0)
            pointLabel2=gtk.Label(" , ")
            blockHBox.pack_start(pointLabel2,False,False,0)
            pointSpin2=gtk.SpinButton(gtk.Adjustment(100,0,400,1,10),0.0,0)
            blockHBox.pack_start(pointSpin2,False,False,0)
            innerVBox.set_name("point")
        elif context.targets==["x"]:
            xLabel=gtk.Label(" x coordinate ")
            blockHBox.pack_start(xLabel,False,False,0)
        elif context.targets==["y"]:
            yLabel=gtk.Label(" y coordinate ")
            blockHBox.pack_start(yLabel,False,False,0)
        elif context.targets==["line"]:
            #this is how a block can be set up to allow other blocks to be dragged onto it as parameters. control statements will work differently--they will come with internal VBoxes to put code into. ideally, in the case of blocks-as-parameters, when an object is created that can serve as a parameter, it will be added to the combobox's list.
            #alternatively, instead of using the extra VBox, you can use blockHBox.child_get_property(child,"position") in removeAndDrop (see removeAndDrop for details)
            lineLabel1=gtk.Label(" line from ")
            blockHBox.pack_start(lineLabel1,False,False,0)
            lineVBox1=gtk.VBox()
            blockHBox.pack_start(lineVBox1,False,False,0)
            lineComboBox1=gtk.combo_box_new_text()
            lineComboBox1.append_text("<point>")
            lineVBox1.drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP,[("point",0,36)],gtk.gdk.ACTION_COPY)
            lineVBox1.connect("drag_drop",self.removeAndDrop)
            lineVBox1.pack_start(lineComboBox1,False,False,0)
            lineLabel2=gtk.Label(" to ")
            blockHBox.pack_start(lineLabel2,False,False,0)
            lineVBox2=gtk.VBox()
            blockHBox.pack_start(lineVBox2,False,False,0)
            lineComboBox2=gtk.combo_box_new_text()
            lineComboBox2.append_text("<point>")
            lineVBox2.pack_start(lineComboBox2,False,False,0)
            lineVBox2.drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP,[("point",0,36)],gtk.gdk.ACTION_COPY)
            lineVBox2.connect("drag_drop",self.removeAndDrop)
            innerVBox.set_name("line")
        elif context.targets==["circle"]:
            circleLabel1=gtk.Label(" circle with center at ")
            blockHBox.pack_start(circleLabel1,False,False,0)
            circleVBox=gtk.VBox()
            circleComboBox=gtk.combo_box_new_text()
            circleComboBox.append_text("<point>")
            circleVBox.pack_start(circleComboBox,False,False,0)
            blockHBox.pack_start(circleVBox,False,False,0)
            circleVBox.drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP,[("point",0,36)],gtk.gdk.ACTION_COPY)
            circleVBox.connect("drag_drop",self.removeAndDrop)
            circleLabel2=gtk.Label(" and radius ")
            blockHBox.pack_start(circleLabel2,False,False,0)
            circleSpin=gtk.SpinButton(gtk.Adjustment(5,1,200,5,20),0.0,0)
            blockHBox.pack_start(circleSpin,False,False,0)
        elif context.targets==["rectangle"]:
            rectangleLabel1=gtk.Label(" rectangle with opposite corners at ")
            blockHBox.pack_start(rectangleLabel1,False,False,0)
            rectangleVBox1=gtk.VBox()
            rectangleComboBox1=gtk.combo_box_new_text()
            rectangleComboBox1.append_text("<point>")
            rectangleVBox1.pack_start(rectangleComboBox1,False,False,0)
            blockHBox.pack_start(rectangleVBox1,False,False,0)
            rectangleVBox1.drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP,[("point",0,36)],gtk.gdk.ACTION_COPY)
            rectangleVBox1.connect("drag_drop",self.removeAndDrop)
            rectangleLabel1=gtk.Label(" and ")
            blockHBox.pack_start(rectangleLabel1,False,False,0)
            rectangleVBox2=gtk.VBox()
            rectangleComboBox2=gtk.combo_box_new_text()
            rectangleComboBox2.append_text("<point>")
            rectangleVBox2.pack_start(rectangleComboBox2,False,False,0)
            blockHBox.pack_start(rectangleVBox2,False,False,0)
            rectangleVBox2.drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP,[("point",0,36)],gtk.gdk.ACTION_COPY)
            rectangleVBox2.connect("drag_drop",self.removeAndDrop)
        elif context.targets==["oval"]:
            ovalLabel=gtk.Label(" oval bounded by ")
            blockHBox.pack_start(ovalLabel,False,False,0)
            ovalVBox=gtk.VBox()
            ovalComboBox=gtk.combo_box_new_text()
            ovalComboBox.append_text("<rectangle>")
            ovalVBox.pack_start(ovalComboBox,False,False,0)
            blockHBox.pack_start(ovalVBox,False,False,0)
            ovalVBox.drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP,[("rectangle",0,41)],gtk.gdk.ACTION_COPY)
            ovalVBox.connect("drag_drop",self.removeAndDrop)
        elif context.targets==["polygon"]:
            polygonLabel1=gtk.Label(" polygon with ")
            blockHBox.pack_start(polygonLabel1,False,False,0)
            polygonSpin=gtk.SpinButton(gtk.Adjustment(3,3,50,1,5),0.0,0)
            blockHBox.pack_start(polygonSpin,False,False,0)
            polygonLabel2=gtk.Label(" points: ")
            blockHBox.pack_start(polygonLabel2,False,False,0)
            #when the number of points is changed, the number of combo boxes should change to reflect that. it should also set up dnd of points onto each combo box
            polygonComboBox=gtk.combo_box_new_text()
            polygonComboBox.append_text("<points>")
            blockHBox.pack_start(polygonComboBox,False,False,0)
        elif context.targets==["text"]:
            text=gtk.Entry()
            text.set_text("text")
            blockHBox.pack_start(text,False,False,0)
            textLabel=gtk.Label(" at ")
            blockHBox.pack_start(textLabel,False,False,0)
            textVBox=gtk.VBox()
            textComboBox=gtk.combo_box_new_text()
            textComboBox.append_text("<point>")
            textVBox.pack_start(textComboBox,False,False,0)
            blockHBox.pack_start(textVBox,False,False,0)
            textVBox.drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP,[("point",0,36)],gtk.gdk.ACTION_COPY)
            textVBox.connect("drag_drop",self.removeAndDrop)
            innerVBox.set_name("text")
        elif context.targets==["image"]:
            imageLabel1=gtk.Label(" image centered at ")
            blockHBox.pack_start(imageLabel1,False,False,0)
            imageVBox=gtk.VBox()
            imageComboBox=gtk.combo_box_new_text()
            imageComboBox.append_text("<point>")
            imageVBox.pack_start(imageComboBox,False,False,0)
            blockHBox.pack_start(imageVBox,False,False,0)
            imageVBox.drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP,[("point",0,36)],gtk.gdk.ACTION_COPY)
            imageVBox.connect("drag_drop",self.removeAndDrop)
            imageLabel2=gtk.Label(" from file ")
            blockHBox.pack_start(imageLabel2,False,False,0)
            imageFileChooser=gtk.FileChooserButton("Choose an Image File")
            blockHBox.pack_start(imageFileChooser,False,False,0)
        elif context.targets==["center"]:
            centerLabel=gtk.Label(" center point ")
            blockHBox.pack_start(centerLabel,False,False,0)
        elif context.targets==["outlinecolor"]:
            outlineColorLabel=gtk.Label(" set outline color to ")
            blockHBox.pack_start(outlineColorLabel,False,False,0)
            outlineColorChooser=gtk.ColorButton()
            blockHBox.pack_start(outlineColorChooser,False,False,0)
        elif context.targets==["fillcolor"]:
            fillColorLabel=gtk.Label(" set fill color to ")
            blockHBox.pack_start(fillColorLabel,False,False,0)
            fillColorChooser=gtk.ColorButton()
            blockHBox.pack_start(fillColorChooser,False,False,0)
        elif context.targets==["thickness"]:
            thicknessLabel=gtk.Label("set outline thickness to ")
            blockHBox.pack_start(thicknessLabel,False,False,0)
            thicknessSpin=gtk.SpinButton(gtk.Adjustment(1,1,10,1,2),0.0,0)
            blockHBox.pack_start(thicknessSpin,False,False,0)
        elif context.targets==["movexy"]:
            movexyLabel1=gtk.Label(" move ")
            blockHBox.pack_start(movexyLabel1,False,False,0)
            movexySpin1=gtk.SpinButton(gtk.Adjustment(5,-400,400,5,10),0.0,0)
            blockHBox.pack_start(movexySpin1,False,False,0)
            movexyLabel2=gtk.Label(" units across and ")
            blockHBox.pack_start(movexyLabel2,False,False,0)
            movexySpin2=gtk.SpinButton(gtk.Adjustment(5,-400,400,5,10),0.0,0)
            blockHBox.pack_start(movexySpin2,False,False,0)
            movexyLabel3=gtk.Label(" units down ")
            blockHBox.pack_start(movexyLabel3,False,False,0)
        else:
            pass
        #needs to be a button for this to work, I think. when it works, it will allow blocks to be dragged to the trash
        #innerVBox.drag_source_set(gtk.gdk.BUTTON1_MASK,[("usedblock",0,0)],gtk.gdk.ACTION_MOVE)
        self.window.show_all()
        
    def removeAndDrop(self,source,context,x,y,time):
        #alternatively, if you haven't put the relevant widget in an extra VBox, and are using the combobox widget as the drag_dest, you can get the index of the combobox with source.get_parent().child_get_property(source,"position"), then source.destroy(), then add the new contents to the end, then reorder it with the index gotten from child_get_property, as follows:
        #index=source.get_parent().child_get_property(source,"position")
        #parent=source.get_parent()
        #source.destroy()
        #parent.pack_start(newthingtogohere,False,False,0)
        #parent.reorder_child(newthingtogohere,index)
        for child in source.get_children():
            source.remove(child)
        self.codeDragDrop(source,context,x,y,time)
        
    def runProgram(self,widget,data=None):
        #get current page's code vbox. read first element, execute. read next element, execute. (use eval() or exec()).
        #add user-defined parameter to do this
        for child in data.get_children():
            if child.get_name()=="print":
                textBox=child.get_children()[0].get_child().get_children()[-1]
                print textBox.get_text()
            elif child.get_name()=="draw":
                drawingArea=self.wTree.get_widget("maindrawingarea").window
                image=child.get_children()[0].get_child().get_children()[1].get_children()[0]
                if image.get_name()=="combo1":
                    drawingArea.draw_point(gtk.gdk.GC(drawingArea),50,50)
                elif image.get_name()=="point":
                    xcoord=image.get_children()[0].get_child().get_children()[1].get_value_as_int()
                    ycoord=image.get_children()[0].get_child().get_children()[3].get_value_as_int()
                    drawingArea.draw_point(gtk.gdk.GC(drawingArea),xcoord,ycoord)
                #the next section crashes the entire program when run. no idea why, though i sent an e-mail out to the pygtk mailing list to ask why.
                elif image.get_name()=="text":
                    text=image.get_children()[0].get_child().get_children()[0]
                    pangoText=text.get_parent().create_pango_layout(text.get_text())
                    point=image.get_children()[0].get_child().get_children()[2]
                    xcoord=point.get_children()[0].get_children()[0].get_children()[0].get_children()[1].get_value_as_int()
                    ycoord=point.get_children()[0].get_children()[0].get_children()[0].get_children()[3].get_value_as_int()
                    drawingArea.draw_layout(gtk.gdk.GC(drawingArea),xcoord,ycoord,pangoText)
                elif image.get_name()=="line":
                    point1xcoord=image.get_children()[0].get_children()[0].get_children()[1].get_children()[0].get_children()[0].get_children()[0].get_children()[1].get_value_as_int()
                    point1ycoord=image.get_children()[0].get_children()[0].get_children()[1].get_children()[0].get_children()[0].get_children()[0].get_children()[3].get_value_as_int()
                    point2xcoord=image.get_children()[0].get_children()[0].get_children()[3].get_children()[0].get_children()[0].get_children()[0].get_children()[1].get_value_as_int()
                    point2ycoord=image.get_children()[0].get_children()[0].get_children()[3].get_children()[0].get_children()[0].get_children()[0].get_children()[3].get_value_as_int()
                    drawingArea.draw_line(gtk.gdk.GC(drawingArea),point1xcoord,point1ycoord,point2xcoord,point2ycoord)
                else:
                    #will add other categories later--line, circle, rectangle, oval, polygon, text, image
                    pass
            else:
                pass
        
    def setUpProgram(self,widget,data=None):
        #get current page's codevbox, run program from this vbox
        if self.wTree.get_widget("dinahscriptsnotebook").get_current_page()==0:
            codevbox=self.wTree.get_widget("codevbox")
        else:
            #get to correct vbox in other tab, set codevbox equal to that vbox
            codevbox=None
        self.runProgram(widget,codevbox)

    def main(self):
        gtk.main()

if __name__ == "__main__":
    pyj=Pyjama()
    pyj.main()
