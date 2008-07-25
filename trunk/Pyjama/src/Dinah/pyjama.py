#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk
import gtk.glade

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


        #sets up drop signal over codevbox, running program, adding new script tab
        scriptDic={"on_codevbox_drag_drop": self.codeDragDrop,"on_playbutton_clicked": self.runProgram,"on_dobutton_toggled":self.scriptChanger,"on_whenbutton_toggled":self.scriptChanger,"on_givenbutton_toggled":self.scriptChanger,"on_whengivenbutton_toggled":self.scriptChanger,"on_addother_clicked":self.addOther}
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

        #makes codevbox a drag destination
        self.wTree.get_widget("codevbox").drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP, [("print",0,4),("forward",0,5),("backward",0,6),("left",0,7),("right",0,8),("stop",0,9),("motors",0,10),("wait",0,11),("currenttime",0,12),("settimer",0,13),("elapsed",0,14),("askuser",0,15),("gamepad",0,16),("joystick",0,17),("takepic",0,18),("showpic",0,19),("loadpic",0,20),("savepic",0,21),("copypic",0,22),("pixels",0,23),("pixel",0,24),("playsound",0,25),("loadsound",0,26),("beep",0,27),("speak",0,28),("setvoice",0,29),("beep2",0,30)],gtk.gdk.ACTION_COPY)
        #the next line is phrased wrong; fix it later
        self.wTree.get_widget("codevbox").connect("drag_drop",self.codeDragDrop)

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
            #toolbar.append_element is deprecated as of pygtk2.4--use toolbar.insert(item,pos) instead, using gtk.radiotoolbutton, gtk.toolitem, gtk.button. since i'm in an old version the new way doesn't work.
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
            newHBox.pack_start(newVBox,False,False,0)
            #makes newScriptHBox global so it can be used to change scripts
            global newScriptHBox
            newScriptHBox=gtk.HBox()
            newVBox.pack_start(newScriptHBox,False,False,0)
            newDoLabel=gtk.Label(" do: ")
            newScriptHBox.pack_start(newDoLabel,False,False,0)
            newCodeVBox=gtk.VBox()
            newVBox.pack_start(newCodeVBox,False,False,0)
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
            newCodeVBox.drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP, [("print",0,4),("forward",0,5),("backward",0,6),("left",0,7),("right",0,8),("stop",0,9),("motors",0,10),("wait",0,11),("currenttime",0,12),("settimer",0,13),("elapsed",0,14),("askuser",0,15),("gamepad",0,16),("joystick",0,17),("takepic",0,18),("showpic",0,19),("loadpic",0,20),("savepic",0,21),("copypic",0,22),("pixels",0,23),("pixel",0,24),("playsound",0,25),("loadsound",0,26),("beep",0,27),("speak",0,28),("setvoice",0,29),("beep2",0,30)],gtk.gdk.ACTION_COPY)
            newCodeVBox.connect("drag_drop",self.codeDragDrop)
        else:
            pass
            
    def scriptChanger(self,widget,data=None):
        #gets called twice every time the widget changes. should set it to only get called once, or to not do anything if it's already set to the right one
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
            givenhbox.pack_start(givenlabel1)
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
            whengivenhbox.pack_start(whengivencombobox1)
            whengivenlabel2=gtk.Label(" is clicked/pressed, and given: ")
            whengivenhbox.pack_start(whengivenlabel2)
            whengivencombobox2=gtk.combo_box_entry_new_text()
            whengivencombobox2.append_text("<parameter>")
            whengivenhbox.pack_start(whengivencombobox2,False,False,0)
            whengivenlabel3=gtk.Label(" , do: ")
            whengivenhbox.pack_start(whengivenlabel3,False,False,0)
        self.window.show_all()

    def codeDragDrop(self,source,context,x,y,time):
        codevbox=source
        innerVBox=gtk.VBox()
        codevbox.add(innerVBox)
        if context.targets==["print"]:
            global printString
            printHBox=gtk.HBox()
            printLabel=gtk.Label(" print ")
            printHBox.pack_start(printLabel,False,False,0)
            printString=gtk.Entry()
            printHBox.pack_start(printString,False,False,0)
            innerVBox.add(printHBox)
        elif context.targets==["forward"]:
            forwardHBox=gtk.HBox()
            forwardLabel1=gtk.Label(" forward at speed ")
            forwardHBox.pack_start(forwardLabel1,False,False,0)
            forwardSpin1=gtk.SpinButton(gtk.Adjustment(1,-1,1,.1,.5,0),0.0,1)
            forwardHBox.pack_start(forwardSpin1,False,False,0)
            forwardLabel2=gtk.Label(" for ")
            forwardHBox.pack_start(forwardLabel2,False,False,0)
            forwardSpin2=gtk.SpinButton(gtk.Adjustment(1,0,10,.1,.5,0),0.0,1)
            forwardHBox.pack_start(forwardSpin2,False,False,0)
            forwardLabel3=gtk.Label(" seconds ")
            forwardHBox.pack_start(forwardLabel3,False,False,0)
            innerVBox.add(forwardHBox)
        elif context.targets==["backward"]:
            backwardHBox=gtk.HBox()
            backwardLabel1=gtk.Label(" backward at speed ")
            backwardHBox.pack_start(backwardLabel1,False,False,0)
            backwardSpin1=gtk.SpinButton(gtk.Adjustment(1,-1,1,.1,.5,0),0.0,1)
            backwardHBox.pack_start(backwardSpin1,False,False,0)
            backwardLabel2=gtk.Label(" for ")
            backwardHBox.pack_start(backwardLabel2,False,False,0)
            backwardSpin2=gtk.SpinButton(gtk.Adjustment(1,0,10,.1,.5,0),0.0,1)
            backwardHBox.pack_start(backwardSpin2,False,False,0)
            backwardLabel3=gtk.Label(" seconds ")
            backwardHBox.pack_start(backwardLabel3,False,False,0)
            innerVBox.add(backwardHBox)
        elif context.targets==["left"]:
            leftHBox=gtk.HBox()
            leftLabel1=gtk.Label(" turn left at speed ")
            leftHBox.pack_start(leftLabel1,False,False,0)
            leftSpin1=gtk.SpinButton(gtk.Adjustment(1,-1,1,.1,.5,0),0.0,1)
            leftHBox.pack_start(leftSpin1,False,False,0)
            leftLabel2=gtk.Label(" for ")
            leftHBox.pack_start(leftLabel2,False,False,0)
            leftSpin2=gtk.SpinButton(gtk.Adjustment(1,0,10,.1,.5,0),0.0,1)
            leftHBox.pack_start(leftSpin2,False,False,0)
            leftLabel3=gtk.Label(" seconds ")
            leftHBox.pack_start(leftLabel3,False,False,0)
            innerVBox.add(leftHBox)
        elif context.targets==["right"]:
            rightHBox=gtk.HBox()
            rightLabel1=gtk.Label(" turn right at speed ")
            rightHBox.pack_start(rightLabel1,False,False,0)
            rightSpin1=gtk.SpinButton(gtk.Adjustment(1,-1,1,.1,.5,0),0.0,1)
            rightHBox.pack_start(rightSpin1,False,False,0)
            rightLabel2=gtk.Label(" for ")
            rightHBox.pack_start(rightLabel2,False,False,0)
            rightSpin2=gtk.SpinButton(gtk.Adjustment(1,0,10,.1,.5,0),0.0,1)
            rightHBox.pack_start(rightSpin2,False,False,0)
            rightLabel3=gtk.Label(" seconds ")
            rightHBox.pack_start(rightLabel3,False,False,0)
            innerVBox.add(rightHBox)
        elif context.targets==["stop"]:
            stopHBox=gtk.HBox()
            stopLabel=gtk.Label(" stop ")
            stopHBox.pack_start(stopLabel)
            innerVBox.add(stopHBox)
        elif context.targets==["motors"]:
            motorsHBox=gtk.HBox()
            motorsLabel1=gtk.Label(" left motor at speed ")
            motorsHBox.pack_start(motorsLabel1)
            motorsSpin1=gtk.SpinButton(gtk.Adjustment(1,-1,1,.1,.5,0),0.0,1)
            motorsHBox.pack_start(motorsSpin1)
            motorsLabel2=gtk.Label(" and right motor at speed ")
            motorsHBox.pack_start(motorsLabel2)
            motorsSpin2=gtk.SpinButton(gtk.Adjustment(1,-1,1,.1,.5,0),0.0,1)
            motorsHBox.pack_start(motorsSpin2)
            innerVBox.add(motorsHBox)
        elif context.targets==["wait"]:
            waitHBox=gtk.HBox()
            waitLabel1=gtk.Label(" wait ")
            waitHBox.pack_start(waitLabel1)
            waitSpin=gtk.SpinButton(gtk.Adjustment(1,0,10,.1,.5,0),0.0,1)
            waitHBox.pack_start(waitSpin)
            waitLabel2=gtk.Label(" seconds ")
            waitHBox.pack_start(waitLabel2)
            innerVBox.add(waitHBox)
        elif context.targets==["currenttime"]:
            currenttimeHBox=gtk.HBox()
            currenttimeLabel=gtk.Label(" current time ")
            currenttimeHBox.add(currenttimeLabel)
            innerVBox.add(currenttimeHBox)
        elif context.targets==["settimer"]:
            settimerHBox=gtk.HBox()
            settimerComboBox=gtk.combo_box_new_text()
            settimerComboBox.append_text("start")
            settimerComboBox.append_text("pause")
            settimerComboBox.append_text("stop")
            settimerComboBox.append_text("reset")
            settimerHBox.pack_start(settimerComboBox,False,False,0)
            settimerLabel=gtk.Label(" timer ")
            settimerHBox.pack_start(settimerLabel,False,False,0)
            innerVBox.add(settimerHBox)
        elif context.targets==["elapsed"]:
            elapsedHBox=gtk.HBox()
            elapsedLabel=gtk.Label(" time elapsed ")
            elapsedHBox.add(elapsedLabel)
            innerVBox.add(elapsedHBox)
        elif context.targets==["askuser"]:
            askuserHBox=gtk.HBox()
            askuserLabel=gtk.Label(" ask user for a ")
            askuserHBox.pack_start(askuserLabel,False,False,0)
            askuserComboBox=gtk.combo_box_entry_new_text()
            askuserComboBox.append_text("number")
            askuserComboBox.append_text("string")
            askuserComboBox.append_text("yes/no")
            askuserHBox.pack_start(askuserComboBox,False,False,0)
            innerVBox.add(askuserHBox)
        elif context.targets==["gamepad"]:
            gamepadHBox=gtk.HBox()
            gamepadLabel=gtk.Label(" use gamepad ")
            gamepadHBox.pack_start(gamepadLabel,False,False,0)
            innerVBox.add(gamepadHBox)
        elif context.targets==["joystick"]:
            joystickHBox=gtk.HBox()
            joystickLabel=gtk.Label(" use joystick ")
            joystickHBox.pack_start(joystickLabel,False,False,0)
            innerVBox.add(joystickHBox)
        elif context.targets==["takepic"]:
            takepicHBox=gtk.HBox()
            takepicLabel1=gtk.Label(" take ")
            takepicHBox.pack_start(takepicLabel1,False,False,0)
            takepicComboBox=gtk.combo_box_new_text()
            takepicComboBox.append_text("color")
            takepicComboBox.append_text("grayscale")
            takepicComboBox.append_text("blob")
            takepicHBox.pack_start(takepicComboBox,False,False,0)
            takepicLabel2=gtk.Label(" picture ")
            takepicHBox.pack_start(takepicLabel2,False,False,0)
            innerVBox.add(takepicHBox)
        elif context.targets==["showpic"]:
            showpicHBox=gtk.HBox()
            showpicLabel=gtk.Label(" show ")
            showpicHBox.pack_start(showpicLabel,False,False,0)
            showpicComboBox=gtk.combo_box_new_text()
            showpicComboBox.append_text("<picture>")
            showpicHBox.pack_start(showpicComboBox,False,False,0)
            innerVBox.add(showpicHBox)
        elif context.targets==["loadpic"]:
            loadpicHBox=gtk.HBox()
            loadpicLabel=gtk.Label(" load ")
            loadpicHBox.pack_start(loadpicLabel,False,False,0)
            loadpicFileChooser=gtk.FileChooserButton("Choose a Picture File")
            loadpicHBox.pack_start(loadpicFileChooser,False,False,0)
            innerVBox.add(loadpicHBox)
        elif context.targets==["savepic"]:
            savepicHBox=gtk.HBox()
            savepicLabel1=gtk.Label(" save ")
            savepicHBox.pack_start(savepicLabel1,False,False,0)
            savepicComboBox=gtk.combo_box_new_text()
            savepicComboBox.append_text("<picture>")
            savepicHBox.pack_start(savepicComboBox,False,False,0)
            savepicLabel2=gtk.Label(" to ")
            savepicFileChooser=gtk.FileChooserButton("Choose a File")
            savepicHBox.pack_start(savepicFileChooser,False,False,0)
            innerVBox.add(savepicHBox)
        elif context.targets==["copypic"]:
            copypicHBox=gtk.HBox()
            copypicLabel=gtk.Label(" copy ")
            copypicHBox.pack_start(copypicLabel,False,False,0)
            copypicComboBox=gtk.combo_box_new_text()
            copypicComboBox.append_text("<picture>")
            copypicHBox.pack_start(copypicComboBox,False,False,0)
            innerVBox.add(copypicHBox)
        elif context.targets==["pixels"]:
            pixelsHBox=gtk.HBox()
            pixelsLabel=gtk.Label(" pixels of ")
            pixelsHBox.pack_start(pixelsLabel,False,False,0)
            pixelsComboBox=gtk.combo_box_new_text()
            pixelsComboBox.append_text("<picture>")
            pixelsHBox.pack_start(pixelsComboBox,False,False,0)
            innerVBox.add(pixelsHBox)
        elif context.targets==["pixel"]:
            pixelHBox=gtk.HBox()
            pixelLabel1=gtk.Label(" pixel of ")
            pixelHBox.pack_start(pixelLabel1,False,False,0)
            pixelComboBox=gtk.combo_box_new_text()
            pixelComboBox.append_text("<picture>")
            pixelHBox.pack_start(pixelComboBox,False,False,0)
            pixelLabel2=gtk.Label(" at position ")
            pixelHBox.pack_start(pixelLabel2,False,False,0)
            pixelSpin1=gtk.SpinButton(gtk.Adjustment(100, 0, 256, 1, 5, 0),0.0,0)
            pixelHBox.pack_start(pixelSpin1,False,False,0)
            pixelLabel3=gtk.Label(" , ")
            pixelHBox.pack_start(pixelLabel3,False,False,0)
            pixelSpin2=gtk.SpinButton(gtk.Adjustment(100,0,192,1,5,0),0.0,0)
            pixelHBox.pack_start(pixelSpin2,False,False,0)
            innerVBox.add(pixelHBox)
        elif context.targets==["playsound"]:
            playsoundHBox=gtk.HBox()
            playsoundLabel=gtk.Label(" play ")
            playsoundHBox.pack_start(playsoundLabel,False,False,0)
            playsoundComboBox=gtk.combo_box_new_text()
            playsoundComboBox.append_text("<sound>")
            playsoundHBox.pack_start(playsoundComboBox,False,False,0)
            innerVBox.add(playsoundHBox)
        elif context.targets==["loadsound"]:
            loadsoundHBox=gtk.HBox()
            loadsoundLabel=gtk.Label(" load ")
            loadsoundHBox.pack_start(loadsoundLabel,False,False,0)
            loadsoundFileChooser=gtk.FileChooserButton("Choose a Sound File (.wav)")
            loadsoundHBox.pack_start(loadsoundFileChooser,False,False,0)
            innerVBox.add(loadsoundHBox)
        elif context.targets==["beep"]:
            beepHBox=gtk.HBox()
            beepLabel1=gtk.Label(" beep at frequency ")
            beepHBox.pack_start(beepLabel1,False,False,0)
            beepSpin1=gtk.SpinButton(gtk.Adjustment(440,25,4200,10,50,0),0.0,2)
            beepHBox.pack_start(beepSpin1,False,False,0)
            beepLabel2=gtk.Label(" for ")
            beepHBox.pack_start(beepLabel2,False,False,0)
            beepSpin2=gtk.SpinButton(gtk.Adjustment(1,0,10,1,2,0),0.0,1)
            beepHBox.pack_start(beepSpin2,False,False,0)
            beepLabel3=gtk.Label(" seconds ")
            beepHBox.pack_start(beepLabel3,False,False,0)
            innerVBox.add(beepHBox)
        elif context.targets==["speak"]:
            speakHBox=gtk.HBox()
            speakLabel1=gtk.Label(" say ")
            speakHBox.pack_start(speakLabel1,False,False,0)
            speakTextEntry=gtk.Entry()
            speakTextEntry.set_text("Hello World")
            speakHBox.pack_start(speakTextEntry,False,False,0)
            innerVBox.add(speakHBox)
        elif context.targets==["setvoice"]:
            setvoiceHBox=gtk.HBox()
            setvoiceLabel=gtk.Label(" set voice to ")
            setvoiceHBox.pack_start(setvoiceLabel,False,False,0)
            setvoiceComboBox=gtk.combo_box_new_text()
            setvoiceComboBox.append_text("<voice>")
            setvoiceHBox.pack_start(setvoiceComboBox,False,False,0)
            innerVBox.add(setvoiceHBox)
        elif context.targets==["beep2"]:
            beep2HBox=gtk.HBox()
            beep2Label1=gtk.Label(" beep at frequency ")
            beep2HBox.pack_start(beep2Label1,False,False,0)
            beep2Spin1=gtk.SpinButton(gtk.Adjustment(440,25,4200,10,50,0),0.0,2)
            beep2HBox.pack_start(beep2Spin1,False,False,0)
            beep2Label2=gtk.Label(" and frequency ")
            beep2HBox.pack_start(beep2Label2,False,False,0)
            beep2Spin2=gtk.SpinButton(gtk.Adjustment(440,25,4200,10,50,0),0.0,2)
            beep2HBox.pack_start(beep2Spin2,False,False,0)
            beep2Label3=gtk.Label(" for ")
            beep2HBox.pack_start(beep2Label3,False,False,0)
            beep2Spin3=gtk.SpinButton(gtk.Adjustment(1,0,10,.1,.5,0),0.0,1)
            beep2HBox.pack_start(beep2Spin3,False,False,0)
            beep2Label4=gtk.Label(" seconds ")
            beep2HBox.pack_start(beep2Label4,False,False,0)
            innerVBox.add(beep2HBox)
        else:
            pass
        self.window.show_all()
        
    def runProgram(self,widget,data=None):
        try:
            print printString.get_text()
        except NameError:
            pass
        

    def main(self):
        gtk.main()

if __name__ == "__main__":
    pyj=Pyjama()
    pyj.main()
