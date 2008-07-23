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

        #makes codevbox a drag destination
        self.wTree.get_widget("codevbox").drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP, [("print",0,4),("forward",0,5),("backward",0,6),("left",0,7),("right",0,8),("stop",0,9),("motors",0,10),("wait",0,11),("currenttime",0,12),("settimer",0,13),("elapsed",0,14),("askuser",0,15),("gamepad",0,16),("joystick",0,17),("takepic",0,18),("showpic",0,19),("loadpic",0,20),("savepic",0,21)], gtk.gdk.ACTION_COPY)
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
            newHBox=gtk.HBox(True,2)
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
            newVBox=gtk.VBox(True,3)
            newHBox.pack_start(newVBox,False,False,0)
            #makes newScriptHBox global so it can be used to change scripts
            global newScriptHBox
            newScriptHBox=gtk.HBox(True,1)
            newVBox.pack_start(newScriptHBox,False,False,0)
            newDoLabel=gtk.Label(" do: ")
            newScriptHBox.pack_start(newDoLabel,False,False,0)
            #make newCodeVBox global so it can be used in drag and drop
            global newCodeVBox
            newCodeVBox=gtk.VBox(True,1)
            newVBox.pack_start(newCodeVBox,False,False,0)
            newReturn=gtk.HBox(True,2)
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
            newCodeVBox.drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP, [("print",0,4),("forward",0,5),("backward",0,6),("left",0,7),("right",0,8),("stop",0,9),("motors",0,10),("wait",0,11),("currenttime",0,12),("settimer",0,13),("elapsed",0,14),("askuser",0,15),("gamepad",0,16),("joystick",0,17),("takepic",0,18),("showpic",0,19),("loadpic",0,20),("savepic",0,21)], gtk.gdk.ACTION_COPY)
            newCodeVBox.connect("drag_drop",self.codeDragDrop)
        else:
            pass
            
    def scriptChanger(self,widget,data=None):
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
            whenhbox=gtk.HBox(True,3)
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
            givenhbox=gtk.HBox(True,3)
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
            whengivenhbox=gtk.HBox(True,5)
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
        if self.wTree.get_widget("dinahscriptsnotebook").get_current_page()==0:
            codevbox=self.wTree.get_widget("codevbox")
        else:
            codevbox=newCodeVBox
        if context.targets==["print"]:
            global printString
            innerVBox=gtk.VBox(True,1)
            codevbox.add(innerVBox)
            printHBox=gtk.HBox(True,2)
            printLabel=gtk.Label(" print ")
            printHBox.pack_start(printLabel,False,False,0)
            printString=gtk.Entry()
            printHBox.pack_start(printString,False,False,0)
            innerVBox.add(printHBox)
        elif context.targets==["forward"]:
            innerVBox=gtk.VBox(True,1)
            codevbox.add(innerVBox)
            forwardHBox=gtk.HBox(True,5)
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
            innerVBox=gtk.VBox(True,1)
            codevbox.add(innerVBox)
            backwardHBox=gtk.HBox(True,5)
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
            innerVBox=gtk.VBox(True,1)
            codevbox.add(innerVBox)
            leftHBox=gtk.HBox(True,5)
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
            innerVBox=gtk.VBox(True,1)
            codevbox.add(innerVBox)
            rightHBox=gtk.HBox(True,5)
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
            innerVBox2=gtk.VBox(True,1)
            codevbox.add(innerVBox)
            stopHBox=gtk.HBox(True,1)
            stopLabel=gtk.Label(" stop ")
            stopHBox.pack_start(stopLabel)
            innerVBox2.add(stopHBox)
        elif context.targets==["motors"]:
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
            motorsHBox=gtk.HBox(True,4)
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
            innerVBox=gtk.VBox(True,1)
            codevbox.add(innerVBox)
            waitHBox=gtk.HBox(True,3)
            waitLabel1=gtk.Label(" wait ")
            waitHBox.pack_start(waitLabel1)
            waitSpin=gtk.SpinButton(gtk.Adjustment(1,0,10,.1,.5,0),0.0,1)
            waitHBox.pack_start(waitSpin)
            waitLabel2=gtk.Label(" seconds ")
            waitHBox.pack_start(waitLabel2)
            innerVBox.add(waitHBox)
        elif context.targets==["currenttime"]:
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
            currenttimeHBox=gtk.HBox(True,1)
            currenttimeLabel=gtk.Label(" current time ")
            currenttimeHBox.add(currenttimeLabel)
            innerVBox.add(currenttimeHBox)
        elif context.targets==["settimer"]:
            innerVBox=gtk.VBox(True,1)
            codevbox.add(innerVBox)
            settimerHBox=gtk.HBox(True,2)
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
            innerVBox=gtk.VBox(True,1)
            codevbox.add(innerVBox)
            elapsedHBox=gtk.HBox(True,1)
            elapsedLabel=gtk.Label(" time elapsed ")
            elapsedHBox.add(elapsedLabel)
            innerVBox.add(elapsedHBox)
        elif context.targets==["askuser"]:
            innerVBox=gtk.VBox(True,1)
            codevbox.add(innerVBox)
            askuserHBox=gtk.HBox(True,2)
            askuserLabel=gtk.Label(" ask user for a ")
            askuserHBox.pack_start(askuserLabel,False,False,0)
            askuserComboBox=gtk.combo_box_entry_new_text()
            askuserComboBox.append_text("number")
            askuserComboBox.append_text("string")
            askuserComboBox.append_text("yes/no")
            askuserHBox.pack_start(askuserComboBox,False,False,0)
            innerVBox.add(askuserHBox)
        elif context.targets==["gamepad"]:
            innerVBox=gtk.VBox(True,1)
            codevbox.add(innerVBox)
            gamepadHBox=gtk.HBox(True,1)
            gamepadLabel=gtk.Label(" use gamepad ")
            gamepadHBox.pack_start(gamepadLabel,False,False,0)
            innerVBox.add(gamepadHBox)
        elif context.targets==["joystick"]:
            innerVBox=gtk.VBox(True,1)
            codevbox.add(innerVBox)
            joystickHBox=gtk.HBox(True,1)
            joystickLabel=gtk.Label(" use joystick ")
            joystickHBox.pack_start(joystickLabel,False,False,0)
            innerVBox.add(joystickHBox)
        elif context.targets==["takepic"]:
            innerVBox=gtk.VBox(True,1)
            codevbox.add(innerVBox)
            takepicHBox=gtk.HBox(True,3)
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
            innerVBox=gtk.VBox(True,1)
            codevbox.add(innerVBox)
            showpicHBox=gtk.HBox(True,2)
            showpicLabel=gtk.Label(" show ")
            showpicHBox.pack_start(showpicLabel,False,False,0)
            showpicComboBox=gtk.combo_box_new_text()
            showpicComboBox.append_text("<picture>")
            showpicHBox.pack_start(showpicComboBox,False,False,0)
            innerVBox.add(showpicHBox)
        elif context.targets==["loadpic"]:
            innerVBox=gtk.VBox(True,1)
            codevbox.add(innerVBox)
            loadpicHBox=gtk.HBox(True,2)
            loadpicLabel=gtk.Label(" load ")
            loadpicHBox.pack_start(loadpicLabel,False,False,0)
            loadpicFileChooser=gtk.FileChooserButton("Choose a Picture File")
            loadpicHBox.pack_start(loadpicFileChooser,False,False,0)
            innerVBox.add(loadpicHBox)
        elif context.targets==["savepic"]:
            innerVBox=gtk.VBox(True,1)
            codevbox.add(innerVBox)
            savepicHBox=gtk.HBox(True,4)
            savepicLabel1=gtk.Label(" save ")
            savepicHBox.pack_start(savepicLabel1,False,False,0)
            savepicComboBox=gtk.combo_box_new_text()
            savepicComboBox.append_text("<picture>")
            savepicHBox.pack_start(savepicComboBox,False,False,0)
            savepicLabel2=gtk.Label(" to ")
            savepicFileChooser=gtk.FileChooserButton("Choose a File")
            savepicHBox.pack_start(savepicFileChooser,False,False,0)
            innerVBox.add(savepicHBox)
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
