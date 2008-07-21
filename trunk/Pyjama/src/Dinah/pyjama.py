#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk
import gtk.glade

class Pyjama:

    '''This is the the class for the main window for Pyjama.'''

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
        else:
            pass
        self.window.show_all()

        #makes innerVBox a drag destination
        innerVBox.drag_dest_set(gtk.DEST_DEFAULT_MOTION|gtk.DEST_DEFAULT_HIGHLIGHT|gtk.DEST_DEFAULT_DROP, [("print",0,4),("forward",0,5),("backward",0,6),("left",0,7),("right",0,8),("stop",0,9),("motors",0,10),("wait",0,11),("currenttime",0,12),("settimer",0,13),("elapsed",0,14),("askuser",0,15),("gamepad",0,16),("joystick",0,17),("takepic",0,18),("showpic",0,19),("loadpic",0,20),("savepic",0,21)], gtk.gdk.ACTION_COPY)
        innerVBox.connect("drag_drop",self.on_innerVBox_drag_drop)

    def on_innerVBox_drag_drop(self,source,context,x,y,time):
        outerVBox2=innerVBox
        if context.targets==["print"]:
            global printString
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
            printHBox=gtk.HBox(True,2)
            printLabel=gtk.Label(" print ")
            printHBox.pack_start(printLabel,False,False,0)
            printString=gtk.Entry()
            printHBox.pack_start(printString,False,False,0)
            innerVBox2.add(printHBox)
        elif context.targets==["forward"]:
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
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
            innerVBox2.add(forwardHBox)
        elif context.targets==["backward"]:
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
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
            innerVBox2.add(backwardHBox)
        elif context.targets==["left"]:
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
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
            innerVBox2.add(leftHBox)
        elif context.targets==["right"]:
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
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
            innerVBox2.add(rightHBox)
        elif context.targets==["stop"]:
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
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
            innerVBox2.add(motorsHBox)
        elif context.targets==["wait"]:
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
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
            innerVBox2.add(currenttimeHBox)
        elif context.targets==["settimer"]:
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
            settimerHBox=gtk.HBox(True,2)
            settimerComboBox=gtk.combo_box_new_text()
            settimerComboBox.append_text("start")
            settimerComboBox.append_text("pause")
            settimerComboBox.append_text("stop")
            settimerComboBox.append_text("reset")
            settimerHBox.pack_start(settimerComboBox,False,False,0)
            settimerLabel=gtk.Label(" timer ")
            settimerHBox.pack_start(settimerLabel,False,False,0)
            innerVBox2.add(settimerHBox)
        elif context.targets==["elapsed"]:
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
            elapsedHBox=gtk.HBox(True,1)
            elapsedLabel=gtk.Label(" time elapsed ")
            elapsedHBox.add(elapsedLabel)
            innerVBox.add(elapsedHBox)
        elif context.targets==["askuser"]:
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
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
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
            gamepadHBox=gtk.HBox(True,1)
            gamepadLabel=gtk.Label(" use gamepad ")
            gamepadHBox.pack_start(gamepadLabel,False,False,0)
            innerVBox2.add(gamepadHBox)
        elif context.targets==["joystick"]:
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
            joystickHBox=gtk.HBox(True,1)
            joystickLabel=gtk.Label(" use joystick ")
            joystickHBox.pack_start(joystickLabel,False,False,0)
            innerVBox2.add(joystickHBox)
        elif context.targets==["takepic"]:
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
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
            innerVBox2.add(takepicHBox)
        elif context.targets==["showpic"]:
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
            showpicHBox=gtk.HBox(True,2)
            showpicLabel=gtk.Label(" show ")
            showpicHBox.pack_start(showpicLabel,False,False,0)
            showpicComboBox=gtk.combo_box_new_text()
            showpicComboBox.append_text("<picture>")
            showpicHBox.pack_start(showpicComboBox,False,False,0)
            innerVBox2.add(showpicHBox)
        elif context.targets==["loadpic"]:
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
            loadpicHBox=gtk.HBox(True,2)
            loadpicLabel=gtk.Label(" load ")
            loadpicHBox.pack_start(loadpicLabel,False,False,0)
            loadpicFileChooser=gtk.FileChooserButton("Choose a Picture File")
            loadpicHBox.pack_start(loadpicFileChooser,False,False,0)
            innerVBox2.add(loadpicHBox)
        elif context.targets==["savepic"]:
            innerVBox2=gtk.VBox(True,1)
            outerVBox2.add(innerVBox2)
            savepicHBox=gtk.HBox(True,4)
            savepicLabel1=gtk.Label(" save ")
            savepicHBox.pack_start(savepicLabel1,False,False,0)
            savepicComboBox=gtk.combo_box_new_text()
            savepicComboBox.append_text("<picture>")
            savepicHBox.pack_start(savepicComboBox,False,False,0)
            savepicLabel2=gtk.Label(" to ")
            savepicFileChooser=gtk.FileChooserButton("Choose a File")
            savepicHBox.pack_start(savepicFileChooser,False,False,0)
            innerVBox2.add(savepicHBox)
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
