import serial
import time
import logging
import sys
import thread
from threading import *

from scribbler import *


def demo0():

    while 1:

        scrib.set_led_center_off()
        time.sleep(.1)
        scrib.set_led_center_on()
        time.sleep(.1)

def demo1():
    while 1:
        if (scrib.get_line_left()):
            scrib.set_led_left_on()
        else:
            scrib.set_led_left_off()
            
        if (scrib.get_line_right()):
            scrib.set_led_right_on()
        else:
            scrib.set_led_right_off()
                

def demo2():
    while 1:
        if (scrib.get_open_left() == 0):
            scrib.set_led_left_on()
        else:
            scrib.set_led_left_off()
            
        if (scrib.get_open_right() == 0):
            scrib.set_led_right_on()
        else:
            scrib.set_led_right_off()
            
def demo3():
    state = 0

    while 1:
        if (state == 0):
            if (scrib.get_stall() or 
                (scrib.get_open_right() == 0 and scrib.get_open_left() == 0)):
                set_led_left_off()
                set_led_right_off()
                if scrib.get_stall():
                    set_led_center_off()
                    set_motor(BACKWARD, BACKWARD) 	
                    state = 1
            elif (scrib.get_open_right() == 0):
                set_led_right_off()
                set_motor(FORWARDSLOW, BACKWARDSLOW) 
            elif (scrib.get_open_left() == 0):
                set_led_left_off()
                set_motor(BACKWARDSLOW, FORWARDSLOW) 
            else:
                set_led_right_on()
                set_led_center_on()
                set_led_left_on()
                set_motor(FORWARD,FORWARD)
        
        if (state == 1):
            set_motor(BACKWARDSLOW, FORWARDSLOW) 
            state = 0		




def demo4():
    
    while 1:
        
        c = sys.stdin.read(1)
        #c = win32_getch()
        
        if (c == "i"):                
            scrib.set_motor(FORWARD,FORWARD)            
        elif (c == "l"):
            scrib.set_motor(BACKWARDSLOW, FORWARDSLOW)
        elif (c == "j"):
            scrib.set_motor(FORWARDSLOW, BACKWARDSLOW)
        elif (c == "k"):
            scrib.set_motor(STOP, STOP)
        elif (c == "m"):
            scrib.set_motor(BACKWARD, BACKWARD)
        elif (c == "q"):
            return

        

fd = sys.stdin.fileno()

def win32_getch():

    ch = "a"
    if (msvcrt.kbhit()):
        ch = msvcrt.getche()
    return ch

def getch():
    saved_attributes = termios.tcgetattr(fd)
    try:
        attributes = termios.tcgetattr(fd)            #get a fresh copy!
        attributes[3] = attributes[3] & ~(TERMIOS.ICANON | TERMIOS.ECHO)
        attributes[6][TERMIOS.VMIN] = 1
        attributes[6][TERMIOS.VTIME] = 0
        termios.tcsetattr(fd, TERMIOS.TCSANOW, attributes)
        a = sys.stdin.read(1)
    finally:            #be sure to reset the attributes no matter what!
        termios.tcsetattr(fd, TERMIOS.TCSANOW, saved_attributes)
    return a

# lock=thread.allocate_lock()

class itemQ:

    def __init__(self):
        self.count=0
        
    def produce(self, ch):
        self.count = 1;
        self.ch = ch
        
    def consume(self):
        self.count = 0
        return self.ch
    
    def isEmpty(self):
        return not self.count
                

class CharProducer(Thread):

    def __init__(self,condition,itemq,sleeptime=0):
        Thread.__init__(self)
        self.cond=condition
        self.itemq=itemq
        self.sleeptime=sleeptime
        
    def run(self):
        cond=self.cond
        itemq=self.itemq
        
        while 1 :

            #x = sys.stdin.read(1)
            x = win32_getch()
            
            cond.acquire() #acquire the lock
            if (x != "a"):
                #print currentThread(),"Produced One Item", x
                itemq.produce(x)
            #cond.notifyAll()
            cond.release()
            if (x == "q"):
                return
            
            time.sleep(self.sleeptime)

class Commander(Thread):

    def __init__(self,condition,itemq,sleeptime=0):
        Thread.__init__(self)
        self.cond=condition
        self.itemq=itemq
        self.sleeptime=sleeptime
        
    def run(self):
        
        laststate = 3
        state = 0
        cond=self.cond
        itemq=self.itemq

        c = "a"
        while 1:

            #c = "a"
            time.sleep(self.sleeptime)
            
            cond.acquire() #acquire the lock
            
            if (not itemq.isEmpty()):
                c = itemq.consume()
                #print currentThread(),"Consumed One Item",c
                
            cond.release()

            if (c == "f"):                
                scrib.set_motor(FORWARD,FORWARD)                
            elif (c == "i"):
                    
                if (scrib.get_stall() or (scrib.get_open_right() == 0
                                    and scrib.get_open_left() == 0)):

                    scrib.set_led_left_off()
                    scrib.set_led_right_off()
                    
                    scrib.set_motor(BACKWARD, BACKWARD)
                    state = laststate
                        
                elif (scrib.get_open_right() == 0):
                    scrib.set_led_right_off()
                    scrib.set_motor(BACKWARD, BACKWARD)
                    state = 2
                    laststate = state
                elif (scrib.get_open_left() == 0):
                    scrib.set_led_left_off()
                    state = 1
                    laststate = state
                    scrib.set_motor(BACKWARD, BACKWARD)
                else:
                    scrib.set_led_right_on()
                    scrib.set_led_center_on()
                    scrib.set_led_left_on()
#                    scrib.set_motor(FORWARD,FORWARD)

                    if (state <= 0):
                        scrib.set_motor(FORWARD,FORWARD)
                        state = 0
                    elif (state > 0 and ((state % 2) == 0)):
                        scrib.set_motor(STOP, FORWARDSLOW)
                        state -=  2
                    elif (state > 0 and ((state % 2) == 1)):
                        scrib.set_motor(FORWARDSLOW, STOP)
                        state -= 2
                    
                                                
            elif (c == "l"):
                scrib.set_motor(BACKWARDSLOW, FORWARDSLOW)
            elif (c == "j"):
                scrib.set_motor(FORWARDSLOW, BACKWARDSLOW)
            elif (c == "k"):
                scrib.set_motor(STOP, STOP)
            elif (c == "m"):
                scrib.set_motor(BACKWARD, BACKWARD)
            elif (c == "q"):
                return
#            else:
#                scrib.set_motor(STOP, STOP)

def demo5():
    q=itemQ()
    
    cond=Lock()

    pro=CharProducer(cond,q)
    cons=Commander(cond,q)
    
    pro.start()
    cons.start()
    pro.join()
    cons.join()
    

if __name__=="__main__":
    
    if (sys.platform == "win32"):
        import msvcrt
        #serialport = "/dev/tty.eb500-A7SerialPort-1"
        #serialport = 3
        serialport = 4
    else:    
        import termios, TERMIOS
        import fcntl, os
        serialport = "/dev/rfcomm0"

    scrib = Scribbler(serialport)
    scrib.init()
    demo5()
    scrib.close()

