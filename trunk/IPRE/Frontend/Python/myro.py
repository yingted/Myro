# Copyright (c) Microsoft Corporation.  All rights reserved.

import clr
import System
import System.IO
import sys

clr.AddReference("MyroRobot")

# This import line will bring in most of the API that is implemented in C#
from Myro.Robot import *

#from Myro.Utilities import Params


# Importing from Myro.Robot gets us most of the movement and sound functions.
# We still need to define the "magic" get/set methods in Python.  These can return either
# single values or arrays, and "all" is a special keyword.

def get(sensor = "all", position = "all"):
    if type(position) == str:
        position = position.lower()
    
    # Fix "aliases"
    if position == "center":
        position = "middle"
        
    # Different return type for "all"
    if position == "all":
        if(sensor == "stall"):
            return Get("stall", 0)
        else:
            return Get(sensor);
    else:
        return Get(sensor, position);

def getLED(position = "all"):
    return get("led", position)        
def getIR(position = "all"):
    return get("ir", position)
def getLight(position = "all"):
    return get("light", position)
def getObstacle(position = "all"):
    return get("obstacle", position)
def getLine(position = "all"):
    return get("line", position)
def getStall():
    return get("stall")
    
def setLED(position = "all", value=1):
    return set("led", position, value)
def setLEDFront(value):
    return set("led", "front", value)
def setLEDBack(value):
    return set("led", "back", value)
        

def set(sensor, positionorvalue, value = None):
    if type(sensor) == str:
        sensor = sensor.lower()
    if type(positionorvalue) == str:
        positionorvalue = positionorvalue.lower()
    
    # Fix "aliases"
    if(value != None):
        if positionorvalue == "center":
            positionorvalue = "middle"
        if value == "on":
            value = 1.0
        elif value == "off":
            value = 0.0
    
    if sensor == "volume":
        setLoud(positionorvalue)
    elif sensor == "led" and positionorvalue == "all":
        for i in range(5):
            Set("led", i, value)
    else:
        if(value != None):
            Set(sensor, positionorvalue, value)
        else:
            raise System.ArgumentException("You must specify a position")
        
def takePicture(type = "jpeg"):
    type = type.lower()
    if(type == "grey"):
        type = "gray"
    return TakePicture(type)

#def init(baseName):
    #f = System.IO.Path.Combine(System.IO.Path.Combine(Params.ConfigPath, baseName + ".manifest"), baseName + ".manifest.xml")
    ##f = str.Concat("C:\\Microsoft Robotics Dev Studio 2008\\config\\", robotType, ".manifest\\", robotType, ".manifest.xml")
    #try:
        #Robot.Init(baseName)
    #except Exception, e:
        #print "Error connecting with manifest file", f
        #print e
        #
#def shutdown():
    #Robot.Shutdown() 
#def move(translate, rotate):
    #Robot.Move(translate, rotate)
#def forward(power, seconds = 0):
    #if seconds == 0:
        #Robot.Forward(power)
    #else:
        #Robot.ForwardFor(power, seconds)
#def backward(power, seconds = 0):
    #if seconds == 0:
        #Robot.Backward(power)
    #else:
        #Robot.Backward(power, seconds)
#def turn(direction, power, seconds = 0):
    #if seconds == 0:
        #Robot.Turn(direction, power)
    #else:
        #Robot.TurnFor(direction, power, seconds)
#def turnLeft(power, seconds = 0):
    #if seconds == 0:
        #Robot.TurnLeft(power)
    #else:
        #Robot.TurnForLeft(power, seconds)
#def turnRight(power, seconds = 0):
    #if seconds == 0:
        #Robot.TurnRight(power)
    #else:
        #Robot.TurnRightFor(power, seconds)
#def stop():
    #Robot.Stop()
#def setMotors(leftPower, rightPower):
    #Robot.SetMotors(leftPower, rightPower)
#def setMotorsFor(leftPower, rightPower, seconds):
    #Robot.SetMotorsFor(leftPower, rightPower, seconds)
#def readSong(filename):
    #return Robot.ReadSong(filename)
#def makeSong(text):
    #return Robot.MakeSong(text)
#def saveSong(text, filename):
    #Robot.SaveSong(text, filename)
#def playSong(song):
    #Robot.PlaySong(song)
#def beep(duration, frequency1, frequency2=0):
    #Robot.Beep(duration, frequency1, frequency2)
#def setLoud(loud):
    #if loud != 0 and loud != 1:
        #raise System.ArgumentException("Loudness must be 0 or 1")
    #else:
        #Robot.setLoud(loud)
#def get(name, pos="all"):
    #if pos == "all":
	    #return tuple(Robot.Get(name))
    #else:
        #return Robot.Get(name, pos)
#def getNames(name):
    #return tuple(Robot.GetNames(name))
#def getPairs(name):
    #(names, values) = Robot.GetPairs(name)
    #return (tuple(names), tuple(values))
#def set(name, pos, value):
    #Robot.Set(name, pos, value)
