import clr
import System
import System.IO
import sys
clr.AddReferenceToFileAndPath("C:\\Microsoft Robotics Dev Studio 2008\\bin\\MyroRobot.dll")
clr.AddReferenceToFileAndPath("C:\\Microsoft Robotics Dev Studio 2008\\bin\\MyroUtilities.dll")
from Myro import Robot
from Myro.Utilities import Params

def init(baseName):
    f = System.IO.Path.Combine(System.IO.Path.Combine(Params.ConfigPath, baseName + ".manifest"), baseName + ".manifest.xml")
    #f = str.Concat("C:\\Microsoft Robotics Dev Studio 2008\\config\\", robotType, ".manifest\\", robotType, ".manifest.xml")
    try:
        Robot.Init(baseName)
    except Exception, e:
        print "Error connecting with manifest file", f
        print e
        
def shutdown():
    Robot.Shutdown() 
def move(translate, rotate):
    Robot.Move(translate, rotate)
def forward(power, seconds = 0):
    if seconds == 0:
        Robot.Forward(power)
    else:
        Robot.ForwardFor(power, seconds)
def backward(power, seconds = 0):
    if seconds == 0:
        Robot.Backward(power)
    else:
        Robot.Backward(power, seconds)
def turn(direction, power, seconds = 0):
    if seconds == 0:
        Robot.Turn(direction, power)
    else:
        Robot.TurnFor(direction, power, seconds)
def turnLeft(power, seconds = 0):
    if seconds == 0:
        Robot.TurnLeft(power)
    else:
        Robot.TurnForLeft(power, seconds)
def turnRight(power, seconds = 0):
    if seconds == 0:
        Robot.TurnRight(power)
    else:
        Robot.TurnRightFor(power, seconds)
def stop():
    Robot.Stop()
def setMotors(leftPower, rightPower):
    Robot.SetMotors(leftPower, rightPower)
def setMotorsFor(leftPower, rightPower, seconds):
    Robot.SetMotorsFor(leftPower, rightPower, seconds)
def readSong(filename):
    return Robot.ReadSong(filename)
def makeSong(text):
    return Robot.MakeSong(text)
def saveSong(text, filename):
    Robot.SaveSong(text, filename)
def playSong(song):
    Robot.PlaySong(song)
def beep(duration, frequency1, frequency2=0):
    Robot.Beep(duration, frequency1, frequency2)
def setLoud(loud):
    if loud != 0 and loud != 1:
        raise System.ArgumentException("Loudness must be 0 or 1")
    else:
        Robot.setLoud(loud)
def get(name, pos="all"):
    if pos == "all":
	    return tuple(Robot.Get(name))
    else:
        return Robot.Get(name, pos)
def getNames(name):
    return tuple(Robot.GetNames(name))
def getPairs(name):
    (names, values) = Robot.GetPairs(name)
    return (tuple(names), tuple(values))
def set(name, pos, value):
    Robot.Set(name, pos, value)
