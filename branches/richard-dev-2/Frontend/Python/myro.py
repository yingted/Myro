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
def forward(power):
    Robot.Forward(power)
def forwardFor(power, seconds):
    Robot.ForwardFor(power, seconds)
def backward(power):
    Robot.Backward(power)
def backwardFor(power, seconds):
    Robot.BackwardFor(power, seconds)
def turn(direction, power):
    Robot.Turn(direction, power)
def turnFor(direction, power, seconds):
    Robot.TurnFor(direction, power, seconds)
def turnLeft(power):
    Robot.TurnLeft(power)
def turnLeftFor(power, seconds):
    Robot.TurnLeftFor(power, seconds)
def turnRight(power):
    Robot.TurnRight(power)
def turnRightFor(power, seconds):
    Robot.TurnRightFor(power, seconds)
def stop():
    Robot.Stop
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
