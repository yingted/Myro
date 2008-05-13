import clr
import sys
import System

#sys.path.Append(System.Environment.CurrentDirectory+r"\services")
sys.path.append(r"C:\Microsoft Robotics Studio (1.5)\bin")
clr.AddReference("Ccr.Core.dll")
clr.AddReference("DssBase.dll")
clr.AddReference("DssRuntime.dll")
clr.AddReference("DssRuntime.Proxy.dll")
clr.AddReference("DssEnvironment.dll")
clr.AddReference("RoboticsCommon.Proxy.dll")

from Microsoft.Ccr.Core import *
from Microsoft.Dss.Core import *
from Microsoft.Dss.Hosting import *
from Microsoft.Dss.ServiceModel.Dssp import *
import Microsoft.Robotics.Services.ContactSensor.Proxy as bumper 

manifestLocation = System.IO.Path.Combine(System.Environment.CurrentDirectory,"..\IPRE\Scribbler\Scribbler\Batch Files\IPRE.Scribbler.standard.manifest.xml")
DssEnvironment.Initialize(50000, 50001, manifestLocation)

while True:
  bumperSvcUri = DssEnvironment.FindService(bumper.Contract.Identifier)
  if bumperSvcUri :
    break
  System.Threading.Thread.Sleep(1000)

_bumperPort = DssEnvironment.ServiceForwarder[bumper.ContactSensorArrayOperations](bumperSvcUri)
bumperNotificationPort = bumper.ContactSensorArrayOperations()
_bumperPort.Subscribe(bumperNotificationPort)

def bumperUpdate(notification):
  if notification.Body.Pressed :
    print "Bumper Hit!"

Arbiter.Activate(DssEnvironment.TaskQueue,Arbiter.Receive[bumper.Update](True, bumperNotificationPort,bumperUpdate) )

print "Wait a few seconds for bumpers or press 'Enter' anytime to exit"
System.Console.ReadLine()
System.Environment.Exit(0)
