import clr
import sys
import System

sys.path.Append(System.Environment.CurrentDirectory+r"\services")
clr.AddReference("Ccr.Core.dll")
clr.AddReference("DssBase.dll")
clr.AddReference("DssRuntime.dll")
clr.AddReference("DssEnvironment.dll")
clr.AddReference("RoboticsCommon.Proxy.dll")

from Microsoft.Ccr.Core import *
from Microsoft.Dss.Hosting import *
import Microsoft.Robotics.Services.ContactSensor.Proxy as bumper 
import Microsoft.Robotics.Services.Drive.Proxy as drive 

manifestLocation = System.IO.Path.Combine(System.Environment.CurrentDirectory,"..\samples\IPRE\Scribbler\Batch Files\IPRE.Scribbler.standard.manifest.xml")
DssEnvironment.Initialize(50000, 50001,manifestLocation)

bumperSvcUri = None
driveSvcUri = None

while True:
  if bumperSvcUri is None :
    bumperSvcUri = DssEnvironment.FindService(bumper.Contract.Identifier)
  if driveSvcUri is None :
    driveSvcUri = DssEnvironment.FindService(drive.Contract.Identifier)
  if bumperSvcUri is not None and driveSvcUri is not None :
    break
  System.Threading.Thread.Sleep(1000)

_drivePort = DssEnvironment.ServiceForwarder[drive.DriveOperations](driveSvcUri)
_bumperPort = DssEnvironment.ServiceForwarder[bumper.ContactSensorArrayOperations](bumperSvcUri)
bumperNotificationPort = bumper.ContactSensorArrayOperations()
_bumperPort.Subscribe(bumperNotificationPort)

motorsLastOn = False

def bumperUpdate(notification):
  global motorsLastOn, _drivePort 
  if not notification.Body.Pressed :
    return
  driveRequest = drive.SetDrivePowerRequest()
  if motorsLastOn :
    driveRequest.LeftWheelPower = 0
    driveRequest.RightWheelPower = 0
  else :
    driveRequest.LeftWheelPower = 1
    driveRequest.RightWheelPower = 1
  motorsLastOn = not motorsLastOn
  _drivePort.SetDrivePower(driveRequest)
  print "Bumper Hit! Running motors: "+motorsLastOn.ToString()

Arbiter.Activate(DssEnvironment.TaskQueue,Arbiter.Receive[bumper.Update](True, bumperNotificationPort,bumperUpdate) )

print "Wait a few seconds for bumpers or press 'Enter' anytime to exit"
System.Console.ReadLine()
System.Environment.Exit(0)
