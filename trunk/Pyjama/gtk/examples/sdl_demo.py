import SdlDotNet
import System

def JoystickAxisChanged(sender, e):
    print e

SdlDotNet.Input.Joysticks.Initialize()
SdlDotNet.Core.Events.JoystickButtonDown += JoystickAxisChanged

js = SdlDotNet.Input.Joysticks.OpenJoystick(0)
SdlDotNet.Core.Events.Run()
