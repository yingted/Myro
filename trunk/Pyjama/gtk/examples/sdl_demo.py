import SdlDotNet
import Tao.Sdl
import System

def JoystickAxisChanged(sender, e):
    print e

SdlDotNet.Input.Joysticks.Initialize()
#SdlDotNet.Core.Events.JoystickButtonDown += JoystickAxisChanged
js = SdlDotNet.Input.Joysticks.OpenJoystick(0)
#SdlDotNet.Core.Events.Run()
for i in range(10000):
    Tao.Sdl.Sdl.SDL_PumpEvents()
    print js.GetButtonState(0)
    
