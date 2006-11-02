Microsoft Robotics Studio (October release) Scribbler services

Put the Scribbler, ScribblerServices, and ScribblerSim directories under:
C:\Microsoft Robotics Studio\samples\Platforms\IPRE

To run the normal scribbler services:
  Open Scribbler.csproj and compile+run with F5

To try out Robotics Tutorials 1 through 4:
  Copy the manifests to the C:\Microsoft Robotics Studio\samples\Config directory
  Then run the specific tutorial with the appropriate Scribbler manifest like so:
  
  C:\Microsoft Robotics Studio>dsshost -p:50000 -t:50001 -m:"samples\Config\RoboticsTutorial1.manifest.xml" -m:"samples\Config\IPRE.Scribbler.MotorTouchSensor.manifest.xml"

  C:\Microsoft Robotics Studio>dsshost -p:50000 -t:50001 -m:"samples\Config\RoboticsTutorial2.manifest.xml" -m:"samples\Config\IPRE.Scribbler.MotorTouchSensor.manifest.xml"

  C:\Microsoft Robotics Studio>dsshost -p:50000 -t:50001 -m:"samples\Config\RoboticsTutorial3.manifest.xml" -m:"samples\Config\IPRE.Scribbler.Vehicle.manifest.xml"

  C:\Microsoft Robotics Studio>dsshost -p:50000 -t:50001 -m:"samples\Config\RoboticsTutorial4.manifest.xml" -m:"samples\Config\IPRE.Scribbler.Vehicle.manifest.xml"

To run the Scribbler simulator:
  Copy Scribbler.bos to C:\Microsoft Robotics Studio\store\media
  Then open ScribblerSim.csproj and compile+run with F5


email me: baxelrod@cc.gatech.edu if you have problems.
