*********************************
Repository Files
*********************************
This repository contains files for the Java implementation of the Myro API.
Specifically:

INSTALL.txt: Instructions for installing Myro-Java.

Binaries: the binary libraries and .jar files required by Myro-Java.  Installation
instructions are included in the various system sub directories.

JInput-Test: A BlueJ project for testing communication with the gamepad

MyroProject: a BlueJ project that contains the Myro-Java source code.  This is 
for informational purposes only - the Myro package is included in the Binaries
folder as a jar file.

Demos: a BlueJ project with some demonstation programs.

scribbler-rxtx-devel: Source code and binaries for the scribbler-adapted rxtx
library, used for communication with the scribbler/fluke.  The files in this
directory are not required (hopefully!) to install myro-Java, but are included here
for information purposes only.


*********************************
Bluetooth Setup
*********************************
The Bluetooth interface can be configured by following the instructions given
in the "Hardware Setup" section of the Myro installation instructions available
at:

  http://wiki.roboteducation.org/Myro_Installation_Manual

*********************************
Using Myro-Java
*********************************
Myro-Java can be used in three general ways in BlueJ: Create an instance, use
the codepad, or write Java application.

1) Create an instance.  Open the "MyroProject" project in BlueJ and navigate to the
Myro package by double-clicking on "Myro".  Several classes are defined in the
Myro package, including Scribbler.  An instance of Scribbler can be created by
right-clicking on the Scribbler icon and selecting "new Myro.Scribbler" from the
popup menu.  The constructor requires the device name of the Scribbler, so enter
this in the dialog box that appears (e.g., "com3", "/dev/tty.scribbler", etc.).

A new Scribbler object will appear in the lower left portion of the BlueJ
window.  Methods can be executed by right-clicking on the object and selecting
the method.  This is a good way to play with simple motor control, the joystick,
the gamepad, senses, etc.  When you're finished with the instance it's important
to invoke the close method before removing the instance.

2) Codepad.  To use the codepad you must first display the code pad from the
BlueJ View menu.  You then create an instance of the Scribbler as described in the
previous section.  This instance can then be controlled by entering a command
in the code pad section of the BlueJ window (i.e., the lower right corner).  For
example, the following command will take a color picture and display it:

  scribble1.takePicture(0).show();

3) Java application.  Several example applications are included in the Demo project.
