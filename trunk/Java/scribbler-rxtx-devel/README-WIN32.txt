Compiling and Installing rxtxScribbler on Win32 machines

1) Install MinGW (minimalist GNU) from http://sourceforge.net/projects/mingw/files/

	Note: The only utility that needs to be installed is the C compiler

2) Create a build folder in scribbler-rxtx-devel.

3) Copy scribbler-rxtx-devel/Makefile.mingw32 to build/Makefile

4) Edit build/Makefile so that correct paths are specified

5) Open DOS command tool.  Cd to build directory

6) Make the project using:

	mingw32-make

7) Copy build/RXTXScribblerComm.jar to .../jre/lib/ext

8) Copy biuld/rxtxScribblerSerial.dll to .../jre/bin





