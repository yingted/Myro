Compiling and Installing rxtxScribbler on Win64 machines

1) Install MinGW (minimalist GNU) from http://sourceforge.net/projects/mingw-w64/

	Note: I installed the sezero Personal Build since it included mingw32-make

	Note: Add c:\MinGW\bin to path (or wherever you copied the bin directory)

2) Create a build folder in scribbler-rxtx-devel.

3) Copy scribbler-rxtx-devel/Makefile.mingw32 to build/Makefile

4) Edit build/Makefile so that correct paths are specified

5) Open DOS command tool as administrator.  Cd to build directory

6) Make the project using:

	mingw32-make

7) Install the files using:

	mingw32-make install

   (or you can copy build/RXTXScribblerComm.jar to .../jre/lib/ext and  build/rxtxScribblerSerial.dll to .../jre/bin )





