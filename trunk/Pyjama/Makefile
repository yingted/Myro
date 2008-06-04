PROJECT=pyjama
HOSTINGVER=2

#PATH=/opt/mono-1.9.1/bin

# Where the IP dlls are:
IPPATH?=/home/dblank/fepy/IPCE/dist/ipy2

# Where site.py and other bits are:
IPSITE?=/opt/mono-1.9.1/lib/IPCE/Lib

#DLLS=-r:Mono.Posix.dll -r:IronPython.dll -r:IronMath.dll
#DLLS=-r:Mono.Posix.dll -r:IronPython.dll -r:IronPython.Modules.dll \
#	-r:Microsoft.Scripting.dll 
DLLS=-r:Mono.Posix.dll -r:IronPython.dll -r:IronPython.Modules.dll \
	-r:Microsoft.Scripting.dll -r:Microsoft.Scripting.Core.dll 

SRCS=src/Pyjama.cs src/PyjamaInterfaces.cs src/Utils.cs \
	src/TextDocument.cs src/Print.cs src/Shell.cs \
	src/PythonShell.cs src/PyjamaGUI.cs src/FindDialog.cs
CSFLAGS=-d:HOSTINGVER$(HOSTINGVER) -pkg:gtksourceview-sharp-2.0 -pkg:gtk-sharp-2.0 -pkg:glade-sharp-2.0 
CS=gmcs
MONO=mono

.PHONY: all

all:	$(PROJECT).exe

run:	$(PROJECT).exe
	MONO_PATH=$(IPPATH) IRONPYTHON_PATH=$(IPSITE) $(MONO) ./$(PROJECT).exe

$(PROJECT).exe:	$(SRCS) Makefile DLLs/Graphics.dll
	$(CS) $(CSFLAGS) $(SRCS) $(DLLS) -lib:$(IPPATH) -out:$@

Config.exe: src/Config.cs
	$(CS) src/Config.cs -out:$@

DLLs/Graphics.dll: src/DLR/Graphics.cs
	$(CS) $(IPPATHLIB) -r:Microsoft.Scripting.Core.dll -target:library -pkg:gtk-sharp-2.0 -pkg:gnome-sharp-2.0 -out:DLLs/Graphics.dll /unsafe -r:IronPython.dll src/DLR/Graphics.cs

clean:
	$(RM) $(PROJECT).exe DLLs/* *~
