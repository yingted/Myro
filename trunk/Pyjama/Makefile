PROJECT=pyjama
HOSTINGVER=1

# Where the IP dlls are:
#IPPATH=/opt/mono-1.9/lib/IPCE/ms
IPPATH?=/opt/mono-1.9/lib/IPCE/ipy
#IPPATH=/opt/mono-1.9/lib/IPCE/ipy2

# Where site.py and other bits are:
IPSITE=/opt/mono-1.9/lib/IPCE/Lib

DLLS=-r:Mono.Posix.dll -r:IronPython.dll -r:IronMath.dll
#DLLS=-r:Mono.Posix.dll -r:IronPython.dll -r:IronPython.Modules.dll \
	-r:Microsoft.Scripting.dll 
#DLLS=-r:Mono.Posix.dll -r:IronPython.dll -r:IronPython.Modules.dll \
	-r:Microsoft.Scripting.dll -r:Microsoft.Scripting.Core.dll 

SRCS=src/Pyjama.cs src/PyjamaInterfaces.cs src/Utils.cs \
	src/TextDocument.cs \
	src/PythonShell.cs src/PyjamaGUI.cs
CSFLAGS=-d:HOSTINGVER$(HOSTINGVER) -pkg:gtk-sharp-2.0 -pkg:gtksourceview-sharp-2.0 -pkg:glade-sharp-2.0
CS=gmcs
MONO=mono

.PHONY: all

all:	$(PROJECT).exe

run:	$(PROJECT).exe
	MONO_PATH=$(IPPATH) IRONPYTHON_PATH=$(IPSITE) $(MONO) ./$(PROJECT).exe

$(PROJECT).exe:	$(SRCS) Makefile
	$(CS) $(CSFLAGS) $(SRCS) $(DLLS) -lib:$(IPPATH) -out:$@

clean:
	$(RM) $(PROJECT).exe *.dll *~

