PROJECT=pyjama
IPPATH=/opt/mono-1.9/lib/IPCE/ipy
SRCS=src/Pyjama.cs src/PyjamaInterfaces.cs src/Utils.cs \
	src/TextDocument.cs src/PyjamaGraphics.cs \
	src/PythonShell.cs
DLLS=-r:Mono.Posix.dll -r:IronPython.dll
CSFLAGS=-pkg:gtk-sharp-2.0 -pkg:gtksourceview-sharp-2.0
CS=gmcs
MONO=mono

.PHONY: all

all:	$(PROJECT).exe

run:	$(PROJECT).exe
	$(MONO) ./$(PROJECT).exe

#PyjamaInterfaces.dll: PyjamaInterfaces.cs
#	$(MCS) PyjamaInterfaces.cs -out:PyjamaInterfaces.dll -target:library

IronPython.dll: $(IPPATH)/IronPython.dll
	cp $(IPPATH)/IronPython.dll .

IronMath.dll: $(IPPATH)/IronMath.dll
	cp $(IPPATH)/IronMath.dll .

$(PROJECT).exe:	$(SRCS) IronPython.dll IronMath.dll
	$(CS) $(CSFLAGS) $(SRCS) $(DLLS) -out:$@

clean:
	$(RM) $(PROJECT).exe *.dll *~

