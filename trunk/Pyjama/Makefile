PROJECT=pyjama

#PATH=/opt/mono-1.9.1/bin

DLLS=-lib:DLLs -r:Mono.Posix.dll -r:IronPython.dll -r:IronPython.Modules.dll \
	-r:Microsoft.Scripting.dll -r:Microsoft.Scripting.Core.dll \
	-r:System.Windows.Forms.dll -r:System.Drawing.dll

SRCS=src/Pyjama.cs src/PyjamaInterfaces.cs src/Utils.cs \
	src/TextDocument.cs src/Shell.cs \
	src/PythonShell.cs src/PyjamaGUI.cs 
CS=gmcs
MONO=mono

.PHONY: all

all:	$(PROJECT).exe

run:	$(PROJECT).exe
	MONO_PATH=DLLs $(MONO) ./$(PROJECT).exe

$(PROJECT).exe:	$(SRCS) Makefile 
	$(CS) $(DEFINES) $(SRCS) $(DLLS) -lib:DLLs -main:Pyjama -out:$@

Config.exe: src/Config.cs
	$(CS) src/Config.cs -out:$@

DLLs/Graphics.dll: src/DLR/Graphics.cs
	$(CS) -r:Microsoft.Scripting.Core.dll -target:library -pkg:gtk-sharp-2.0 -pkg:gnome-sharp-2.0 \
		-out:DLLs/Graphics.dll /unsafe -r:IronPython.dll src/DLR/Graphics.cs

clean:
	$(RM) $(PROJECT).exe DLLs/Graphics.dll *~
