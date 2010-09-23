
all: ../Conx.dll Conx.exe

MONO_PATH=../../bin

../Conx.dll: Conx.cs
	MONO_PATH=$(MONO_PATH) gmcs -r:IronPython -target:library Conx.cs -out:../Conx.dll 

Conx.exe: Conx.cs
	MONO_PATH=$(MONO_PATH) gmcs -r:IronPython -target:exe Conx.cs -out:Conx.exe

clean::
	rm -rf ../Conx.dll *~ Conx.exe
