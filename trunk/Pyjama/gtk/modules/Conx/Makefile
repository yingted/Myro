
all: ../Conx.dll Conx.exe

../Conx.dll: Conx.cs
	MONO_PATH=$(MONO_PATH) gmcs -target:library Conx.cs -out:../Conx.dll 

Conx.exe: Conx.cs
	MONO_PATH=$(MONO_PATH) gmcs -target:exe Conx.cs -out:Conx.exe

clean::
	rm -rf ../Conx.dll *~ Conx.exe
