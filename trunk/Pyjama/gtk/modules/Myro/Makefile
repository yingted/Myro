
all: ../Myro.dll

MONO_PATH=../../bin

../Myro.dll: Myro.cs
	MONO_PATH=$(MONO_PATH) gmcs -target:library Myro.cs -r:IronPython -out:../Myro.dll

clean::
	rm -rf ../Myro.dll */*~ *~
