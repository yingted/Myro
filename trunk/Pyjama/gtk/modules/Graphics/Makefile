
all: ../Graphics.dll

../Graphics.dll: Graphics.cs
	gmcs -target:library Graphics.cs -out:../Graphics.dll -r:gtk-sharp.dll

clean::
	rm -rf ../Graphics.dll *~
