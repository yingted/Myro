
all: ../Graphics.dll

../Graphics.dll: Graphics.cs
	gmcs -target:library Graphics.cs -out:Graphics.dll -r:System.Windows.Forms.dll -r:System.Drawing.dll

clean::
	rm -rf ../Graphics.dll *~
