
all: ../Graphics.dll

../Graphics.dll: Graphics.cs
	gmcs -target:library Graphics.cs -out:../Graphics.dll -r:gtk-sharp.dll 

../Graphics0.dll: Graphics0.cs
	gmcs -target:library Graphics0.cs -out:../Graphics0.dll -r:System.Windows.Forms.dll -r:System.Drawing.dll


clean::
	rm -rf ../Graphics.dll *~
