
all: ../Graphics.dll

MONO_PATH=/usr/lib/cli/gtk-sharp-2.0:/usr/lib/cli/gdk-sharp-2.0:/usr/lib/cli/pango-sharp-2.0:/usr/lib/mono/2.0/

../Graphics.dll: Graphics.cs
	MONO_PATH=$(MONO_PATH) gmcs -target:library Polygon.cs Vector.cs Collision.cs Graphics.cs -out:../Graphics.dll -r:gtk-sharp.dll -r:gdk-sharp.dll -r:pango-sharp.dll -r:Mono.Cairo.dll -r:mscorlib.dll

../Graphics0.dll: Graphics0.cs
	gmcs -target:library Graphics0.cs -out:../Graphics0.dll -r:System.Windows.Forms.dll -r:System.Drawing.dll


clean::
	rm -rf ../Graphics.dll *~
