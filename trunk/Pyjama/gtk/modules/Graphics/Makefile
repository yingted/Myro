
all: ../Graphics.dll

MONO_PATH=/usr/lib/cli/pango-sharp-2.0:/usr/lib/mono/2.0/:/usr/lib/mono/gtk-sharp-2.0/:/usr/lib/cli/gtk-sharp-2.0/:/usr/lib/cli/gdk-sharp-2.0/

# Polygon.cs Vector.cs Collision.cs
../Graphics.dll: Graphics.cs
	MONO_PATH=$(MONO_PATH) gmcs -target:library Graphics.cs -out:../Graphics.dll -r:gtk-sharp.dll -r:gdk-sharp.dll -r:pango-sharp.dll -r:Mono.Cairo.dll 

clean::
	rm -rf ../Graphics.dll *~
