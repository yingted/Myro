REM Batch file for making Pyjama on Windows with Mono

REM Needed Gtkrun: C:\GtkRun\2.8\lib\gtksourceview-sharp-2.0

REM Tried: set PKG_CONFIG_PATH=C:\PROGRA~1\MONO-1~1.1\share\pkgconfig

REM This works fine:
REM mcs PyjamaInterfaces.cs -out:PyjamaInterfaces.dll -target:library

REM This did not work:
REM mcs -pkg:gtk-sharp-2.0 -pkg:gtksourceview-sharp-2.0 Pyjama.cs -r:PyjamaInterfaces.dll -r:Mono.Posix.dll -out:pyjama.exe

REM Had to copy gtksourceview-sharp.dll and gtk-sharp.dll to this dir
REM Had to set MONO_PATH to include lib:
gmcs -d:HOSTINGVER1 -recurse:src/*.cs -lib:./lib -r:gtk-sharp.dll -r:gtksourceview-sharp -r:Mono.Posix.dll -r:glib-sharp.dll -r:gdk-sharp.dll -r:pango-sharp.dll -r:IronPython.dll -out:pyjama.exe 

