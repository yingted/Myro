REM Batch file for making Pyjama on Windows with Mono

REM Needed Gtkrun: C:\GtkRun\2.8\lib\gtksourceview-sharp-2.0

REM Tried: set PKG_CONFIG_PATH=C:\PROGRA~1\MONO-1~1.1\share\pkgconfig

REM This works fine:
REM mcs PyjamaInterfaces.cs -out:PyjamaInterfaces.dll -target:library

REM This did not work:
REM mcs -pkg:gtk-sharp-2.0 -pkg:gtksourceview-sharp-2.0 Pyjama.cs -r:PyjamaInterfaces.dll -r:Mono.Posix.dll -out:pyjama.exe

REM Had to copy gtksourceview-sharp.dll and gtk-sharp.dll to this dir
REM Had to set MONO_PATH to include lib:
REM set MONO_PATH="c:\program files\Mono-1.9.1\lib"
REM gmcs -d:HOSTINGVER2 -recurse:src/*.cs -lib:"c:\program files\Mono-1.9.1\lib\" -lib:. -pkg:gtk-sharp-2.0 -pkg:gtksourceview-sharp-2.0 -r:Mono.Posix.dll -pkg:glade-sharp-2.0 -r:gtk-sharp.dll -r:glib-sharp.dll -r:gdk-sharp.dll -r:pango-sharp.dll -r:IronPython.dll -r:Microsoft.Scripting.dll -r:Microsoft.Scripting.Core.dll -out:pyjama.exe 

REM gmcs -target:library src/Testme.cs -out:Testme.dll
REM namespace Testme {
REM    class Doit {
REM    }
REM }

set PKG_CONFIG_PATH="c:\Program Files\Mono-1.9.1\lib\pkgconfig"
set MONO_PATH="c:\program files\Mono-1.9.1\lib"
gmcs -d:HOSTINGVER2 -recurse:src/*.cs -r:gtksourceview-sharp.dll -pkg:gtk-sharp-2.0 -r:glade-sharp.dll -r:Mono.Posix.dll -r:IronPython.dll -r:Microsoft.Scripting.dll -r:Microsoft.Scripting.Core.dll -out:pyjama.exe 


