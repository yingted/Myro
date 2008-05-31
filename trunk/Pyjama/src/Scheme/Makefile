
#PATH=/opt/mono-1.9/bin:$PATH
MONO_PATH?=/opt/mono-1.9/lib/mono/

SRC=Expression.cs Program.cs Env.cs Hashlist.cs Primitive.cs \
    Util.cs Repl.cs


scheme.exe: $(SRC)
	gmcs $(SRC) -r:Microsoft.VisualBasic.dll -r:System.Windows.Forms.dll \
             -out:scheme.exe

clean:
	$(RM) scheme.exe *~
