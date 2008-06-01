
#PATH=/opt/mono-1.9/bin:$PATH
MONO_PATH?=/opt/mono-1.9/lib/mono/

SRC=Expression.cs Program.cs Env.cs Hashlist.cs Primitive.cs Util.cs Repl.cs


scheme.exe: $(SRC)
	gmcs $(SRC) -r:Microsoft.VisualBasic.dll -r:IronPython.dll -r:IronPython.Modules.dll -r:Microsoft.Scripting.dll -r:Microsoft.Scripting.Core.dll -out:scheme.exe 

CsharpClass1.dll: Class1.cs
	gmcs Class1.cs -target:library -out:CsharpClass1.dll

clean:
	$(RM) scheme.exe CsharpClass1.dll *~
