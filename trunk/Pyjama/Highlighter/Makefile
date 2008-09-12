Program: Program.exe
	mono Program.exe

Program.exe: Program.cs TextHighlighter.dll
	gmcs Program.cs -r:System.Windows.Forms.dll -r:System.Drawing.dll \
	-r:TextHighlighter.dll -out:Program.exe

TextHighlighter.dll: Parser.cs  ParseTree.cs  Scanner.cs  TextHighlighter.cs
	gmcs Scanner.cs Parser.cs ParseTree.cs TextHighlighter.cs \
	-r:System.Windows.Forms.dll -r:System.Drawing.dll \
	-target:library -out:TextHighlighter.dll

clean:
	$(RM) *~ TextHighlighter.dll Program.exe
