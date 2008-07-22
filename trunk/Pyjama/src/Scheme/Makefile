MCS=gmcs

Reader.exe: Reader.cs Scanner.cs
	$(MCS) Reader.cs Scanner.cs -main:Reader

Test.exe: Test.cs
	$(MCS) Test.cs -r:Microsoft.VisualBasic.dll

clean:
	$(RM) *.exe *~
