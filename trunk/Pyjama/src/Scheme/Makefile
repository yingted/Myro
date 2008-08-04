MCS=gmcs


pjscheme-rm.ss: environments-cps.ss parser-cps.ss unifier-cps.ss interpreter-cps.ss reader-cps.ss ds-transformer.ss rm-transformer.ss
	cat *cps.ss > pjscheme.ss
	echo "(load \"rm-transformer.ss\")(compile \"pjscheme\")(exit)" > compile.ss
	petite compile.ss
	echo "(run start)" >> pjscheme-rm.ss

run: pjscheme-rm.ss
	petite pjscheme-rm.ss


Reader.exe: Reader.cs Scanner.cs
	$(MCS) Reader.cs Scanner.cs -main:Reader

Test.exe: Test.cs
	$(MCS) Test.cs -r:Microsoft.VisualBasic.dll

clean:
	$(RM) *.exe *~ pjscheme*.ss compile.ss

