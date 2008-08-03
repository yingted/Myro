MCS=gmcs


pjscheme.ss: environments-cps.ss parser-cps.ss unifier-cps.ss interpreter-cps.ss reader-cps.ss
	cat *cps.ss > pjscheme.ss
	echo "(load \"rm-transformer.ss\")(ds-transform-file \"pjscheme.ss\" \"pjscheme-ds.ss\")(reg-transform-file \"pjscheme-ds.ss\" \"pjscheme-reg.ss\")(exit)" > compile.ss
	petite compile.ss
	echo "(run start)" >> pjscheme-reg.ss

run: pjscheme-reg.ss
	petite pjscheme-reg.ss


Reader.exe: Reader.cs Scanner.cs
	$(MCS) Reader.cs Scanner.cs -main:Reader

Test.exe: Test.cs
	$(MCS) Test.cs -r:Microsoft.VisualBasic.dll

clean:
	$(RM) *.exe *~ pjscheme*.ss compile.ss

