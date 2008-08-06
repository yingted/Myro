MCS=gmcs

pjscheme-rm.ss: environments-cps.ss parser-cps.ss unifier-cps.ss interpreter-cps.ss \
		reader-cps.ss ds-transformer.ss rm-transformer.ss pjscheme-cps.ss
	cat *cps.ss > pjscheme.ss
	echo "(load \"rm-transformer.ss\")" > compile.ss
	echo "(compile-level-output)" >> compile.ss
	echo "(compile \"pjscheme\")" >> compile.ss
	echo "(exit)" >> compile.ss
	petite compile.ss

run: pjscheme-rm.ss 
	echo "(load \"pjscheme-rm.ss\")" > pjscheme-run.ss
	echo "(begin (display (run start)) (newline))" >> pjscheme-run.ss
	petite pjscheme-run.ss

pjscheme.exe: pjscheme-rm.ss pjscheme-cps.ss scheme-to-csharp.ss
	echo "(load \"scheme-to-csharp.ss\")" > make-pjscheme.ss
	echo "(delete-file \"pjscheme.cs\")" >> make-pjscheme.ss
	echo "(convert-file \"pjscheme-rm.ss\" \"pjscheme.cs\")" >> make-pjscheme.ss
	echo "(exit)" >> make-pjscheme.ss
	petite make-pjscheme.ss
	indent -di0 -i3 pjscheme.cs
	$(MCS) pjscheme.cs Util.cs -r:Microsoft.VisualBasic.dll
	mono pjscheme.exe

Reader.exe: Reader.cs Scanner.cs
	$(MCS) Reader.cs Scanner.cs -main:Reader

Test.exe: Test.cs
	$(MCS) Test.cs -r:Microsoft.VisualBasic.dll

fact.exe: fact.ss Util.cs
	echo "(load \"rm-transformer.ss\")" > make-fact.ss
	echo "(compile \"fact\")" >> make-fact.ss
	echo "(delete-file \"fact.cs\")" >> make-fact.ss
	echo "(load \"scheme-to-csharp.ss\")" >> make-fact.ss
	echo "(convert-file \"fact-rm.ss\" \"fact.cs\")" >> make-fact.ss
	echo "(exit)" >> make-fact.ss
	petite make-fact.ss
	indent -di0 -i3 fact.cs
	$(MCS) fact.cs Util.cs -r:Microsoft.VisualBasic.dll
	mono fact.exe

clean:
	$(RM) *.exe *~ pjscheme-ds.ss pjscheme-rm.ss compile.ss make-fact.ss \
		fact.cs make-pjscheme.ss pjscheme.exe pjscheme.cs pjscheme-run.ss

