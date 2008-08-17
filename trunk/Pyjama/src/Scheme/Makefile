MCS=gmcs

pjscheme-rm.ss: environments-cps.ss parser-cps.ss unifier-cps.ss \
		interpreter-cps.ss reader-cps.ss ds-transformer.ss \
		rm-transformer.ss pjscheme-data.ss
	$(RM) pjscheme-cps.ss
	cat *cps.ss > pjscheme.ss
	cat pjscheme-data.ss >> pjscheme.ss
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
	$(MCS) pjscheme.cs Util.cs -main:pjscheme_rm \
		-r:Microsoft.VisualBasic.dll
	mono pjscheme.exe

Reader.exe: Reader.cs Scanner.cs
	$(MCS) Reader.cs Scanner.cs -main:Reader

Test.exe: Test.cs
	$(MCS) Test.cs -r:Microsoft.VisualBasic.dll

make-fact.ss: scheme-to-csharp.ss 
	echo "(load \"rm-transformer.ss\")" > make-fact.ss
	echo "(compile \"fact\")" >> make-fact.ss
	echo "(delete-file \"fact.cs\")" >> make-fact.ss
	echo "(load \"scheme-to-csharp.ss\")" >> make-fact.ss
	echo "(convert-file \"fact-rm.ss\" \"fact.cs\")" >> make-fact.ss
	echo "(exit)" >> make-fact.ss

make-reader.ss: scheme-to-csharp.ss 
	echo "(load \"rm-transformer.ss\")" > make-reader.ss
	echo "(delete-file \"reader-ds.ss\")" >> make-reader.ss
	echo "(ds-transform-file \"reader-cps.ss\" \"reader-ds.ss\")" >> make-reader.ss
	echo "(delete-file \"reader-rm.ss\")" >> make-reader.ss
	echo "(rm-transform-file \"reader-ds.ss\" \"reader-rm.ss\")" >> make-reader.ss
	echo "(delete-file \"reader.cs\")" >> make-reader.ss
	echo "(load \"scheme-to-csharp.ss\")" >> make-reader.ss
	echo "(convert-file \"reader-rm.ss\" \"reader.cs\")" >> make-reader.ss
	echo "(exit)" >> make-reader.ss

all: environments-cps.ss parser-cps.ss unifier-cps.ss interpreter-cps.ss reader-cps.ss \
		ds-transformer.ss rm-transformer.ss
	cat environments-cps.ss unifier-cps.ss reader-cps.ss parser-cps.ss interpreter-cps.ss > all.ss
	echo "(load \"rm-transformer.ss\")" > make-all.ss
	echo "(ds-transform-file \"all.ss\" \"all-ds.ss\")" >> make-all.ss
	echo "(compile-level-output)" >> make-all.ss
	echo "(rm-transform-file \"all-ds.ss\" \"all-rm.ss\")" >> make-all.ss
	echo "(low-level-output)" >> make-all.ss
	echo "(rm-transform-file \"all-ds.ss\" \"all-rm-low.ss\")" >> make-all.ss
	echo "(high-level-output)" >> make-all.ss
	echo "(rm-transform-file \"all-ds.ss\" \"all-rm-high.ss\")" >> make-all.ss
	echo "(exit)" >> make-all.ss
	petite make-all.ss
	$(RM) make-all.ss

testds: all-ds.ss
	echo "(load \"all-ds.ss\")" > testall.ss
	echo "(testall)" >> testall.ss
	echo "(exit)" >> testall.ss
	petite testall.ss
	$(RM) testall.ss

testrm: all-rm.ss all-rm-low.ss all-rm-high.ss
	echo "(load \"all-rm.ss\")" > testall.ss
	echo "(run testall)" >> testall.ss
	echo "(exit)" >> testall.ss
	petite testall.ss
	sleep 1
	echo "(load \"all-rm-low.ss\")" > testall.ss
	echo "(run testall)" >> testall.ss
	echo "(exit)" >> testall.ss
	petite testall.ss
	sleep 1
	echo "(load \"all-rm-high.ss\")" > testall.ss
	echo "(run testall)" >> testall.ss
	echo "(exit)" >> testall.ss
	petite testall.ss
	$(RM) testall.ss

fact.cs: fact.ss Util.cs make-fact.ss rm-transformer.ss
	petite make-fact.ss
	indent -di0 -i3 fact.cs
	$(MCS) fact.cs Util.cs -main:fact_rm -r:Microsoft.VisualBasic.dll

reader.exe: reader-cps.ss Util.cs make-reader.ss rm-transformer.ss
	petite make-reader.ss
	indent -di0 -i3 reader.cs
	$(MCS) reader.cs Util.cs -main:reader_rm -r:Microsoft.VisualBasic.dll

util.exe: Util.cs
	$(MCS) Util.cs -r:Microsoft.VisualBasic.dll -out:util.exe

dummy:

run-util: util.exe dummy
	mono util.exe

util: run-util

run-fact.exe:
	mono fact.exe

fact.exe: fact.cs run-fact.exe

clean:
	$(RM) *.exe *~ pjscheme-ds.ss pjscheme-rm.ss compile.ss make-fact.ss \
		fact.cs make-pjscheme.ss pjscheme.exe pjscheme.cs \
		pjscheme-run.ss pjscheme.ss fact-rm.ss fact-ds.ss all*.ss testall.ss

