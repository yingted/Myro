MCS=gmcs

Scheme.dll: pjscheme.exe
	$(MCS) Scheme.cs pjscheme.cs -target:library -lib:../../bin -r:Microsoft.VisualBasic.dll -r:Microsoft.Scripting.dll -out:../PJScheme.dll

pjscheme: pjscheme.exe
	MONO_PATH=../../bin mono --debug pjscheme.exe

pjscheme-ds.ss: environments-cps.ss parser-cps.ss unifier-cps.ss \
		interpreter-cps.ss reader-cps.ss ds-transformer.ss
	$(RM) pjscheme-cps.ss
	cat reader-cps.ss environments-cps.ss parser-cps.ss interpreter-cps.ss unifier-cps.ss > pjscheme-cps.ss
	echo "(load \"ds-transformer.ss\")" > compile-ds.ss
	echo "(delete-file \"pjscheme-ds.ss\")" >> compile-ds.ss
	echo "(ds-transform-file \"pjscheme-cps.ss\" \"pjscheme-ds.ss\")" >> compile-ds.ss
	echo "(exit)" >> compile-ds.ss
	petite compile-ds.ss

pjscheme-rm.ss: pjscheme-ds.ss
	echo "(load \"rm-transformer.ss\")" > compile-rm.ss
	echo "(compile-level-output)" >> compile-rm.ss
	echo "(delete-file \"pjscheme-rm.ss\")" >> compile-rm.ss
	echo "(rm-transform-file \"pjscheme-ds.ss\" \"pjscheme-rm.ss\")" >> compile-rm.ss
	echo "(exit)" >> compile-rm.ss
	petite compile-rm.ss

run: pjscheme-rm.ss 
	echo "(load \"pjscheme-rm.ss\")" > pjscheme-run.ss
	echo "(begin (display (run start)) (newline))" >> pjscheme-run.ss
	petite pjscheme-run.ss

pjscheme.exe: pjscheme-rm.ss scheme-to-csharp.ss Scheme.cs
	echo "(load \"scheme-to-csharp.ss\")" > make-pjscheme.ss
	echo "(delete-file \"pjscheme.cs\")" >> make-pjscheme.ss
	echo "(convert-file \"pjscheme-rm.ss\" \"pjscheme.cs\")" >> make-pjscheme.ss
	echo "(exit)" >> make-pjscheme.ss
	petite make-pjscheme.ss
	indent -di0 -i3 pjscheme.cs
	$(MCS) -debug pjscheme.cs Scheme.cs getline.cs -main:PJScheme \
		-lib:../../bin -r:Microsoft.VisualBasic.dll -r:Microsoft.Scripting.dll

Test.exe: Test.cs
	$(MCS) Test.cs -lib:../../bin -r:Microsoft.VisualBasic.dll -r:Microsoft.Scripting.dll

make-fact.ss: scheme-to-csharp.ss 
	echo "(load \"rm-transformer.ss\")" > make-fact.ss
	echo "(compile \"fact\")" >> make-fact.ss
	echo "(delete-file \"fact.cs\")" >> make-fact.ss
	echo "(load \"scheme-to-csharp.ss\")" >> make-fact.ss
	echo "(convert-file \"fact-rm.ss\" \"fact.cs\")" >> make-fact.ss
	echo "(exit)" >> make-fact.ss

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

fact.cs: fact.ss Scheme.cs getline.cs make-fact.ss rm-transformer.ss
	petite make-fact.ss
	indent -di0 -i3 fact.cs

fact: fact.exe
	MONO_PATH=../../bin mono fact.exe

fact.exe: fact.cs
	$(MCS) fact.cs Scheme.cs getline.cs -main:PJScheme -lib:../../bin -r:Microsoft.VisualBasic.dll -r:Microsoft.Scripting.dll

Scheme.exe: Scheme.cs fact.cs
	$(MCS) Scheme.cs fact.cs getline.cs -main:Scheme -lib:../../bin -r:Microsoft.VisualBasic.dll -r:Microsoft.Scripting.dll
	MONO_PATH=../../bin mono Scheme.exe

DLR.exe: DLR.cs
	gmcs DLR.cs -lib:../../bin -r:Microsoft.Scripting.dll -r:Microsoft.Scripting.Core.dll

External.dll: External.cs
	$(MCS) External.cs -target:library

clean:
	$(RM) *.exe *~ pjscheme-ds.ss pjscheme-rm.ss compile-rm.ss make-fact.ss \
		fact.cs make-pjscheme.ss pjscheme.exe pjscheme.cs pjscheme-cps.ss \
		pjscheme-run.ss pjscheme.ss fact-rm.ss fact-ds.ss all*.ss testall.ss compile-dm.ss

