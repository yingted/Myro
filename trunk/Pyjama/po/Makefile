top_srcdir=..

pre:
	xgettext -a --from-code=UTF-8 ../src/*.cs -o pyjama.pot

# for de:
init:
	msginit -l de --output-file=de.po

# for de:
post:
	mkdir -p ../locale/de/LC_MESSAGES/
	msgfmt de.po -o ../locale/de/LC_MESSAGES/pyjama.mo

# testing:
run:
#	LANG=de_DE mono ../pyjama.exe
	LANGUAGE=de mono ../pyjama.exe

clean:	
	$(RM) *~

