top_srcdir=..

pre:
#	intltool-update --pot -g pyjama
	xgettext -a --from-code=UTF-8 ../src/*.cs -o de.po

#init:
#	msginit -l de --output-file=de.po

post:
	mkdir -p ../locale/de/LC_MESSAGES/
	msgfmt de.po -o ../locale/de/LC_MESSAGES/pyjama.mo

run:
	LANG=de_DE mono ../pyjama.exe
#       LANGUAGE=de mono ../pyjama.exe

clean:	
	$(RM) *~

clean-dist:
	$(RM) *.po
