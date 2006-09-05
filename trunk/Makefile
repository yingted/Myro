site = `python -c 'import sys; print [x for x in sys.path if x[-13:] == "site-packages"][0]'`

zip:
	zip -r myro.zip myro

clean:
	rm -f `find | grep "~$"`

install:
	cp -r myro $(site)

