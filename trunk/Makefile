site = `python -c 'import sys; print [x for x in sys.path if x[-13:] == "site-packages"][0]'`

zip:
	zip -r myro.zip myro -x myro/CVS/ myro/*/CVS/ myro/CVS/* myro/*/CVS/*

clean:
	rm -f `find | grep "~$$"`
	rm -f `find | grep "\.pyc$$"`
	rm -f myro.zip

install:
	cp -r myro $(site)

