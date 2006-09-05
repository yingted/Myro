site = `python -c 'import sys; print [x for x in sys.path if x[-13:] == "site-packages"][0]'`
zipfile = `python -c 'import myro; print "myro-%s.zip" % myro.__VERSION__' 2> /dev/null`

zip:
	zip -r myro.zip myro -x myro/CVS/ myro/*/CVS/ myro/CVS/* myro/*/CVS/*
	mkdir -p ../html/myro/
	cp myro.zip ../html/myro/$(zipfile)
	cp myro.zip ../html/myro/myro.zip
	ls -al ../html/myro/

clean:
	rm -f `find | grep "~$$"`
	rm -f `find | grep "\.pyc$$"`
	rm -f myro.zip

install:
	python -c 'import compileall; compileall.compile_dir("myro")'
	cp -r myro $(site)

