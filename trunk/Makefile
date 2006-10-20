site = `python -c 'import sys; print [x for x in sys.path if x[-13:] == "site-packages"][0]'`
zipfile = `python -c 'import myro; print "myro-%s.zip" % myro.__VERSION__' 2> /dev/null`

compile: clean
	python -c 'import compileall; compileall.compile_dir("myro")'

zip: clean
	zip -r myro.zip myro -x myro/CVS/ myro/*/CVS/ myro/CVS/* myro/*/CVS/*

build: zip
	mkdir -p ../html/myro/
	cp myro.zip ../html/myro/$(zipfile)
	cp myro.zip ../html/myro/myro.zip
	cp packages/scribbler.exe ../html/myro/scribbler.exe
	ls -al ../html/myro/
	cd ../html/myro/; ./index.py > index.html

clean:
	rm -f `find | grep "~$$"`
	rm -f `find | grep "\.pyc$$"`
	rm -f myro.zip

install: compile
	cp -r myro $(site)


test:
	echo $(zipfile)
	echo $(site)
