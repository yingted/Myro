site = `python -c 'import sys; print [x for x in sys.path if x[-13:] == "site-packages"][0]'`
zipfile = `python -c 'import myro; print "myro-%s.zip" % myro.__VERSION__' 2> /dev/null`

MYRO_DOWNLOAD=../html/myro

compile: clean
	python -c 'import compileall; compileall.compile_dir("myro")'

zip: clean
	zip -r myro.zip *.py install.bat "Start Python.py" myro misc -x myro/.svn/ myro/.svn/* myro/*/.svn/* myro/.svn/* myro/*/.svn/* misc/.svn/* misc/.svn/ misc/.svn/*/* myro/robots/.svn/ myro/globvars/.svn/ myro/worlds/.svn/ misc/.svn/*/* misc/.svn/*/ misc/.svn/ misc/.svn/*/*/ myro/.svn/*/* myro/.svn/*/*/ myro/.svn/*/ myro/*/.svn/* myro/*/.svn/*/*/ myro/*/.svn/*/*/* myro/*/.svn/*/* myro/*/.svn/*/


build: zip
	mkdir -p $(MYRO_DOWNLOAD)/
	cp myro.zip $(MYRO_DOWNLOAD)/$(zipfile)
	cp myro.zip $(MYRO_DOWNLOAD)/myro.zip
	cp packages/scribbler.exe $(MYRO_DOWNLOAD)/scribbler.exe
	cp Scribbler/ScribblerAPI.html $(MYRO_DOWNLOAD)/docs/
	ls -al $(MYRO_DOWNLOAD)/
	cd $(MYRO_DOWNLOAD)/; ./index.py > index.html

clean:
	rm -f `find | grep "~$$"`
	rm -f `find | grep "\.pyc$$"`
	rm -f `find | grep "^\.\#"`
	rm -f myro.zip

install: compile
	cd myro; python setup.py install


test:
	echo $(zipfile)
	echo $(site)
