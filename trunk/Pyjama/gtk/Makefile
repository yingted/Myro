

all: modules/Graphics.dll modules/Myro.dll languages/PJScheme.dll

download: all
	zip -r Pyjama-0.1.0.zip * -x \*/.svn/\* \*~ Makefile languages/Scheme/\* modules/Myro/\* modules/Graphics/\*


modules/Graphics.dll:
	cd modules/Graphics; make

modules/Myro.dll: 
	cd modules/Myro; make ../Myro.dll

languages/PJScheme.dll:
	cd languages/Scheme; make



clean:
	rm -f modules/Graphics.dll modules/Myro.dll languages/PJScheme.dll Pyjama*.zip