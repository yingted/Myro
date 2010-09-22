BUILD=0.1.3

all: modules/Graphics.dll modules/Myro.dll languages/PJScheme.dll \
	modules/Conx.dll

build: clean-build all 
	zip -r Pyjama-$(BUILD).zip * -x \*/.svn/\* \*~ Makefile \
		languages/Scheme/\* modules/Myro/\* modules/Graphics/\*

modules/Graphics.dll:
	cd modules/Graphics; make

modules/Conx.dll:
	cd modules/Conx; make

modules/Myro.dll: 
	cd modules/Myro; make ../Myro.dll

languages/PJScheme.dll:
	cd languages/Scheme; make

clean-build:
	rm -f Pyjama-*.zip *~

clean:
	rm -f modules/Graphics.dll modules/Myro.dll \
		languages/PJScheme.dll Pyjama*.zip