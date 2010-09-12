

all: modules/Graphics.dll modules/Myro.dll languages/PJScheme.dll

modules/Graphics.dll:
	cd modules/Graphics; make

modules/Myro.dll: 
	cd modules/Myro; make ../Myro.dll

languages/PJScheme.dll:
	cd languages/Scheme; make



clean:
	rm -f modules/Graphics.dll modules/Myro.dll languages/PJScheme.dll