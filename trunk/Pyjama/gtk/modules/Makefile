
all: Myro.dll

Myro.dll: Myro/Myro.cs
	gmcs -target:library Myro/Myro.cs -out:Myro.dll

clean::
	rm -rf Myro.dll */*~ *~
