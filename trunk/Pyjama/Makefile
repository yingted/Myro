PROJECT=pyjama
SRCS=Pyjama.cs
DLLS=-r:PyjamaInterfaces.dll
CSFLAGS=-pkg:gtk-sharp-2.0 -pkg:gtksourceview-sharp-2.0
MCS=mcs
MONO=mono

.PHONY: all

all:	$(PROJECT).exe

clean:
	@$(RM) -f $(PROJECT).exe *~

run:	$(PROJECT).exe
	$(MONO) ./$(PROJECT).exe

PyjamaInterfaces.dll: PyjamaInterfaces.cs
	$(MCS) PyjamaInterfaces.cs -out:PyjamaInterfaces.dll -target:library

$(PROJECT).exe:	$(SRCS) PyjamaInterfaces.dll
	$(MCS) $(CSFLAGS) $(SRCS) $(DLLS) -out:$@
