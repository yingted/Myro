PROJECT=pyjama
SRCS=Pyjama.cs PyjamaInterfaces.cs Utils.cs TextDocument.cs
DLLS=-r:Mono.Posix.dll
CSFLAGS=-pkg:gtk-sharp-2.0 -pkg:gtksourceview-sharp-2.0
MCS=mcs
MONO=mono

.PHONY: all

all:	$(PROJECT).exe

run:	$(PROJECT).exe
	$(MONO) ./$(PROJECT).exe

#PyjamaInterfaces.dll: PyjamaInterfaces.cs
#	$(MCS) PyjamaInterfaces.cs -out:PyjamaInterfaces.dll -target:library

$(PROJECT).exe:	$(SRCS) # PyjamaInterfaces.dll
	$(MCS) $(CSFLAGS) $(SRCS) $(DLLS) -out:$@

clean:
	@$(RM) $(PROJECT).exe *.dll *~

