CC = g++
INCLUDES = ../headers
CFLAGS = -g -Wall -I$(INCLUDES)
EXECUTABLES = panoramic

all: panoramic 

clean:
	rm -f core $(EXECUTABLES) *.o

.SUFFIXES: .cpp .o
#.cpp.o:
#	$(CC) $(CFLAGS) -c src/*.cpp


panoramic: panoramic.cpp
	$(CC) $(CFLAGS) -c panoramic.cpp
	$(CC) $(CFLAGS) -o panoramic panoramic.cpp ../libscribbler.a -lMagick++
