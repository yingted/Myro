#-- Set LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/. before running programs --#
#   This makefile generates the stampapp executable.
#
#   The stampapp finds the library tokenizer.so at runtime
#   to call the TestRecAlignment, Version and Compile functions. 
#
#   libraries used
#      -ldl         libdl.so  dynamic load library
#      -lc          libc.so   C library
#      -ltokenizer  tokenizer.so  tokenizer library
#

CC = gcc

all: stampapp

clean:
	rm -f *.o *.bak stampapp

stampapp: stampapp.o
	$(CC) -g -fpic -DLINUX -o stampapp stampapp.o -ldl -lc -lstdc++

#-------------------------------------------------------#
# unix specific automatic dependencies

$(OBJS):
	$(CC) -g -c -fpic -DLINUX $*.cpp -o $@

.DEFAULT:

################################################################################
#
# Include the default behavior at the eend so as not to 
# replace the default target above if any.
#




