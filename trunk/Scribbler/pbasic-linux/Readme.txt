The files in this directory make an example program that interfaces with the
PBASIC Tokenizer Shared Library (tokenizer.so)

	stampapp.cpp	- An example application that links in the tokenizer.so
			  library, creates a TModuleRec structure, calls
			  Version, TestRecAlignment and Compile and displays
			  the results of each on standard out.

	tokenizer.h	- An example header file that ensures type sizes and
			  defines the TModuleRec structure.

	makefile	- A make file for GCC.

To make this application:
  1) Copy all these files, plus the tokenizer.so file, into a directory of your 
     choice.

  2) Type: make all
     NOTE: You may have to set your PATH environment variable before making
     the executable, such as by typing: PATH=$PATH:.;export PATH from inside the
     directory you copied the files to.

  3) If the make worked you should have a file called stampapp  (no extension).

  4) Run that program and pipe it to more:

     Type: ./stampapp | more

     to see the results.  You should see the version number of the tokenizer,
     the size of each element within TModuleRec, the returned values from
     calling TestRecAlignment and finally the results of compiling the sample
     source PBASIC program, which is a simple 2-line program:

	'{$STAMP BS2}
	DEBUG "Hello"
