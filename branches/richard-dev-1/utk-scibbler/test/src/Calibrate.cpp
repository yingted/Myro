#include <iostream>
#include <python2.4/Python.h>

#define IMPORT "from myro import *"

int main(int argc, char ** argv) {

	Py_Initialize();

	char * init;

	if(argc < 2) {
		init = (char*)malloc(sizeof(char) * strlen("initialize()")
				+ 1);
		strcpy(init, "initialize()");
		*(init + strlen("initialize()")) = 0;
	}
	else {
		init = (char*)malloc(sizeof(char) * strlen("initialize(\"\")")
				+ strlen(argv[1]) + 1);
		strcpy(init, "initialize(\"");
		strcpy(init + strlen("initialize(\""), argv[1]);
		strcpy(init + strlen("initialize(\"") + strlen(argv[1]), "\")");
		*(init + strlen("initialize(\"\")") + strlen(argv[1])) = 0;
	}

	printf("Init Command %s\n", init);
	PyRun_SimpleString(IMPORT);
	PyRun_SimpleString(init);

	Py_Finalize();
}
