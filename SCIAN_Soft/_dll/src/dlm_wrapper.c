/* Standard Libraries */
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

/* IDL Libraries */
#include "idl_export.h"

typedef IDL_VPTR (*fn)(int, IDL_VPTR *);

IDL_VPTR dlm_wrapper(int argc, IDL_VPTR *argv) {

	IDL_VPTR result;
	char *func_name;
	char lib_file[255];
	char msg[255];
	HMODULE library;
	fn _fn;

	/* The parameters are the shared object (dll) and the function */
	func_name = IDL_VarGetString(argv[0]);
	sprintf(lib_file, "C:\\Users\\Laboratorio\\IDLWorkspace\\s_TimeCalc\\dll\\%s", IDL_VarGetString(argv[1]));

	/* Call dyanmically the library as usual */
	library = LoadLibrary(lib_file);

	if(!library) {
		printf("Library failed to load!\n");
		sprintf(msg, "Error on LoadLibrary: %s\n",lib_file);
		return IDL_StrToSTRING(msg) ;
	}

	_fn = (fn)GetProcAddress(library,func_name);

	if(_fn == NULL) {
		printf("GetProcAddress failed!\n");
		sprintf(msg, "Error on GetProcAddress: %s\n",func_name);
		FreeLibrary(library);
		return IDL_StrToSTRING(msg);
	}

	/* Call the method with the rest of the arguments from IDL */
	result = _fn(argc-2, (argv+2));
	FreeLibrary(library);

	return result;

}

static IDL_SYSFUN_DEF2 main_def[] = {
	{dlm_wrapper, "DLM_WRAPPER", 0, 100, 0, 0}
};

int IDL_Load(void) {
	return IDL_SysRtnAdd(main_def, TRUE, IDL_CARRAY_ELTS(main_def));
}
