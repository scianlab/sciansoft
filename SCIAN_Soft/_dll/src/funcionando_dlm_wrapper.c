/* Standard Libraries */
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

/* IDL Libraries */
#include "idl_export.h"

typedef IDL_VPTR (*fn)(int, IDL_VPTR *);
typedef char* (*sdl_init_function_type)(char *, char *);


IDL_VPTR dlm_wrapper(int argc, IDL_VPTR *argv) {


	//IDL_VPTR (*fn)(int, IDL_VPTR *);
	IDL_VPTR result;
	char *func_name;
	char *lib_file;
	char msg[255];
	char msg2[255];
	HMODULE library;
	fn _fn;
	sdl_init_function_type _sdl_init_function_type;
	HMODULE sdl_library = LoadLibrary("C:\\Documents and Settings\\Usuario\\Mis documentos\\Visual Studio 2008\\Projects\\First\\Release\\First.dll");

	if(sdl_library == NULL) {
	   return IDL_StrToSTRING("Library failed to load!\n");
	} else {
		_sdl_init_function_type = (sdl_init_function_type)GetProcAddress(sdl_library,"RetrieveTempFilename");

		if(_sdl_init_function_type == NULL) {
			return IDL_StrToSTRING("GetProcAddress failed!\n");
		}
		else {
			result = IDL_StrToSTRING(_sdl_init_function_type("C:\\Temp", "tmp"));
			FreeLibrary(sdl_library);
			return result;
		}
	}


	/* The parameters are the shared object (dll) and the function */
	func_name = IDL_VarGetString(argv[0]);
	lib_file = IDL_VarGetString(argv[1]);

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
		FreeLibrary(library);
		return IDL_StrToSTRING("Error on GetProcAddress\n");
	}

	//sprintf(msg, "Antes de llamar a la funcion\n",func_name);

	/* Call the method with the rest of the arguments from IDL */
	_fn(argc-2, (argv+2));
	//result  = (*_fn)(argc-2, (argv+2));
	//FreeLibrary(library);

	return result;

}

static IDL_SYSFUN_DEF2 main_def[] = {
	{dlm_wrapper, "DLM_WRAPPER", 0, 100, 0, 0}
};

int IDL_Load(void) {
	return IDL_SysRtnAdd(main_def, TRUE, IDL_CARRAY_ELTS(main_def));
}
