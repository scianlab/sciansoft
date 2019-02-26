/* Standard Libraries */
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

/* IDL Libraries */
#include "idl_export.h"

IDL_VPTR dlm_wrapper(int argc, IDL_VPTR *argv) {

	void *lib_handle;
	IDL_VPTR (*fn)(int, IDL_VPTR *);
	IDL_VPTR result;
	char *func_name;
	char *lib_file;

	/* The parameters are the shared object (dll) and the function */
	func_name = IDL_VarGetString(argv[0]);
	lib_file = IDL_VarGetString(argv[1]);

	/* Call dyanmically the library as usual */
	lib_handle = dlopen(lib_file, RTLD_LAZY);

	if(!lib_handle) {
		printf("%s\n", dlerror());
		return IDL_StrToSTRING("Error on lib_name\n") ;
	}

	fn = dlsym(lib_handle, func_name);
	if((dlerror() != NULL)) {
		printf("%s\n", dlerror());
		dlclose(lib_handle);
		return IDL_StrToSTRING("Error on func_name\n");
	}


	/* Call the method with the rest of the arguments from IDL */
	result  = (*fn)(argc-2, (argv+2));
	dlclose(lib_handle);

	return result;
	
}

static IDL_SYSFUN_DEF2 main_def[] = { 
	{dlm_wrapper, "DLM_WRAPPER", 0, 100, 0, 0}
};

int IDL_Load(void) {
	return IDL_SysRtnAdd(main_def, TRUE, IDL_CARRAY_ELTS(main_def));
}

