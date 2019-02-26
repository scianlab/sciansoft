//You need to declare types to point on classes/functions in LoadMe.dll
//Assume, you have a function in your LoadMe.dll with a name
//EntryPoint, which takes two parameters of types int and const char *,
//and is of type void. You need to create a new type as a
//pointer to that function as it is shown below.

typedef void (*EntryPointfuncPtr)(int argc, const char * argv );

//Declare an HINSTANCE and load the library dynamically. Don’t forget
//to specify a correct path to the location of LoadMe.dll

HINSTANCE LoadME;
LoadMe = LoadLibrary("C:\\Documents and Settings\\Usuario\\IDLWorkspace\\s_TimeCalc\\dll\\LoadMe.dll");

// Check to see if the library was loaded successfully
if (LoadMe != 0)
    printf("LoadMe library loaded!\n");
else
    printf("LoadMe library failed to load!\n");

//declare a variable of type pointer to EntryPoint function, a name of
// which you will later use instead of EntryPoint
EntryPointfuncPtr LibMainEntryPoint;

// GetProcAddress – is a function, which returns the address of the
// specified exported dynamic-link library (DLL) function. After
// performing this step you are allowed to use a variable
// LibMainEntryPoint as an equivalent of the function exported in
// LoadMe.dll. In other words, if you need to call
// EntryPoint(int, const char *) function, you call it as
// LibMainEntryPoint(int, const char *)

LibMainEntryPoint = (EntryPointfuncPtr)GetProcAddress(LoadMe,"entryPoint");