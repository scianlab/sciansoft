#include <stdio.h> 
#include "idl_export.h" 
 
IDL_VPTR hello_world(int argc, IDL_VPTR argv[]) 
{ 
  return(IDL_StrToSTRING("Hello World!")); 
} 

