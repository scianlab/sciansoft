/*
 * $Id: //depot/idl/IDL_70/idldir/external/call_external/C/incr_struct.c#1 $
 *
 *
 * NAME:
 * 	incr_struct.c
 *
 * PURPOSE:
 *	This C function is used to demonstrate how to read an IDL
 *      structure or array of IDL structures in a CALL_EXTERNAL routine.
 *      The fields of the IDL structure and the corresponding C structure
 *      must match exactly.
 *
 * CATEGORY:
 *	Dynamic Linking Examples
 *
 * CALLING SEQUENCE:
 *	This function is called in IDL by using the following commands:
 *
 *      IDL>s = {ASTRUCTURE}
 *      IDL>r = call_external(library_name,'incr_struct',s,N_ELEMENTS(s),
 *                            VALUE=[0,1])
 *
 *      See incr_struct.pro for a more complete calling sequence.
 *
 * INPUTS:
 *      mystructure - an array of structures of type ASTRUCTURE
 *      n  - number of elements in the array (type is IDL_LONG)
 *
 * OUTPUTS:
 *	The function returns 1 (long) on success and 0 otherwise.
 *
 * SIDE EFFECTS:
 *	None.
 *
 * RESTRICTIONS:
 *
 *      It is important that the IDL structure definition
 *      and the C structure definition match exactly.  Otherwise,
 *      there will be no way to prevent this program from
 *      segfaulting or doing other strange things.
 *
 * MODIFICATION HISTORY:
 *	Written May, 1998 JJG
 *	AB, 11 April 2002, Updated for MAKE_DLL and general cleanup
 *
*/
#include <stdio.h>
#include "idl_export.h"

/*
 * C definition for the structure that this
 * routine accepts.  The corresponding IDL
 * structure definition would look like this:
 * s = {zero:0B,one:0L,two:0.,three:0D,four: intarr(2)}
*/
typedef struct {
  unsigned char zero;
  IDL_LONG one;
  float two;
  double three;
  short four[2];
} ASTRUCTURE;

IDL_VPTR incr_struct_natural(ASTRUCTURE *mystructure, IDL_LONG n)
/*
 * Version with natural C interface. This version can be called directly
 * by IDL using the AUTO_GLUE keyword to CALL_EXTERNAL.
 */
{
  char msg[255];
  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "En incr_struct_natural");
      sprintf(msg, "n = %d", n);
      IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, msg);
  /* for each structure in the array, increment every field */
  for (; n--; mystructure++) {
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "Dentro del for");
    mystructure->zero++;
    mystructure->one++;
    mystructure->two++;
    mystructure->three++;
    mystructure->four[0]++;
    mystructure->four[1]++;
  }
  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "Afuera del for");
  return IDL_GettmpInt((IDL_INT) 1);
}


IDL_MEMINT one = 1;
IDL_MEMINT tag2_dims[] = { 3, 2, 3, 4};
IDL_MEMINT tag3_dims[] = { 1, 10 };
IDL_STRUCT_TAG_DEF s_tags[] = {
  { "TAG1", 0, (void *) IDL_TYP_LONG},
  { "TAG2", tag2_dims, (void *) IDL_TYP_FLOAT},
  { "TAG3", tag3_dims, (void *) IDL_TYP_STRING},
  { 0 }
};
typedef struct data_struct {
  IDL_LONG tag1_data;
  float tag2_data [4] [3] [2];
  IDL_STRING tag_3_data [10];
} DATA_STRUCT;


IDL_MEMINT dim1 = 1;
IDL_MEMINT dim2[] = {1,10};
IDL_STRUCT_TAG_DEF s_tags2[] = {
  { "zero", 0, (void *) IDL_TYP_BYTE},
  { "one", 0, (void *) IDL_TYP_LONG},
  { "two", 0, (void *) IDL_TYP_FLOAT},
  { "three", 0, (void *) IDL_TYP_DOUBLE},
  { "four", dim2, (void *) IDL_TYP_INT},
  { 0 }
};


IDL_VPTR incr_struct(int argc, IDL_VPTR argv[])
/*
 * Version with IDL portable calling convension.
 *
 * entry:
 *	argc - Must be 2.
 *	argv[0] - Array of structures.
 *	argv[2] - number of elements in array.
 *
 * exit:
 *	Returns TRUE for success, and FALSE for failure.
 */
{
  IDL_VPTR res;
  char msg[255];
  void *s;
  IDL_VPTR v;
  DATA_STRUCT *s_data;
  ASTRUCTURE a_data;
  int i, j, k;
  float ***t2_data;

  if (argc != 2) return 0;
  if(argv[0]->flags & IDL_V_STRUCT)
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "argv[0] es una IDL_V_STRUCT");
  if(argv[0]->flags & IDL_V_ARR)
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "argv[0] es IDL_V_ARR");
  if(argv[0]->flags & IDL_V_DYNAMIC)
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "argv[0] es IDL_V_DYNAMIC");
  if(argv[0]->flags & IDL_TYP_STRUCT)
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "argv[0] es IDL_TYP_STRUCT");


  if(argv[0]->type == IDL_TYP_STRUCT)
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "argv[0] es IDL_TYP_STRUCT");


  s_data = (DATA_STRUCT *)IDL_MemAlloc(
			sizeof(DATA_STRUCT),
			"Could Not Allocate Memory for s_data",
			IDL_MSG_LONGJMP
			);

  /*res = incr_struct_natural((ASTRUCTURE*) argv[0]->value.s, (IDL_LONG) argv[1]->value.l);*/



/* Create the structure definition */

IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "antes de IDL_MakeStruct");
s = IDL_MakeStruct("ASTRUCTURE", s_tags2);
IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "despues de IDL_MakeStruct");

  /*
  s_data->tag1_data = (IDL_LONG)1;
  t2_data = (float ***)malloc(sizeof(float **)*4);
  for(i = 0; i < 4; ++i) {
	t2_data[i] = (float **)malloc(sizeof(float *)*3);
    for(j = 0; j < 3; ++j) {
      t2_data[i][j] = (float *)malloc(sizeof(float)*2);
      for(k = 0; k < 2; ++k) {
        t2_data[i][j][k] = (float)(100*i+10*j+k);
      }
    }
  }
  for(i = 0; i < 4; ++i) {
    for(j = 0; j < 3; ++j) {
      for(k = 0; k < 2; ++k) {
        s_data->tag2_data[i][j][k] = (float)(100*i+10*j+k);
      }
    }
  }
  */

  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "fin primer for");

  a_data.zero = 0;
  a_data.one = 1;
  a_data.two = 2;
  a_data.three = 3;


/* Import the data area s_data into an IDL structure,
   note that no data are moved. */
  v = IDL_ImportArray(1, &one, IDL_TYP_STRUCT,
                (UCHAR *) &a_data, 0, s);
  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "despues de IDL_ImportArray");

  /*
  for(i = 0; i < 10; ++i) {
	IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "1");
    s_data.tag_3_data[i] = IDL_StrToSTRING("")->value.str;
  }
  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "fin segundo for");
  */

  return v;
  /*return IDL_GettmpInt((IDL_INT) 1);*/
}
