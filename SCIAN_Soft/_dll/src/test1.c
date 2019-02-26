#include <stdio.h>
#include "idl_export.h"



/* prototype for IDL_Load */
int IDL_Load_1( void );

/*
 * Define message codes and their corresponding printf(3) format
 * strings. Note that message codes start at zero and each one is
 * one less that the previous one. Codes must be monotonic and
 * contiguous.
 */
static IDL_MSG_DEF msg_arr[] =
{
#define M_TM_INPRO                       0
  {  "M_TM_INPRO",   "%NThis is from a loadable module procedure." },
#define M_TM_INFUN                       -1
  {  "M_TM_INFUN",   "%NThis is from a loadable module function." },
};



/*
 * The load function fills in this message block handle with the
 * opaque handle to the message block used for this module. The other
 * routines can then use it to throw errors from this block.
 */
static IDL_MSG_BLOCK msg_block;



/* Implementation of the TESTPRO IDL procedure */

static void testpro_1(int argc, IDL_VPTR *argv)
{
  IDL_MessageFromBlock(msg_block, M_TM_INPRO, IDL_MSG_RET);
}



/* Implementation of the TESTFUN IDL function */
static IDL_VPTR testfun_1(int argc, IDL_VPTR *argv)
{
  IDL_MessageFromBlock(msg_block, M_TM_INFUN, IDL_MSG_RET);
  return IDL_StrToSTRING("TESTFUN_1");
}



int IDL_Load_1(void)
{
  int res;
  /*
   * These tables contain information on the functions and procedures
   * that make up the TESTMODULE DLM. The information contained in these
   * tables must be identical to that contained in testmodule.dlm.
   */
  static IDL_SYSFUN_DEF2 function_addr[] = {
    { testfun_1, "TESTFUN_1", 0, IDL_MAXPARAMS, 0, 0},
  };

  static IDL_SYSFUN_DEF2 procedure_addr[] = {
    { (IDL_FUN_RET) testpro_1, "TESTPRO_1", 0, IDL_MAX_ARRAY_DIM, 0, 0},
  };


  /*
   * Create a message block to hold our messages. Save its handle where
   * the other routines can access it.
   */
  if (!(msg_block = IDL_MessageDefineBlock("Testmodule",
					   IDL_CARRAY_ELTS(msg_arr), msg_arr)))
    return IDL_FALSE;

  /*
   * Register our routine. The routines must be specified exactly the same
   * as in testmodule.dlm.
   */


  res = IDL_SysRtnAdd(function_addr, TRUE, IDL_CARRAY_ELTS(function_addr))
    && IDL_SysRtnAdd(procedure_addr, FALSE, IDL_CARRAY_ELTS(procedure_addr));
  if(!res)
    IDL_Message(IDL_M_GENERIC, IDL_MSG_RET,
     "Error adding system routine");
  return res;
}
