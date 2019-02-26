        #include <stdio.h>
        #include <string.h>
        #include <stdarg.h>
        #include <sys/types.h>
        #include <sys/stat.h>
        #include <fcntl.h>
		#include "idl_export.h"


        void error(char *fmt, ...)
          {
            va_list args;

            va_start(args, fmt);
            fprintf(stderr, "Error: ");
            vfprintf(stderr, fmt, args);
            fprintf(stderr, "\n");
            va_end(args);

            exit(1);
          }



/* prototype for IDL_Load */
int IDL_Load( void );

/*
 * Define message codes and their corresponding printf(3) format
 * strings. Note that message codes start at zero and each one is
 * one less that the previous one. Codes must be monotonic and
 * contiguous.
 */
static IDL_MSG_DEF msg_arr[] =
{
#define M_TM_EP                       0
  {  "M_TM_EP",   "%NTEntry Point function." },
};



/*
 * The load function fills in this message block handle with the
 * opaque handle to the message block used for this module. The other
 * routines can then use it to throw errors from this block.
 */
static IDL_MSG_BLOCK msg_block;


        static void EntryPoint(int argc, IDL_VPTR *argv)
          {
            int fd;
            char buf[BUFSIZ];
            char *path="C:\\Documents and Settings\\Usuario\\IDLWorkspace\\s_TimeCalc\\dll\\out.txt";
            char *msg="Ejecutando...";
            int n = 13;

            if((fd=creat(path, 0666))==-1)
                error("Can't open %s", path);
            /* usando stats se pueden mantener los permisos */

            if(write(fd, msg, n)!=n)
                error("Write error on file %s", path);

			IDL_MessageFromBlock(msg_block, M_TM_EP, IDL_MSG_RET);
          }


int IDL_Load(void)
{
  /*
   * These tables contain information on the functions and procedures
   * that make up the TESTMODULE DLM. The information contained in these
   * tables must be identical to that contained in testmodule.dlm.
   */
  static IDL_SYSFUN_DEF2 procedure_addr[] = {
    { (IDL_FUN_RET) EntryPoint, "ENTRYPOINT", 0, IDL_MAX_ARRAY_DIM, 0, 0},
  };


  /*
   * Create a message block to hold our messages. Save its handle where
   * the other routines can access it.
   */
  if (!(msg_block = IDL_MessageDefineBlock("LoadMe",
					   IDL_CARRAY_ELTS(msg_arr), msg_arr)))
    return IDL_FALSE;

  /*
   * Register our routine. The routines must be specified exactly the same
   * as in testmodule.dlm.
   */
  return IDL_SysRtnAdd(procedure_addr, FALSE, IDL_CARRAY_ELTS(procedure_addr));
}

        main(int argc, char *argv[])
          {
			EntryPoint(argc, 0);
            return 0;
          }