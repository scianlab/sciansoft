#include <stdio.h>
#include "idl_export.h"

IDL_VPTR mult2(int argc, IDL_VPTR argv[])
{
  IDL_VPTR dst, src;
  float *src_d, *dst_d;
  int n;
  src = dst = argv[0];

  IDL_ENSURE_SIMPLE(src);
  IDL_ENSURE_ARRAY(src);

  if (src->type != IDL_TYP_FLOAT)
    src = dst = IDL_CvtFlt(1, argv);

  src_d = dst_d = (float *) src->value.arr->data;

  if (!(src->flags & IDL_V_TEMP))
    dst_d = (float *)
      IDL_MakeTempArray(IDL_TYP_FLOAT,src->value.arr->n_dim,
                      src->value.arr->dim,
                      IDL_ARR_INI_NOP, &dst);

  for (n = src->value.arr->n_elts; n--; )
     *dst_d++ = 2.0 * *src_d++;

  return(dst);
}

