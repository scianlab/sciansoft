#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <stdlib.h>

#include "idl_export.h"

struct my_arr
{
  IDL_ULONG *data;
  int size;
};

IDL_VPTR where_xyz_arr(int argc, IDL_VPTR argv[])
{
  IDL_VPTR arr, val, res, arr_ind, val_ind;
  IDL_ULONG *arr_d, *res_tmp, *res_d;
  IDL_INT *value_d;
  IDL_ARRAY_DIM dim;
  int size_1, size_2, i = 0, j = 0, k = 0, l = 0, block_size_1 = 4, block_size_2, arr_size, val_size;
  struct my_arr **all_data = 0;
  IDL_LONG *indexes_arr, *indexes_val;

  arr = argv[0];
  val = argv[1];
  arr_ind = argv[2];
  val_ind = argv[3];

  IDL_ENSURE_SIMPLE(arr);
  IDL_ENSURE_ARRAY(arr);
  IDL_ENSURE_SIMPLE(val);
  IDL_ENSURE_ARRAY(val);
  IDL_ENSURE_SIMPLE(arr_ind);
  IDL_ENSURE_ARRAY(arr_ind);
  IDL_ENSURE_SIMPLE(val_ind);
  IDL_ENSURE_ARRAY(val_ind);

  if (arr->type != IDL_TYP_ULONG) {
    arr = IDL_CvtULng(1, argv);
  }

  arr_d = (IDL_ULONG *) arr->value.arr->data;
  arr_size = arr->value.arr->n_elts;

  if (val->type != IDL_TYP_INT) {
    val = IDL_CvtUInt(1, argv+1);
  }

  value_d = (IDL_INT *) val->value.arr->data;
  val_size = val->value.arr->n_elts;

  if (arr_ind->type != IDL_TYP_LONG) {
    arr_ind = IDL_CvtLng(1, argv+2);
  }

  indexes_arr = (IDL_LONG *) arr_ind->value.arr->data;

  if (val_ind->type != IDL_TYP_LONG) {
    val_ind = IDL_CvtLng(1, argv+3);
  }

  indexes_val = (IDL_LONG *) val_ind->value.arr->data;

  block_size_2 = val_size;
  res_tmp = (IDL_ULONG *) malloc(block_size_1*sizeof(IDL_ULONG));
  size_1 = block_size_1;
  size_2 = block_size_2;
  dim[0] = 0;

  all_data = (struct my_arr**)malloc(block_size_2*sizeof(struct my_arr*));

  all_data[k] = (struct my_arr*)malloc(sizeof(struct my_arr));
  all_data[k]->data = res_tmp;
  all_data[k++]->size = 1; /* Initialize with one, considering the first element will be the size of each row */

  while(i < arr_size && l < val_size) {
    if(arr_d[indexes_arr[i]] == value_d[indexes_val[l]]) {
      if(j >= size_1) {
        size_1*=2;
        res_tmp = (IDL_ULONG *) realloc(res_tmp, size_1*sizeof(IDL_ULONG));
      }

      res_tmp[j++] = indexes_arr[i++];
      all_data[k-1]->size++;
    }
    else if(arr_d[indexes_arr[i]] > value_d[indexes_val[l]]) {
      if(k >= size_2) {
        size_2*=2;
        all_data = (struct my_arr**) realloc(all_data, size_2*sizeof(struct my_arr*));
      }

      dim[0] += all_data[k-1]->size;
      l++;
      all_data[k-1]->data = res_tmp;
      all_data[k] = (struct my_arr*)malloc(sizeof(struct my_arr));
      res_tmp = (IDL_ULONG *)malloc(block_size_1*sizeof(IDL_ULONG));
      size_1 = block_size_1;
      all_data[k++]->size = 1;
      j = 0;
    }
    else {
      i++;
    }
  }

  dim[0] += all_data[k-1]->size;
  all_data[k-1]->data = res_tmp;

  res_d = (IDL_ULONG *)
    IDL_MakeTempArray(IDL_TYP_ULONG,1,dim,
                      IDL_ARR_INI_NOP, &res);

  for(l = 0; l < val_size; ++l) {
	j = all_data[indexes_val[l]]->size;
	res_tmp = all_data[indexes_val[l]]->data;
	*res_d++ = j-1;
    while(--j) {
      *res_d++ = *res_tmp++;
    }
    free(all_data[indexes_val[l]]->data);
    free(all_data[indexes_val[l]]);
  }

  free(all_data);

  return(res);
}