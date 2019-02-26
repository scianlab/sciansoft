#include <stdio.h>
#include <limits.h>
#include "idl_export.h"

struct my_arr
{
  IDL_ULONG *data;
  int size;
};

void swap(int *indexes, int a, int b)
{
	int temp=indexes[a];
	indexes[a]=indexes[b];
	indexes[b]=temp;
}

void quicksort(IDL_ULONG* arr, int izq, int der, int *indexes) /*Se llama con: quicksort(&vector[0],&vector[n-1]);*/
{
	int i = 0;
	IDL_ULONG pivot;
	int ult=der;
	int pri=izq;
	if(der<izq) return;
	pivot=arr[indexes[izq]];


	while(izq<der)
	{
		while(arr[indexes[izq]]<=pivot && izq<der+1) izq++;
		while(arr[indexes[der]]>pivot && der>izq-1) der--;
		if(izq<der) swap(indexes, izq,der);
	}
	swap(indexes, pri,der);
	quicksort(arr, pri,der-1, indexes);
	quicksort(arr, der+1,ult, indexes);
}

/*
 int *quickSort(IDL_ULONG *arr, int elements) {

  #define  MAX_LEVELS  300

  char msg[255];
  int *indexes = (int *)malloc(elements*sizeof(int));
  int  piv, beg[MAX_LEVELS], end[MAX_LEVELS], i=0, L, R, swap ;
  for(i = 0; i < elements; ++i)
    indexes[i]= i;
  sprintf(msg, "i final = %d.", (i-1));
  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, msg);
  i = 0;

  if(indexes == NULL) {
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "Malloc fallo");
  }

  beg[0]=0; end[0]=elements;
  while (i>=0) {
    L=beg[i]; R=end[i]-1;
    if (L<R) {
      piv=indexes[L];
      while (L<R) {
        while (arr[indexes[R]]>=arr[piv] && L<R) R--; if (L<R) indexes[L++]=indexes[R];
        while (arr[indexes[L]]<=arr[piv] && L<R) L++; if (L<R) indexes[R--]=indexes[L];
      }
      indexes[L]=piv; beg[i+1]=L+1; end[i+1]=end[i]; end[i++]=L;
      if (end[i]-beg[i]>end[i-1]-beg[i-1]) {
        swap=beg[i]; beg[i]=beg[i-1]; beg[i-1]=swap;
        swap=end[i]; end[i]=end[i-1]; end[i-1]=swap;
      }
    }
    else {
      i--;
    }
  }
  return indexes;
}
*/
long *quickSort2(IDL_INT *arr, int elements) {

  #define  MAX_LEVELS  300

  int *indexes = (int *)malloc(elements*sizeof(int));
  int  piv, beg[MAX_LEVELS], end[MAX_LEVELS], i=0, L, R, swap ;

  if(indexes == NULL) {
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "Malloc fallo");
  }

  for(i = 0; i < elements; ++i)
    indexes[i]= i;
  i = 0;

  beg[0]=0; end[0]=elements;
  while (i>=0) {
    L=beg[i]; R=end[i]-1;
    if (L<R) {
      piv=indexes[L];
      while (L<R) {
        while (arr[indexes[R]]>=arr[piv] && L<R) R--; if (L<R) indexes[L++]=indexes[R];
        while (arr[indexes[L]]<=arr[piv] && L<R) L++; if (L<R) indexes[R--]=indexes[L];
      }
      indexes[L]=piv; beg[i+1]=L+1; end[i+1]=end[i]; end[i++]=L;
      if (end[i]-beg[i]>end[i-1]-beg[i-1]) {
        swap=beg[i]; beg[i]=beg[i-1]; beg[i-1]=swap;
        swap=end[i]; end[i]=end[i-1]; end[i-1]=swap;
      }
    }
    else {
      i--;
    }
  }
  return indexes;
}

IDL_VPTR where_xyz_arr(int argc, IDL_VPTR argv[])
{
  IDL_VPTR arr, val, res;
  IDL_ULONG *arr_d, *res_tmp, *res_d;
  IDL_INT *value_d;
  IDL_ARRAY_DIM dim;
  int size_1, size_2, i = 0, j = 0, k = 0, l = 0, block_size_1 = 4, block_size_2, arr_size, val_size;
  struct my_arr **all_data = 0;
  char msg[255];
  int *indexes_arr, *indexes_val;
  arr = argv[0];
  val = argv[1];

  IDL_ENSURE_SIMPLE(arr);
  IDL_ENSURE_ARRAY(arr);
  IDL_ENSURE_SIMPLE(val);
  IDL_ENSURE_ARRAY(val);

  if (arr->type != IDL_TYP_ULONG) {
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "Se convirtio arr a ULng");
    arr = IDL_CvtULng(1, argv);
  }

  arr_d = (IDL_ULONG *) arr->value.arr->data;
  arr_size = arr->value.arr->n_elts;

  if (val->type != IDL_TYP_INT) {
    val = IDL_CvtUInt(1, argv+1);
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "Se convirtio value a UInt");
  }

  value_d = (IDL_INT *) val->value.arr->data;
  val_size = val->value.arr->n_elts;
  block_size_2 = val_size;
  res_tmp = (IDL_ULONG *) malloc(block_size_1*sizeof(IDL_ULONG));
  size_1 = block_size_1;
  size_2 = block_size_2;
  dim[0] = 0;

  all_data = (struct my_arr**)malloc(block_size_2*sizeof(struct my_arr*));

  all_data[k] = (struct my_arr*)malloc(sizeof(struct my_arr));
  all_data[k]->data = res_tmp;
  all_data[k++]->size = 1; /* Initialize with one, considering the first element will be the size of each row */


  sprintf(msg, "Antes de quickSort, n_elem = %d", val_size);
  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, msg);
  indexes_val = quickSort2(value_d, val_size);

  sprintf(msg, "Antes de quickSort, n_elem = %d", arr_size);
  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, msg);
  indexes_arr = (int *)malloc(arr_size*sizeof(int));
  for(i = 0; i < arr_size; ++i)
    indexes_arr[i]= i;

  quicksort(arr_d, 0, arr_size-1, indexes_arr);

/*
  if(1) {
    sprintf(msg, "INT_MAX = %d.", INT_MAX);
	IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, msg);
    sprintf(msg, "LONG_MAX = %d.", LONG_MAX);
	IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, msg);
    return(IDL_StrToSTRING("A tiiii"));
  }
*/
  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "Despues de quickSort");

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

  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "Despues de while");
  dim[0] += all_data[k-1]->size;
  all_data[k-1]->data = res_tmp;

  res_d = (IDL_ULONG *)
    IDL_MakeTempArray(IDL_TYP_ULONG,1,dim,
                      IDL_ARR_INI_NOP, &res);

  j = all_data[indexes_val[0]]->size;
  res_tmp = all_data[indexes_val[0]]->data;

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

  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "Fin funcion");

  return(res);
}