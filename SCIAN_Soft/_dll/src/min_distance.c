/* Standard Headers */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* OpenMP Headers */
#include <omp.h>

/* IDL Headers */
#include "idl_export.h"

#define INSANELY_BIG_NUMBER 1e50

static char msg[512];

IDL_VPTR min_distance(int argc, IDL_VPTR *argv) {
	IDL_VPTR vp_data, vp_sizes, vp_max_size, vp_n_objects, vp_thres, vp_ret;

	double *data;
	double thres;
	IDL_LONG *sizes;
	IDL_LONG n_objects, max_size;
	IDL_LONG i1, i2, k1, k2;

	double *ret, norm, x1, y1, z1, x2, y2, z2;
	IDL_INT *marks;
	IDL_MEMINT dim[2];

	// Fill the variable pointers
	vp_data      = argv[0];
	vp_sizes     = argv[1];
	vp_max_size  = argv[2];
	vp_n_objects = argv[3];
	vp_thres     = argv[4];

	// Extracting the data from the variable pointers to friendly variables
	data      = (double *)vp_data->value.arr->data;
	sizes     = (IDL_LONG *)vp_sizes->value.arr->data;
	max_size  = (IDL_LONG)vp_max_size->value.l;
	n_objects = (IDL_LONG)vp_n_objects->value.l;
	thres     = (double)vp_thres->value.d;


	// Allocating memory for the return value and the auxiliary variable marks
	// and then initializing it's values
	ret = (double *)IDL_MemAlloc(
			sizeof(double)*3*n_objects,
			"Could Not Allocate Memory for ret", 
			IDL_MSG_LONGJMP
			);

	marks = (IDL_INT *)IDL_MemAlloc(
			sizeof(IDL_INT)*n_objects*max_size,
			"Could Not Allocate Memory for marks",
			IDL_MSG_LONGJMP
			);

	for(i1=0; i1< n_objects*max_size; i1++) marks[i1] = 0;
	for(i1=0; i1< n_objects; i1++) ret[i1] = INSANELY_BIG_NUMBER;
	for(i1=n_objects; i1< 3*n_objects; i1++) ret[i1] = 0.0;



	// for every object we compare it against every other object ONCE
	for(i1=0; i1<n_objects; i1++) {
		int index1;
		int index2;
		for(i2=i1+1; i2<n_objects; i2++) {
			double *norms;
			norms = (double *)malloc(sizeof(double)*sizes[i2]);
			for(k1 = 0; k1<sizes[i1]; k1++) {

				index1 = i1+3*k1*n_objects;

                #pragma omp parallel for \
				 shared(ret,marks,n_objects,data,i1,i2,k1) \
				 private(index1,index2,norm) 
				for(k2 = 0; k2<sizes[i2]; k2++) {

					index2 = i2+3*k2*n_objects;


					/*
					x1 = data[i1+k1*n_objects*3];
					x2 = data[i2+k2*n_objects*3];

					y1 = data[i1+n_objects+k1*n_objects*3];
					y2 = data[i2+n_objects+k2*n_objects*3];

					z1 = data[i1+2*n_objects+k1*n_objects*3];
					z2 = data[i2+2*n_objects+k2*n_objects*3];
					*/

					norms[k2] =  pow(data[index1]-data[index2],2.0) + 
							pow(data[index1+n_objects]-data[index2+n_objects],2.0) + 
							pow(data[index1+2*n_objects]-data[index2+2*n_objects],2.0);
				}

				#pragma omp sections
				{

					#pragma omp section
					for(k2=0; k2<sizes[i2]; k2++) {
						if(ret[i1] > norms[k2]) ret[i1] = norms[k2];
						if(ret[i2] > norms[k2]) ret[i2] = norms[k2];
					}
					
					#pragma omp section
					for(k2=0; k2<sizes[i2]; k2++) {

						if(norms[k2] <= thres && !marks[i1+k1*n_objects]) {
							ret[i1+n_objects]++;
							marks[i1+k1*n_objects] = 1;
						}
					}

					#pragma omp section
					for(k2=0; k2<sizes[i2]; k2++) {
						if(norms[k2] <= thres && !marks[i2+k2*n_objects]) {
							ret[i2+n_objects]++;
							marks[i2+k2*n_objects] = 1;
						}
					}

				}
			}
			free(norms);
		}
	}

	
	for(i1=2*n_objects; i1< 3*n_objects; i1++) {

		ret[i1-2*n_objects] = sqrt(ret[i1-2*n_objects]);
		ret[i1] = ret[i1-n_objects]/(double)sizes[i1-2*n_objects];
	}

	IDL_MemFree(marks, "Couldn't free memory for marks", IDL_MSG_INFO);

	dim[0] = n_objects;
	dim[1] = 3;
	vp_ret = IDL_ImportArray(2, dim, IDL_TYP_DOUBLE, (UCHAR *)ret, (IDL_ARRAY_FREE_CB)IDL_MemFree, NULL);
	return vp_ret;

}




/* IDL Linking Functions */
static IDL_SYSFUN_DEF2 main_def[] = { 
	{min_distance, "MIN_DISTANCE", 5, 5, 0, 0} // This throws a harmless warning with -Wall
};

int IDL_Load(void) {
	return IDL_SysRtnAdd(main_def, TRUE, IDL_CARRAY_ELTS(main_def));
}

