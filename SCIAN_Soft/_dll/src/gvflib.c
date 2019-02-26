#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <omp.h>
#include "idl_export.h"


double double_max(double, double);
int gvf_natural(
		long *iterations_ptr,
		short  *x_max_ptr, short *y_max_ptr,
		double *dt_ptr, 
		double *u_idl, double *v_idl, 
		double *fx, double *fy,
		double *g, double *h,
		double *err_tol_ptr
		){

	unsigned short i, k, iterations;
	short x, y;
	long ci, x_max, y_max;
	double dt;
	double *u_next, *v_next, *aux;
	double *u, *v;
	double uxx, uyy;
	double vxx, vyy;
	double err, err_tol, norm, norm_ant;
	double *errs, *norms;


	char buf[1024];

	x_max = (long)*x_max_ptr;
	y_max = (long)*y_max_ptr;
	iterations = *iterations_ptr;
	dt = *dt_ptr;
	err_tol = *err_tol_ptr;

	u= (double *)malloc(x_max * y_max * sizeof(double));
	v= (double *)malloc(x_max * y_max * sizeof(double));

	u_next = (double *)malloc(x_max * y_max * sizeof(double));
	v_next = (double *)malloc(x_max * y_max * sizeof(double));

	errs      = (double *)malloc(omp_get_max_threads() * sizeof(double));
	norms     = (double *)malloc(omp_get_max_threads() * sizeof(double));

	ci = 0;
	for(y = 0; y < y_max; y++) {
		for(x = 0; x < x_max; x++) {
			u[ci] = u_idl[ci];
			v[ci] = v_idl[ci];
			ci++;
		}
	}

	for(i = 0; i < iterations; i++) {
		err = 0;
		norm = 0;
		norm_ant = 0;

		for(k = 0; k < omp_get_max_threads(); k++) {
			errs[k] = 0;
			norms[k] = 0;
		}

		#pragma omp parallel for \
		private(i,ci,\
				x,y, \
				uxx,uyy,\
				vxx,vyy\
		) \
		shared(u,v,dt,\
			   x_max,y_max,\
			   err, norm_ant, norm,\
			   errs, norms \
		)
		for(ci = 0; ci < x_max * y_max; ci++) {
			x = ci % x_max;
			y = ci / x_max;

			if(x == 0) {
				uxx = (-2 * u[ci] + u[ci+1])/2;
				vxx = (-2 * v[ci] + v[ci+1])/2;

				if(y == 0) {
					uyy = (-2 * u[ci] + u[ci+x_max])/2;
					vyy = (-2 * v[ci] + v[ci+x_max])/2;
				}

				else if(y == y_max-1) {
					uyy = (u[ci-x_max] - 2*u[ci])/2;
					vyy = (v[ci-x_max] - 2*v[ci])/2;
				}

				else {
					uyy = (u[ci+x_max] - 2*u[ci] + u[ci-x_max]);
					vyy = (v[ci+x_max] - 2*v[ci] + v[ci-x_max]);
				}
			}
			else if(x == x_max-1) {
				uxx = (u[ci-1] - 2*u[ci])/2;
				vxx = (v[ci-1] - 2*v[ci])/2;

				if(y == 0) {
					uyy = (-2 * u[ci] + u[ci+x_max])/2;
					vyy = (-2 * v[ci] + v[ci+x_max])/2;
				}

				else if(y == y_max-1) {
					uyy = (u[ci-x_max] - 2*u[ci])/2;
					vyy = (v[ci-x_max] - 2*v[ci])/2;
				}

				else {
					uyy = (u[ci+x_max] - 2*u[ci] + u[ci-x_max]);
					vyy = (v[ci+x_max] - 2*v[ci] + v[ci-x_max]);
				}
			}
			else {
				uxx = (u[ci+1] - 2*u[ci] + u[ci-1])/2;
				vxx = (v[ci+1] - 2*v[ci] + v[ci-1])/2;

				if(y == 0) {
					uyy = (-2 * u[ci] + u[ci+x_max])/2;
					vyy = (-2 * v[ci] + v[ci+x_max])/2;
				}

				else if(y == y_max-1) {
					uyy = (u[ci-x_max] - 2*u[ci])/2;

					vyy = (v[ci-x_max] - 2*v[ci])/2;
				}

				else {
					uyy = (u[ci+x_max] - 2*u[ci] + u[ci-x_max]);
					vyy = (v[ci+x_max] - 2*v[ci] + v[ci-x_max]);
				}

			}

			u_next[ci] = u[ci] + dt*(g[ci]*(uxx + uyy)- h[ci]*(u[ci] - fx[ci]));
			v_next[ci] = v[ci] + dt*(g[ci]*(vxx + vyy) -h[ci]*(v[ci] - fy[ci]));

			errs[omp_get_thread_num()] += 
				pow(g[ci]*(uxx + uyy)- h[ci]*(u[ci] - fx[ci]), 2) + 
				pow(g[ci]*(vxx + vyy) -h[ci]*(v[ci] - fy[ci]), 2);

			norms[omp_get_thread_num()] += pow(u[ci], 2) + pow(v[ci], 2);

			/*
			err += 
				pow(g[ci]*(uxx + uyy)- h[ci]*(u[ci] - fx[ci]), 2) + 
				pow(g[ci]*(vxx + vyy) -h[ci]*(v[ci] - fy[ci]), 2);

			norm += pow(u[ci], 2) + pow(v[ci], 2);
			*/
		}


		aux = u;
		u = u_next;
		u_next = aux;

		aux = v;
		v = v_next;
		v_next = aux;

		for(k = 0; k < omp_get_max_threads(); k++) {
			err += errs[k];
			norm += norms[k];
		}

		err = sqrt(err);
		norm = sqrt(norm);


		err /= double_max(norm, norm_ant);

		if(err < err_tol || i == iterations-1) {
			sprintf(buf, "stopped at err = %f, it= %d", err, i);
			IDL_Message(IDL_M_GENERIC, IDL_MSG_INFO, buf);
			break;
		}

		norm_ant = norm;
	}

	ci = 0;
	for(y = 0; y < y_max; y++) {
		for(x = 0; x< x_max; x++) {
			u_idl[ci] = u[ci];
			v_idl[ci] = v[ci];
			ci++;
		}
	}

	free(u);
	free(v);
	free(u_next);
	free(v_next);


	return 1;
}

double double_max(double a, double b) {
	if(a>b) return a ;
	else return b;
}

int gvf(int argc, void *argv[]) {
	if(argc != 11) 
		return 0;

	return gvf_natural(
		(long *) argv[0],
		(short  *)argv[1], (short *)argv[2],
		(double *)argv[3], 
		(double *)argv[4], (double *)argv[5], 
		(double *)argv[6], (double *)argv[7], 
		(double *)argv[8], (double *)argv[9],
		(double *)argv[10]
		);
}
