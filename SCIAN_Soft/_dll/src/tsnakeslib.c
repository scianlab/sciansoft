/* Standard Headers */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <float.h>
#include <time.h>

/* OpenMP Headers */
#include <omp.h>

/* IDL Headers */
#include "idl_export.h"

/* Custom Headers */
#include "tsnakeslib.h"



static int global_it;
IDL_VPTR evolve_snakes(int argc, IDL_VPTR *argv) {
	IDL_MEMINT dims[IDL_MAX_ARRAY_DIM];
	IDL_INT *image;
	IDL_INT m, interior1_ls, interior1_rs, interior2_ls, interior2_rs, auto_init;
	IDL_LONG x_max, y_max, nsnakes, npts, nproc, lenarr, res_x, res_y;
	double *snake_x, *snake_y, *pdnpts, *vfx, *vfy, *data;
	double p, q, a, b, gamma, dt, dnpts, res_factor, rad;
	mesh grid;
	sbox sb, sb_aux;
	snake s;
	queue qu, q_in, q_out;

	IDL_LONG i,j,it;

	image = (IDL_INT *)argv[0]->value.arr->data;
	x_max = (IDL_LONG)argv[0]->value.arr->dim[0];
	y_max = (IDL_LONG)argv[0]->value.arr->dim[1];


	m           = (IDL_INT)argv[3]->value.i;
	p           = (double)argv[4]->value.d;
	q           = (double)argv[5]->value.d;
	interior1_ls = (IDL_INT)argv[6]->value.i;
	interior1_rs = (IDL_INT)argv[7]->value.i;
	interior2_ls = (IDL_INT)argv[8]->value.i;
	interior2_rs = (IDL_INT)argv[9]->value.i;

	a     = (double)argv[10]->value.d;
	b     = (double)argv[11]->value.d;
	gamma = (double)argv[12]->value.d;
	dt    = (double)argv[13]->value.d;

	iprintf("a = %f, b = %f, gamma = %f, dt = %f", a, b, gamma, dt);

	vfx = (double *)argv[14]->value.arr->data;
	vfy = (double *)argv[15]->value.arr->data;

	it = (IDL_LONG)argv[16]->value.l;

	auto_init  = (IDL_INT)argv[17]->value.i;
	res_factor = (double)argv[18]->value.d;
	rad        = (double)argv[19]->value.d;

	res_x = x_max * res_factor;
	res_y = y_max * res_factor;

	grid = new_mesh(res_x, res_y, x_max, y_max, 2*ceil(res_x/10), 2*ceil(res_y/10));

	iprintf("Tsnakes library called");
	iprintf("a = %f, b = %f, gamma = %f, dt = %f", a, b, gamma, dt);
	iprintf("m = %d, p = %f, q = %f, interior1_ls = %d, interior1_rs = %d", m, p, q, interior1_ls, interior1_rs);
	iprintf("Created new grid, resolution : %ldx%ld", res_x, res_y);
	iprintf("nx = %ld, ny = %ld, hx = %f, hy = %f, bfx = %ld, bfy = %ld", grid->nx, grid->ny, grid->hx, grid->hy, grid->bfx, grid->bfy);


	qu = new_queue();
	q_in = new_queue(); // boundary interior nodes
	q_out = new_queue();// boudary exterior nodes
	sb= new_sbox();
	sb_aux = new_sbox();

	if (auto_init == 1) {
		double x0, y0;
		iprintf("AutoInit on, creating seed snakes");
		iprintf("rad = %f", rad);
		x0 = rad/2;
		while(x0 < (x_max + rad/3)) {
			y0 = rad/2;
			while(y0 < (y_max + rad/3)) {
				s = new_snake(8);

				s->x[0] = x0 + rad;
				s->x[1] = x0 + sqrt(2)/2 * rad;
				s->x[2] = x0;
				s->x[3] = x0 - sqrt(2)/2 * rad;
				s->x[4] = x0 - rad;
				s->x[5] = x0 - sqrt(2)/2 * rad;
				s->x[6] = x0;
				s->x[7] = x0 + sqrt(2)/2 * rad;

				s->y[0] = y0;
				s->y[1] = y0 + sqrt(2)/2 * rad;
				s->y[2] = y0 + rad;
				s->y[3] = y0 + sqrt(2)/2 * rad;
				s->y[4] = y0;
				s->y[5] = y0 - sqrt(2)/2 * rad;
				s->y[6] = y0 - rad;
				s->y[7] = y0 - sqrt(2)/2 * rad;

				phase_1(s, grid, qu);
				destroy_snake(s);

				y0 += rad * 2 + 2;
			}

			// Separacion entre centros de los circulos
			x0 += rad * 2 + 2;
		}
	}

	else {
		iprintf("AutoInit off");
		nsnakes = (IDL_LONG)argv[1]->value.l;
		lenarr  = (IDL_LONG)argv[2]->value.arr->dim[0];   

		iprintf("Collecting %ld snakes", nsnakes);
		for(i=0; i<nsnakes;i++){
			snake_x = (double *)argv[2]->value.arr->data + 2*i*lenarr + 1;
			snake_y = (double *)argv[2]->value.arr->data + (2*i+1)*lenarr + 1;

			pdnpts = snake_x - 1;
			dnpts = *pdnpts;
			npts = (IDL_LONG)dnpts;

			s = new_snake(npts);

			for(j=0; j<npts; j++) {
				s->x[j] = snake_x[j];
				s->y[j] = snake_y[j];
			}


			phase_1(s, grid, qu);
			destroy_snake(s);
		}
	}

	iprintf("First Phase 2");

	if (auto_init == 1) {
		phase_2(grid, qu, 10);
	
	} else {
		phase_2(grid, qu, -1);
	}

	iprintf("Collecting Snakes");
	collect_snakes(grid, qu, sb);
	iprintf("Cleaning Intersections");
	clean_intersections(grid);
	clean_process_status(grid);

	iprintf("snake box has %d snakes on init, %ld iterations to do", sb->nsnakes, it);

	// Main Iteration
	for(i = 0; i < it; i++) {
		int snake_n = 0;
		int current_snakes = sb->nsnakes;
		snake *snake_ary = (snake *)malloc(current_snakes*sizeof(snake)); 

		if((i+1) % 10 == 0) {
			iprintf("Iteration %ld : %d snakes" , i+1, sb->nsnakes);
		}

		global_it = i;
		if (sb->nsnakes == 0) break;

		//iprintf("============= IT: %ld contraction ============", i);
		//iprintf("Performing Deformation Step");
		while(!sbox_is_empty(sb)) {
			s = pop_snake(sb);
			snake_ary[snake_n++] = s;
			push_snake(sb_aux, s);
		}

        #pragma omp parallel for shared(snake_ary, image, s, x_max, y_max, m, p, q, interior1_ls, interior1_rs, interior2_ls, interior2_rs,  a, b, gamma, dt, vfx, vfy)
		for(snake_n = 0; snake_n < current_snakes; snake_n++) {
			deformation_step(
					image,
					snake_ary[snake_n],
					x_max, y_max,
					m, p, q, 
					interior1_ls, interior1_rs,
					interior2_ls, interior2_rs,
					a, b, gamma, dt,
					vfx, vfy,
					CONTRACT_ONLY
			);
		}

		free(snake_ary);

		if (i  == -1) {
			iprint_indicator(grid);
			iprint_indicator_full(grid);
		}

		//iprintf("Discarding Local Self Intersections");
		discard_local_self_intersections(sb_aux);

		//iprintf("Discarding too close points");
		//discard_too_close_points(sb_aux);

		//iprintf("Performing phase_1");

		while(!sbox_is_empty(sb_aux)) {
			s = pop_snake(sb_aux);
			phase_1_out(s, grid, qu);
			push_snake(sb, s);
		}

		if (i  == -1) {
			iprint_indicator(grid);
			iprint_indicator_full(grid);
		}

		if (i ==  -1) {
			while(!sbox_is_empty(sb_aux)) {
				push_snake(sb, pop_snake(sb_aux));
			}
			break;
		}


		//iprintf("Performing phase_2");

		nproc = phase_2_out(grid, qu, 5);
		// if (nproc == 0) break;

		if (grid->vlist[0].ind == 1)  { 
			iprintf("this is the one!");
			break;
		}
		if (i == -1) {
			iprint_indicator(grid);
			iprint_indicator_full(grid);
		}

		if (i == -1) {
			break;
		}



		//iprintf("Destroying old snakes");
		while(!sbox_is_empty(sb)) {
			s = pop_snake(sb);
			destroy_snake(s);
		}
		//iprintf("Collecting snakes");

		collect_snakes_out(grid, qu, q_in, q_out, sb);
		if (i == -1) {
			iprint_indicator(grid);
			iprint_indicator_full(grid);
		}
		//iprintf("Cleaning");
		clean_intersections(grid);
		clean_process_status(grid);
		//iprintf("done inter");
		//iprintf("snake box has %d snakes", sb->nsnakes);


		if (sb->nsnakes == 0) break;

		snake_n = 0;
		current_snakes = sb->nsnakes;
		snake_ary = (snake *)malloc(current_snakes*sizeof(snake)); 

		//iprintf("============= IT: %ld expansion ============", i);
		//iprintf("Performing Deformation Step");
		// Snakes are traspased from sb to sb_aux
		while(!sbox_is_empty(sb)) {
			s = pop_snake(sb);
			snake_ary[snake_n++] = s;
			push_snake(sb_aux, s);
		}

        #pragma omp parallel for shared(snake_ary, image, s, x_max, y_max, m, p, q, interior1_ls, interior1_rs, interior2_ls, interior2_rs, a, b, gamma, dt, vfx, vfy)
		for(snake_n = 0; snake_n < current_snakes; snake_n++) {
			deformation_step(
					image,
					snake_ary[snake_n],
					x_max, y_max,
					m, p, q, 
					interior1_ls, interior1_rs,
					interior2_ls, interior2_rs,
					a, b, gamma, dt,
					vfx, vfy,
					EXPAND_ONLY);
		}

		free(snake_ary);


		//iprintf("Discarding Local Self Intersections");
		discard_local_self_intersections(sb_aux);

		//iprintf("Discarding too close points");
		//discard_too_close_points(sb_aux);

		//iprintf("Performing phase_1");
		// Snakes are traspased from sb_aux to sb
		while(!sbox_is_empty(sb_aux)) {
			s = pop_snake(sb_aux);
			phase_1(s, grid, qu);
			push_snake(sb, s);
		}


		if (i == -1) {
			iprint_indicator(grid);
			iprint_indicator_full(grid);
		}
		if (i ==  -1) {
			while(!sbox_is_empty(sb_aux)) {
				push_snake(sb, pop_snake(sb_aux));
			}
			break;
		}


		//iprintf("Performing phase_2");
		nproc = phase_2(grid, qu, 5); 

		if (grid->vlist[0].ind == 1) {
			iprintf("this is the one!");
			break;
			reinitialize(grid, qu);
			iprintf("[EXTRA] Performing Extra phase_1");

			while(!sbox_is_empty(sb)) {
				s = pop_snake(sb);
				phase_1(s, grid, qu);
				push_snake(sb_aux, s);
			}
			iprintf("[EXTRA] Performing phase_2");
			nproc = phase_2(grid, qu, 5); 


			iprintf("[EXTRA]Destroying old snakes");

			while(!sbox_is_empty(sb_aux)) {
				s = pop_snake(sb_aux);
				destroy_snake(s);
			}


			//iprintf("Collecting snakes");
			collect_snakes(grid, qu, sb);

			iprint_indicator(grid);
			iprint_indicator_full(grid);

			if (i == -1) break;
		} 

		else {

			if (i == -1) {
				iprint_indicator(grid);
				iprint_indicator_full(grid);
			}

			if (i == -1) {
				break;
			}


			//iprintf("Destroying old snakes");
			while(!sbox_is_empty(sb)) {
				s = pop_snake(sb);
				destroy_snake(s);
			}

			//iprintf("Collecting snakes");
			collect_snakes(grid, qu, sb);
		}

		//iprintf("Cleaning");
		clean_intersections(grid);
		clean_process_status(grid);
		//iprintf("done inter");
		//iprintf("snake box has %d snakes", sb->nsnakes);

	} // End of the main iteration
	destroy_queue(qu);
	destroy_queue(q_in);
	destroy_queue(q_out);
	destroy_mesh(grid);

	iprintf("snake box has %d snakes", sb->nsnakes);
	if (sb->nsnakes > 0) {
		IDL_LONG max_pts = 0;

		iprintf("performing last adjust");
		while(!sbox_is_empty(sb)) {
			s = pop_snake(sb);
			if (it > 0) {
				deformation_step(
						image,
						s,
						x_max, y_max,
						70, p, 0, 
						interior1_ls, interior1_rs,
						interior2_ls, interior2_rs,
						a, b, gamma, dt,
						vfx, vfy,
						FREE_MOVE);
			}
			max_pts = s->npts > max_pts ? s->npts : max_pts;
			push_snake(sb_aux, s);
		}

		iprintf("Discarding Local Self Intersections");
		discard_local_self_intersections(sb_aux);


		iprintf("Allocating memory for return variable");
		dims[0] = max_pts+1;
		dims[1] = 2*sb_aux->nsnakes;

		data = (double *)IDL_MemAlloc(
				sizeof(double)*(max_pts+1)*(2*sb_aux->nsnakes),
				"No Memory : data",
				IDL_MSG_LONGJMP);


		iprintf("Filling the return variable");
		i = 0;
		while(!sbox_is_empty(sb_aux)) {
			s = pop_snake(sb_aux);

			data[2*i*(max_pts+1)] = (double)s->npts;
			data[(2*i+1)*(max_pts+1)] = (double)s->npts;


			for(j = 1; j < s->npts+1; j++) {
				data[j+2*i*(max_pts+1)] = s->x[j-1];
				data[j+(2*i+1)*(max_pts+1)] = s->y[j-1];

			}

			destroy_snake(s);
			i++;
		}

		iprintf("Destroying boxes");
		destroy_sbox(sb);
		destroy_sbox(sb_aux);


		iprintf("returning");
		return IDL_ImportArray(2, dims, IDL_TYP_DOUBLE, (UCHAR *)data, (IDL_ARRAY_FREE_CB)IDL_MemFree, NULL);
	}

	else {
		return IDL_StrToSTRING("No snakes");
	}
}
/* T-Snake Methods */
void deformation_step(
	IDL_INT *image, 
	snake s,
	IDL_LONG x_max, IDL_LONG y_max,
	IDL_INT m, double p, double q, 
	IDL_INT interior1_ls, IDL_INT interior1_rs,
	IDL_INT interior2_ls, IDL_INT interior2_rs,
	double a, double b, double gamma, double dt,
	double *vfx, double *vfy,
	int mode
) {
	IDL_LONG i, previous, next, npts;
	IDL_INT time;
	double *x, *y, *tot_force_x, *tot_force_y, *normal_x, *normal_y;
	double x_curr, y_curr,
		   x_limit, y_limit, 
		   real_x1, real_x2, 
		   real_y1, real_y2, 
		   tg1_x, tg1_y,
		   tg2_x, tg2_y,
		   quo, factor,
		   *alpha_x, *alpha_y, beta_x, beta_y;

	IDL_LONG x1, x2, y1, y2, x1y1, x1y2, x2y1, x2y2;
	double lvfx, lvfy, I;

	npts = s->npts;
	x = s->x;
	y = s->y;

	x_limit = (double)x_max-1;
	x_limit += 0.5;

	y_limit = (double)y_max-1;
	y_limit += 0.5;

	tot_force_x   = (double *)IDL_MemAlloc(
			sizeof(double)*npts, 
			"Could Not allocate Memory for tot_force_x and that's a shame", 
			IDL_MSG_INFO
			);

	tot_force_y   = (double *)IDL_MemAlloc(
			sizeof(double)*npts, 
			"Could Not allocate Memory for tot_force_y and that's a shame", 
			IDL_MSG_INFO
			);

	alpha_x       = (double *)IDL_MemAlloc(
			sizeof(double)*npts, 
			"Could Not allocate Memory for alpha_x and that's a shame", 
			IDL_MSG_INFO
			);

	alpha_y       = (double *)IDL_MemAlloc(
			sizeof(double)*npts,
			"Could Not allocate Memory for alpha_y and that's a shame", 
			IDL_MSG_INFO
			);

	normal_x       = (double *)IDL_MemAlloc(
			sizeof(double)*npts,
			"Could Not allocate Memory for alpha_y and that's a shame", 
			IDL_MSG_INFO
			);
	normal_y       = (double *)IDL_MemAlloc(
			sizeof(double)*npts,
			"Could Not allocate Memory for alpha_y and that's a shame", 
			IDL_MSG_INFO
			);

	//iprintf("npts = %ld", npts);
	for(time = 0; time < m; time++) {
		// This is the calculation of the Inflation Force
		// and External Force via bilinear interpolation
		//


	
		/*
		#pragma omp parallel for                   \
		private(I, i,                              \
				x1, y1, x2, y2,                    \
				real_x1, real_x2, real_y1, real_y2,\
				x1y1, x1y2, x2y1, x2y2,            \
				x_curr, y_curr,                    \
				tg1_x, tg1_y, tg2_x, tg2_y, quo,   \
				lvfx, lvfy,                        \
				previous, next, factor             \
		)                                          \
		shared(x, y, x_limit, y_limit,             \
			tot_force_x, tot_force_y,              \
			alpha_x, alpha_y,                      \
			normal_x, normal_y,                    \
			image,                                 \
			vfx, vfy,                              \
			interior1_ls, interior1_rs               \
		) 
		*/
		for(i=0; i<npts; i++) {
			//iprintf("Starting parallel section on thread %d", omp_get_thread_num());
			x_curr = x[i];
			y_curr = y[i];
			//iprintf("time = %ld, i= %ld", time, i);


			// Interpolation Calculations (lvfx, lvfy, I)
			// lvfx = local variable for the vector field in the x axis
			// lvfy = same but for the y component
			// I = image
			if ( (x_curr > 0.5 && !double_eq(x_curr, 0.5, CMP_EPSILON)) &&
				(x_curr < x_limit && !double_eq(x_curr, x_limit, CMP_EPSILON))) {
				x1 = (IDL_LONG)floor(x_curr-0.5);
				x2 = x1+1;
			}

			else if (x_curr < 0.5 || double_eq(x_curr, 0.5, CMP_EPSILON)) {
				x1 = 0;
				x2 = 0;
			}

			else {
				x1 = x_max-1;
				x2 = x_max-1;
			}


			if ((y_curr > 0.5  && !double_eq(y_curr, 0.5, CMP_EPSILON)) && 
			   (y_curr < y_limit && !double_eq(y_curr, y_limit, CMP_EPSILON))) {
				y1 = (IDL_LONG)floor(y_curr-0.5);
				y2 = y1+1;
			}

			else if (y_curr < 0.5 || double_eq(y_curr, 0.5, CMP_EPSILON)) {
				y1 = 0;
				y2 = 0;
			}

			else { 
				y1 = y_max-1;
				y2 = y_max-1;
			}

			//iprintf("I've calculed x1, x2, y1 and y2");


			//iprintf("(x1,y1) = (%ld, %ld) ; (x2, y2) = (%ld, %ld)", x1, y1, x2, y2);
			//iprintf("(x_curr, y_curr) = (%f, %f) ; (x_limit, y_limit) = (%f, %f)", x_curr, y_curr, x_limit, y_limit);


			x1y1 = x1 + x_max * y1;
			x1y2 = x1y1 + x_max;
			x2y1 = x1y1 + 1;
			x2y2 = x1y2 + 1;

			real_x1 = x1+0.5;
			real_x2 = x2+0.5;
			real_y1 = y1+0.5;
			real_y2 = y2+0.5;


			// The cases to handle are : Corners, Borders and Interior 

			// Corners and Vertical Borders 
			if ((x_curr < 0.5 || double_eq(x_curr, 0.5, CMP_EPSILON)) || 
			   (x_curr > x_limit || double_eq(x_curr, x_limit, CMP_EPSILON))) {
				// Corners
				if ((y_curr < 0.5 || double_eq(y_curr, 0.5, CMP_EPSILON)) ||
				   (y_curr > y_limit || double_eq(y_curr, y_limit, CMP_EPSILON))) {

					I = image[x1y1];

					if (x_curr < 0.0) lvfx = 5;
					else if (x_curr > x_limit + 0.5) lvfx = -5;
					else lvfx = vfx[x1y1];

					if (y_curr < 0.0) lvfy = 5;
					else if (y_curr > y_limit + 0.5) lvfy = -5;
					else lvfy = vfy[x1y1];

				}

				// Vertical Borders
				else  {

					I = image[x1y1]*(real_y2-y_curr) + image[x1y2]*(y_curr-real_y1);

					if (x_curr < 0.0) lvfx = 5;
					else if (x_curr > x_limit + 0.5) lvfx = -5;
					else lvfx = vfx[x1y1]*(real_y2-y_curr) + vfx[x1y2]*(y_curr-real_y1);


					if (y_curr < 0.0) lvfy = 5;
					else if (y_curr > y_limit + 0.5) lvfy = -5;
					else lvfy = vfy[x1y1]*(real_y2-y_curr) + vfy[x1y2]*(y_curr-real_y1);
				}

			}

			// Horizontal Borders 
			else if ((y_curr < 0.5  || double_eq(y_curr, 0.5, CMP_EPSILON)) || 
					(y_curr > y_limit || double_eq(y_curr, y_limit, CMP_EPSILON)))  {

				I = image[x1y1]*(real_x2-x_curr) + image[x2y1]*(x_curr-real_x1);

				lvfx = vfx[x1y1]*(real_x2-x_curr) + vfx[x2y1]*(x_curr-real_x1);

				if (y_curr < 0.0) lvfy = 5;
				else if (y_curr > y_limit + 0.5) lvfy = -5;
				else lvfy = vfy[x1y1]*(real_x2-x_curr) + vfy[x2y1]*(x_curr-real_x1);
			}

			// Interior 
			else {
				//iprintf("Image[%d,%d] = %d", x1, y1, image[x1y1]);
				I = image[x1y1]*(real_x2-x_curr)*(real_y2-y_curr) + 
					image[x2y1]*(x_curr-real_x1)*(real_y2-y_curr) + 
					image[x1y2]*(real_x2-x_curr)*(y_curr-real_y1) + 
					image[x2y2]*(x_curr-real_x1)*(y_curr-real_y1);

				lvfx = vfx[x1y1]*(real_x2-x_curr)*(real_y2-y_curr) + 
					vfx[x2y1]*(x_curr-real_x1)*(real_y2-y_curr) + 
					vfx[x1y2]*(real_x2-x_curr)*(y_curr-real_y1) + 
					vfx[x2y2]*(x_curr-real_x1)*(y_curr-real_y1);

				lvfy = vfy[x1y1]*(real_x2-x_curr)*(real_y2-y_curr) + 
					vfy[x2y1]*(x_curr-real_x1)*(real_y2-y_curr) + 
					vfy[x1y2]*(real_x2-x_curr)*(y_curr-real_y1) + 
					vfy[x2y2]*(x_curr-real_x1)*(y_curr-real_y1);
			}

			//iprintf("I've calculed the interpolation");

			// Normal Calculation 
			// (this works only if the polygon comes in counterclockwise 
			// order of points)

			previous = (i-1 == -1 ? npts-1 : i-1); 
			next     = (i+1)%npts;

			tg1_x = x[i] - x[previous];
			tg1_y = y[i] - y[previous];


			quo = sqrt(pow(tg1_x,2)+pow(tg1_y,2));

			tg1_x /= quo;
			tg1_y /= quo;

			tg2_x = x[next] - x[i];
			tg2_y = y[next] - y[i];

			quo = sqrt(pow(tg2_x,2)+pow(tg2_y,2));

			tg2_x /= quo;
			tg2_y /= quo;


			normal_x[i] = tg1_y + tg2_y;
			normal_y[i] = -tg1_x - tg2_x;


			// Finally the total force acting on the contour is the external field 
			// plus the inflation force that points in the normal direction of the
			// contour (outwards if I > threshold and inwards else).
			// The intensity of these forces are modifiables by the parameters p and q.

			/* old factor calculation 
			factor = (((I > (double)interior1_ls && I < (double)interior1_rs) || 
						double_eq(I, interior1_ls, CMP_EPSILON) || 
						double_eq(I, interior1_rs, CMP_EPSILON)) ? 1 : -1);
			*/

			factor = inflation_factor(I, interior1_ls, interior1_rs, interior2_ls, interior2_rs);

			tot_force_x[i] = factor * normal_x[i] * q + lvfx * p;
			tot_force_y[i] = factor * normal_y[i] * q + lvfy * p;

			//iprintf("I've calculed the inflation force");

			//iprintf("inflation[%ld] = (%f, %f) I=%f", i, factor * normal_x[i] * q, factor * normal_y[i] * q, I);
			//iprintf("vf[%ld] = (%f, %f)", i, lvfx * p, lvfy * p);


			// We also use this loop to calculate the alpha 
			alpha_x[i] = 2 * x[i] - x[previous] - x[next];
			alpha_y[i] = 2 * y[i] - y[previous] - y[next];
			//iprintf("I've calculed the alpha");
		} // End of the for of interpolating, force and alpha calculation
		//iprintf("I've finished the parallel part");


		// Now that we have all the forces calculated we proceed to make the timestep evolution
		for(i=0; i<npts; i++) {
			previous = (i-1 == -1 ? npts-1 : i-1); 
			next     = (i+1)%npts;
			//iprintf("infl+vf[%ld] = (%f, %f)", i, tot_force_x[i], tot_force_y[i]);
			beta_x = 2 * alpha_x[i] - alpha_x[previous] - alpha_x[next];
			beta_y = 2 * alpha_y[i] - alpha_y[previous] - alpha_y[next];



			tot_force_x[i] = -dt/gamma*(a*alpha_x[i]+ b*beta_x - tot_force_x[i]);
			tot_force_y[i] = -dt/gamma*(a*alpha_y[i]+ b*beta_y - tot_force_y[i]);
			

			if (double_eq(tot_force_x[i], 0, CMP_EPSILON) && 
			   double_eq(tot_force_y[i], 0, CMP_EPSILON)) 
				continue;


			switch(mode) { 
				case FREE_MOVE : 
					x[i] += tot_force_x[i]; 
					y[i] += tot_force_y[i]; 
					break;

				case EXPAND_ONLY :
					if (tot_force_x[i]*normal_x[i]+tot_force_y[i]*normal_y[i] > 0 && 
					   !double_eq(tot_force_x[i]*normal_x[i]+tot_force_y[i]*normal_y[i], 0, CMP_EPSILON)) {
						x[i] += tot_force_x[i]; 
						y[i] += tot_force_y[i]; 
					}

					break;

				case CONTRACT_ONLY :
					if (tot_force_x[i]*normal_x[i]+tot_force_y[i]*normal_y[i] < 0 && 
					!double_eq(tot_force_x[i]*normal_x[i]+tot_force_y[i]*normal_y[i],0, CMP_EPSILON)) {
						x[i] += tot_force_x[i]; 
						y[i] += tot_force_y[i]; 
					}
					break;

				default : break;

			}
		}
	}

	IDL_MemFree(
			tot_force_x, 
			"Could Not free Memory for tot_force_x", 
			IDL_MSG_INFO
			);

	IDL_MemFree(
			tot_force_y, 
			"Could Not free Memory for tot_force_x", 
			IDL_MSG_INFO
			);

	IDL_MemFree(
			alpha_x, 
			"Could Not free Memory for tot_force_x", 
			IDL_MSG_INFO
			);

	IDL_MemFree(
			alpha_y, 
			"Could Not free Memory for tot_force_x", 
			IDL_MSG_INFO
			);
	IDL_MemFree(
			normal_x, 
			"Could Not free Memory for normal_x", 
			IDL_MSG_INFO
			);
	IDL_MemFree(
			normal_y, 
			"Could Not free Memory for normal_y", 
			IDL_MSG_INFO
			);
}

double inflation_factor(
		double intensity,
		IDL_INT interior1_ls, IDL_INT interior1_rs,
		IDL_INT interior2_ls, IDL_INT interior2_rs
) { 
	
	if ((intensity >= interior1_ls && intensity <= interior1_rs) ||
		(intensity >= interior2_ls && intensity <= interior2_rs) 
	) {
		return 1;
	}

	return -1;
}

void phase_1(snake s, mesh grid, queue q) {
	IDL_LONG i, next_i, 
			 nx, ny, npts;

	double x_left, x_right, y_left, y_right, x_max, y_max, 
		   x1, y1, x2, y2, 
		   num, den, m,  hx, hy;

	double *x, *y;

	edge current_edge, final_edge, next_edge;
	vertex v1, v2, to_queue;
	int swapped;
	int x1_eq_x_left, y1_eq_y_left, x2_eq_x_right, y2_eq_y_right;


	// Saving this variables for less verbosity
	m  = grid->m;
	nx = grid->nx;
	ny = grid->ny;
	hx = grid->hx;
	hy = grid->hy;
	x_max = grid->x_max;
	y_max = grid->y_max;

	npts = s->npts;
	x = s->x;
	y = s->y;

	// We define new variables so (x_left, y_left) is the left point of the segment
	// and (x_right, y_right) is the right point of the segment
	for(i=0; i<npts; i++) {
		//iprintf("======= point %ld of %ld ========", i+1, npts);
		next_i = (i+1)%npts;

		//iprintf("x[i] = (%f,%f)  , x[i+1] = (%f,%f)", x[i], y[i], x[next_i], y[next_i]);
	
		
		if (x[i] > x[next_i]) {
			x_left  = x[next_i];
			x_right = x[i];
			y_left  = y[next_i];
			y_right = y[i];
			swapped = 1;
		}

		else {
			x_left  = x[i];
			x_right = x[next_i]; 
			y_left  = y[i];
			y_right = y[next_i]; 
			swapped = 0;
		}


		// num and den are the numerator and the denominator of the slope of the segment
		num = y_right - y_left;
		den = x_right - x_left;

		// First if the left points is the same as the right point we ignore it
		// as it is redundant
		if (double_eq(num, 0.0, CMP_EPSILON) && double_eq(den, 0.0, CMP_EPSILON)) continue; 

		// We attack the case when the segment is parallel to the x-axis 
		else if (double_eq(num, 0.0, CMP_EPSILON)) {
			// (x1, y1) and (x2, y2) are setted in such way that the segment they define is
			// the smallest rectangle in the grid that contains the segment 
			// (x_left, y_left)-(x_right, y_right).
			// i1, j1, i2, j2 are they respective indexes in the array 
			// containing the vertices 

			v1 = get_init_vertex(grid, x_left, y_left, 1);
			v2 = get_end_vertex(grid, x_right, y_right, 1);


			x1 = v1->x;
			y1 = v1->y;

			x2 = v2->x;
			y2 = v2->y;


			// Test to see where the initial and final points are
			x1_eq_x_left = double_eq(x1, x_left, CMP_EPSILON);
			y1_eq_y_left = double_eq(y1, y_left, CMP_EPSILON);
			x2_eq_x_right = double_eq(x2, x_right, CMP_EPSILON);
			y2_eq_y_right = double_eq(y2, y_right, CMP_EPSILON);

			// We now proceed to determine the initial edge (stored in current_edge)
			// and the final edge to process (stored in final_edge)

			if (swapped) {
				// Case when the segment is contained in a grid line
				if (y1_eq_y_left) { 
					// The initial point is a grid point
					if (x1_eq_x_left) {
						//iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
						x[(i+1)%npts] -= 4 * CMP_EPSILON;
						y[(i+1)%npts] += 4 * CMP_EPSILON;
						i--;
						continue;
					}

					// Or not
					else
						current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];

					// The final point is a grid point
					if (x2_eq_x_right) {
						/*
						v2->e[EDGE_NE]->inter_sign++;
						if (v2->e[EDGE_W]->inter_x == NOT_SET) {
							v2->e[EDGE_W]->inter_x = x_right;
							v2->e[EDGE_W]->inter_y = y_right;
						}

						v2->e[EDGE_E]->inter_sign++;
						if (v2->e[EDGE_W]->inter_x == NOT_SET) {
							v2->e[EDGE_W]->inter_x = x_right;
							v2->e[EDGE_W]->inter_y = y_right;
						}

						final_edge = v2->e[EDGE_N];
						*/

						iprintf("Corrected (%f, %f)", x[i], y[i]);
						x[i] -= 2 * CMP_EPSILON;
						y[i] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// Or not
					else 
						final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_NE];
				}

				else { // The line it's not a grid line
					// The initial point lies in the | edge of a |/ face.
					if (x1_eq_x_left) 
						current_edge = v1->e[EDGE_N];

					// The initial point lies in the |/ face but not in the | edge of its.
					else if (y_left > m*x_left+(y1-m*x1)||
							double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) 
						current_edge = v1->e[EDGE_NE]; 

					// The initial point lies in the /| face
					else
						current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];

					// The final point lies in the | edge of a /| face
					if (x2_eq_x_right)
						final_edge = v2->e[EDGE_S];

					// The final point lies in the |/ face but not in the / edge
					else if (y_right > m*x_right+(y2-m*x2) && 
							!double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON))
						final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];

					// The final point lies in the /| face 
					else 
						final_edge = v2->e[EDGE_SW];
				}


				// This is to test if both points lies in the same triangle
				if (final_edge->f[FACE_RIGHT] == current_edge->f[FACE_LEFT]) continue;

				// Do not get confused in the next assigment
				// it's only to preserve the condition in the following loop 
				// which "paints" the grid edges the segment intersects 
				next_edge = current_edge;

				//print_edge(final_edge, "fe");
				while(1) {
					current_edge = next_edge;
					//print_edge(current_edge, "ce");


					// This is the case when the edge is diagonal
					if (current_edge->type == EDGE_D) {
						x1 = current_edge->v[0]->x;
						y1 = current_edge->v[0]->y;
						current_edge->inter_sign++;

						if (current_edge->inter_x == NOT_SET) {
							current_edge->inter_x = (y_left-(y1-m*x1))/m;
							current_edge->inter_y = y_left;

							to_queue = current_edge->v[0];

							if (to_queue->is_queued == 0) {
								to_queue->is_queued = 1;
								queue_push(q, to_queue);
							}
						}

						else {
							to_queue = current_edge->v[0];

							if (current_edge->inter_sign > 0) {
								current_edge->inter_x = (y_left-(y1-m*x1))/m;
								current_edge->inter_y = y_left;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}
							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[1]->is_queued &&
								   !is_still_valid(current_edge->v[1])) {
									queue_remove(q, current_edge->v[1]);
									current_edge->v[1]->is_queued = 0;
								}

							}

							// else if (current_edge->inter_sign < 0) do nada
						}

						if (current_edge == final_edge) break;
						next_edge = current_edge->f[FACE_RIGHT]->e[1];
					}

					// This is the case when the edge is vertical 
					else { 
						current_edge->inter_sign++;

						if (current_edge->inter_x == NOT_SET) {
							current_edge->inter_x = current_edge->v[0]->x;
							current_edge->inter_y = y_left;

							to_queue = current_edge->v[0];
							if (to_queue->is_queued == 0) {
								to_queue->is_queued = 1;
								queue_push(q, to_queue);
							}
						}

						else {
							to_queue = current_edge->v[0];

							if (current_edge->inter_sign > 0) {
								current_edge->inter_x = current_edge->v[0]->x;
								current_edge->inter_y = y_left;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}
							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[1]->is_queued &&
								   !is_still_valid(current_edge->v[1])) {
									queue_remove(q, current_edge->v[1]);
									current_edge->v[1]->is_queued = 0;
								}
							}

							// else if (current_edge->inter_sign < 0) do nada
						}

						if (current_edge == final_edge) break;
						next_edge = current_edge->f[FACE_RIGHT]->e[0];
					}

					//Extra check to prevent segfault
					//Further programming may be needed
					if (
						   next_edge->v[0]->x > x2 + hx
						|| next_edge->v[0]->y > y2 + hy
					) {
						iprint_phase_1_segfault(0, swapped,
								x_left, y_left, 
								x_right, y_right,
								x1, y1,
								x2, y2,
								hx, hy,
								nx, ny
						);
						break;
					}
				}
			}

			else {
				// Case when the segment is contained in a grid line
				if (y1_eq_y_left) { 
					// The initial point is a grid point
					if (x1_eq_x_left)  {
						/*
						v1->e[EDGE_W]->inter_sign--;
						if (v1->e[EDGE_W]->inter_x == NOT_SET) {
							v1->e[EDGE_W]->inter_x = x_left;
							v1->e[EDGE_W]->inter_y = y_left;
						}

						v1->e[EDGE_SW]->inter_sign--;
						if (v1->e[EDGE_SW]->inter_x == NOT_SET) {
							v1->e[EDGE_SW]->inter_x = x_left;
							v1->e[EDGE_SW]->inter_y = y_left;
						}

						current_edge = v1->e[EDGE_S];
						*/

						iprintf("Corrected (%f, %f)", x[i], y[i]);
						x[i] -= 2 * CMP_EPSILON;
						y[i] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// Or not
					else
						current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_SW];

					// The final point is a grid point
					if (x2_eq_x_right) {
						/*
						v2->e[EDGE_S]->inter_sign--;
						if (v2->e[EDGE_S]->inter_x == NOT_SET) {
							v2->e[EDGE_S]->inter_x = x_right;
							v2->e[EDGE_S]->inter_y = y_right;
						}

						v2->e[EDGE_E]->inter_sign++;
						if (v2->e[EDGE_E]->inter_x == NOT_SET) {
							v2->e[EDGE_E]->inter_x = x_right;
							v2->e[EDGE_E]->inter_y = y_right;
						}

						final_edge = v2->e[EDGE_SW];
						*/

						iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
						x[(i+1)%npts] -= 2 * CMP_EPSILON;
						y[(i+1)%npts] += 2 * CMP_EPSILON;
						i--;
						continue;

					}

					// Or not
					else 
						final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];

				}

				else { // The line it's not a grid line
					// The initial point lies in the | edge of a |/ face.

					if (x1_eq_x_left) 
						current_edge = v1->e[EDGE_N];

					// The initial point lies in the |/ face but not in the | edge of its.
					else if (y_left >= m*x_left+(y1-m*x1)) 
						current_edge = v1->e[EDGE_NE]; 

					// The initial point lies in the /| face
					else
						current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];

					// The final point lies in the | edge of a /| face
					if (x2_eq_x_right)
						final_edge = v2->e[EDGE_S];

					// The final point lies in the |/ face but not in the / edge
					else if (y_right > m*x_right+(y2-m*x2)) 
						final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];

					// The final point lies in the /| face 
					else 
						final_edge = v2->e[EDGE_SW];
				}


				// This is to test if both points lies in the same triangle
				if (final_edge->f[FACE_RIGHT] == current_edge->f[FACE_LEFT]) continue;

				// Do not get confused in the next assigment
				// it's only to preserve the condition in the following loop 
				// which "paints" the grid edges the segment intersects 
				next_edge = current_edge;

				//print_edge(final_edge, "fe");
				while(1) {
					current_edge = next_edge;
					//print_edge(current_edge, "ce");

					// This is the case when the edge is diagonal
					if (current_edge->type == EDGE_D) {
						x1 = current_edge->v[0]->x;
						y1 = current_edge->v[0]->y;
						current_edge->inter_sign--;

						if (current_edge->inter_x == NOT_SET ) {
							current_edge->inter_x = (y_left-(y1-m*x1))/m;
							current_edge->inter_y = y_left;

							to_queue = current_edge->v[1];

							if (to_queue->is_queued == 0) {
								to_queue->is_queued = 1;
								queue_push(q, to_queue);
							}

						}

						else {
							to_queue = current_edge->v[1];

							if (current_edge->inter_sign < 0) {
								current_edge->inter_x = (y_left-(y1-m*x1))/m;
								current_edge->inter_y = y_left;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}
							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[0]->is_queued &&
										!is_still_valid(current_edge->v[0])) {
									queue_remove(q, current_edge->v[0]);
									current_edge->v[0]->is_queued = 0;
								}

							}

							// else if (current_edge->inter_sign > 0) do nada
						}

						if (current_edge == final_edge) break;

						next_edge = current_edge->f[FACE_RIGHT]->e[1];

					}

					// This is the case when the edge is vertical 
					else { 
						current_edge->inter_sign--;

						if (current_edge->inter_x == NOT_SET) {
							current_edge->inter_x = current_edge->v[0]->x;
							current_edge->inter_y = y_left;

							to_queue = current_edge->v[1];

							if (to_queue->is_queued == 0) {
								to_queue->is_queued = 1;
								queue_push(q, to_queue);
							}
						}

						else {

							to_queue = current_edge->v[1];

							if (current_edge->inter_sign < 0) {
								current_edge->inter_x = current_edge->v[0]->x;
								current_edge->inter_y = y_left;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}
							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[0]->is_queued &&
										!is_still_valid(current_edge->v[0])) {
									queue_remove(q, current_edge->v[0]);
									current_edge->v[0]->is_queued = 0;
								}

							}

							// else if (current_edge->inter_sign > 0) do nada
						}

						if (current_edge == final_edge) break;
						next_edge = current_edge->f[FACE_RIGHT]->e[0];
					}

					//Extra check to prevent segfault
					//Further programming may be needed
					if (
						   next_edge->v[0]->x > x2 + hx
						|| next_edge->v[0]->y > y2 + hy
					) {
						iprint_phase_1_segfault(0, swapped,
								x_left, y_left, 
								x_right, y_right,
								x1, y1,
								x2, y2,
								hx, hy,
								nx, ny
						);
						break;
					}
				}
			}
		}

		// This is the case when the segment is parallel to the y-axis
		else if (double_eq(den, 0.0, CMP_EPSILON)) { 
			// Switch the variables so (x_left,y_left) is the point on the bottom
			// (there is NO left and right in this case since it is 
			// parallel to the y-axis
			if (y_left > y_right) {
				double aux;
				aux = y_left;
				y_left = y_right;
				y_right  = aux;

				aux = x_left;
				x_left = x_right;
				x_right = x_left;
				swapped = 1;
			}

			// (x1, y1) and (x2, y2) are setted in such way 
			// that the segment they define is the smallest rectangle 
			// in the grid that contains 
			// the segment x_left, y_left)-(x_right, y_right)
			//
			// i1, j1, i2, j2 are they respective indexes 
			// in the array containing the vertices 


			v1 = get_init_vertex(grid, x_left, y_left, 1);
			v2 = get_end_vertex(grid, x_right, y_right, 1);

			x1 = v1->x;
			y1 = v1->y;

			x2 = v2->x;
			y2 = v2->y;

			// Precalculate the comparisons for economy
			x1_eq_x_left = double_eq(x1, x_left,CMP_EPSILON);
			y1_eq_y_left = double_eq(y1, y_left, CMP_EPSILON);
			x2_eq_x_right = double_eq(x2, x_right, CMP_EPSILON);
			y2_eq_y_right = double_eq(y2, y_right, CMP_EPSILON);


			if (swapped) {
				// Determine the initial edge (stored in current_edge)
				// and the final edge to process (stored in final_edge)

				// The segment is contained in a grid line
				if (x1_eq_x_left) {
					// The initial point is a grid point
					if (y1_eq_y_left) {
						/*
						// Mark the south edge as intersected
						v1->e[EDGE_S]->inter_sign--;
						if (v1->e[EDGE_S]->inter_x == NOT_SET) {
							v1->e[EDGE_S]->inter_x = x_left;
							v1->e[EDGE_S]->inter_y = y_left;
						}

						// Mark the south west edge as intersected
						v1->e[EDGE_SW]->inter_sign--;
						if (v1->e[EDGE_SW]->inter_x == NOT_SET) {
							v1->e[EDGE_SW]->inter_x = x_left;
							v1->e[EDGE_SW]->inter_y = y_left;
						}

						current_edge = v1->e[EDGE_W];
						*/

						iprintf("Corrected (%f, %f)", x[i], y[i]);
						x[i] -= 2 * CMP_EPSILON;
						y[i] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// Or not
					else
						current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_SW];

					// The final point is a grid point
					if (y2_eq_y_right) {
						/*
						// Mark the north edge as intersected
						v2->e[EDGE_N]->inter_sign++;
						if (v2->e[EDGE_N]->inter_x == NOT_SET) {
							v2->e[EDGE_N]->inter_x = x_right;
							v2->e[EDGE_N]->inter_y = y_right;
						}

						// Mark the west edge as intersected
						v2->e[EDGE_W]->inter_sign--;
						if (v2->e[EDGE_W]->inter_x == NOT_SET) {
							v2->e[EDGE_W]->inter_x = x_right;
							v2->e[EDGE_W]->inter_y = y_right;
						}

						final_edge = v2->e[EDGE_SW];
						*/

						iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
						x[(i+1)%npts] -= 2 * CMP_EPSILON;
						y[(i+1)%npts] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// Or not
					else 
						final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
				}


				// Case when the line is not a grid line
				else {
					// The initial point lies in the _ edge of a /| face.
					if (y1_eq_y_left) 
						current_edge = v1->e[EDGE_E];

					// The initial point lies in the /| face but not in the _ edge of its.
					else if (y_left < m*x_left+(y1-m*x1) ||
							double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) 
						current_edge = v1->e[EDGE_NE]; 

					// The initial point lies in the |/ face
					else
						current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E];

					// The final point lies in the _ edge of a |/ face
					if (y2_eq_y_right)
						final_edge = v2->e[EDGE_W];

					// The final point lies in the |/ face but not in the _ edge
					else if (y_right > m*x_right+(y2-m*x2) ||
							double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) 
						final_edge = v2->e[EDGE_SW];

					// The final point lies in the /| face 
					else 
						final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
				}

				if (
						final_edge->f[FACE_UP] == current_edge->f[FACE_DOWN] && 
						final_edge->f[FACE_UP] != NULL
				  ) 
					continue;

				// Do not get confused in the next assigment
				// it's only to preserve the condition in the following loop 
				// that "paints" the grid edges that the segment intersects 

				next_edge = current_edge;

				//print_edge(final_edge, "fe");
				while(1) { 
					current_edge = next_edge;
					//print_edge(current_edge, "ce");

					// Case when the edge is Horizontal
					if (current_edge->type == EDGE_H) {
						to_queue = current_edge->v[1];
						current_edge->inter_sign--;

						if (current_edge->inter_x == NOT_SET) {
							current_edge->inter_x = x_left;
							current_edge->inter_y = current_edge->v[0]->y;


							if (to_queue->is_queued == 0) {
								to_queue->is_queued = 1;
								queue_push(q, to_queue);
							}
						}

						else {
							if (current_edge->inter_sign < 0) {
								current_edge->inter_x = x_left;
								current_edge->inter_y = current_edge->v[0]->y;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}

							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[0]->is_queued &&
								   !is_still_valid(current_edge->v[0])) {
									queue_remove(q, current_edge->v[0]);
									current_edge->v[0]->is_queued = 0;
								}
							}

							// else if (current_edge->inter_sign > 0) do nada
						}

						if (current_edge == final_edge) 
							break;

						next_edge = current_edge->f[FACE_UP]->e[2];
					}

					else { // Case when the edge is Diagonal
						x1 = current_edge->v[0]->x;
						y1 = current_edge->v[0]->y;

						current_edge->inter_sign--;
						to_queue = current_edge->v[1];

						if (current_edge->inter_x == NOT_SET) {
							current_edge->inter_x = x_left;
							current_edge->inter_y = m*x_left+(y1-m*x1);


							if (to_queue->is_queued == 0) {
								to_queue->is_queued = 1;
								queue_push(q, to_queue);
							}
						}

						else {

							if (current_edge->inter_sign < 0) {
								current_edge->inter_x = x_left;
								current_edge->inter_y = m*x_left+(y1-m*x1);
							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[0]->is_queued &&
										!is_still_valid(current_edge->v[0])) {
									queue_remove(q, current_edge->v[0]);
									current_edge->v[0]->is_queued = 0;
								}
							}

							// else if (current_edge->inter_sign > 0) do nada
						}

						if (current_edge == final_edge) 
							break;

						next_edge = current_edge->f[FACE_UP]->e[1];
					}

					//Extra check to prevent segfault
					//Further programming may be needed
					if (next_edge->v[0]->x > x2 + hx) {
						iprint_phase_1_segfault(DBL_MAX, swapped,
								x_left, y_left, 
								x_right, y_right,
								x1, y1,
								x2, y2,
								hx, hy,
								nx, ny
						);

						break;
					}
				}
			}

			else { // if (not swapped)
				// Determine the initial edge (stored in current_edge)
				// and the final edge to process (stored in final_edge)

				// The segment is contained in a grid line
				if (x1_eq_x_left) {
					// The initial point is a grid point
					if (y1_eq_y_left)  {
						/*
						// Mark the south edge as intersected
						v1->e[EDGE_S]->inter_sign--;
						if (v1->e[EDGE_S]->inter_x == NOT_SET) {
							v1->e[EDGE_S]->inter_x = x_left;
							v1->e[EDGE_S]->inter_y = y_left;
						}

						// Mark the east edge as intersected
						v1->e[EDGE_E]->inter_sign++;
						if (v1->e[EDGE_E]->inter_x == NOT_SET) {
							v1->e[EDGE_E]->inter_x = x_left;
							v1->e[EDGE_E]->inter_x = y_left;
						}

						current_edge = v1->e[EDGE_NE];
						*/

						iprintf("Corrected (%f, %f)", x[i], y[i]);
						x[i] -= 2 * CMP_EPSILON;
						y[i] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// Or not
					else
						current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E];

					// The final point is a grid point
					if (y2_eq_y_right) {
						/*
						v2->e[EDGE_N]->inter_sign++;
						if (v2->e[EDGE_N]->inter_x == NOT_SET) {
							v2->e[EDGE_N]->inter_x = x_right;
							v2->e[EDGE_N]->inter_y = y_right;
						}

						v2->e[EDGE_NE]->inter_sign++;
						if (v2->e[EDGE_NE]->inter_x == NOT_SET) {
							v2->e[EDGE_NE]->inter_x = x_right;
							v2->e[EDGE_NE]->inter_y = y_right;
						}

						final_edge = v2->e[EDGE_E];
						*/

						iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
						x[(i+1)%npts] -= 2 * CMP_EPSILON;
						y[(i+1)%npts] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// Or not
					else 
						final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_NE];
				}


				// Case when the line is no a grid line
				else {
					// The initial point lies in the _ edge of a /| face.
					if (y1_eq_y_left) 
						current_edge = v1->e[EDGE_E];

					// The initial point lies in the /| face but not in the _ edge of its.
					else if (y_left < m*x_left+(y1-m*x1) ||
							double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) 
						current_edge = v1->e[EDGE_NE]; 

					// The initial point lies in the |/ face
					else
						current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E];

					// The final point lies in the _ edge of a |/ face
					if (y2_eq_y_right)
						final_edge = v2->e[EDGE_W];

					// The final point lies in the |/ face but not in the _ edge
					else if (y_right > m*x_right+(y2-m*x2) ||
							double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) 
						final_edge = v2->e[EDGE_SW];

					// The final point lies in the /| face 
					else 
						final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
				}

				if (
						final_edge->f[FACE_UP] == current_edge->f[FACE_DOWN] && 
						final_edge->f[FACE_UP] != NULL
				  ) 
					continue;

				// Do not get confused in the next assigment
				// it's only to preserve the condition in the following loop 
				// that "paints" the grid edges that the segment intersects 

				next_edge = current_edge;

				//print_edge(final_edge, "fe");
				while(1) { 
					current_edge = next_edge;
					//print_edge(current_edge, "ce");

					// Case when the edge is Horizontal
					if (current_edge->type == EDGE_H) {
						current_edge->inter_sign++;

						if (current_edge->inter_x == NOT_SET) {
							current_edge->inter_x = x_left;
							current_edge->inter_y = current_edge->v[0]->y;

							to_queue = current_edge->v[0];

							if (to_queue->is_queued == 0) {
								to_queue->is_queued = 1;
								queue_push(q, to_queue);
							}
						}

						else {
							to_queue = current_edge->v[0];

							if (current_edge->inter_sign > 0) {
								current_edge->inter_x = x_left;
								current_edge->inter_y = current_edge->v[0]->y;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}

							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[1]->is_queued &&
										!is_still_valid(current_edge->v[1])) {
									queue_remove(q, current_edge->v[1]);
									current_edge->v[1]->is_queued = 0;
								}

							}

							// else if (current_edge->inter_sign > 0) do nada
						}

						if (current_edge == final_edge) 
							break;

						next_edge = current_edge->f[FACE_UP]->e[2];
					}

					else { // Case when the edge is Diagonal
						x1 = current_edge->v[0]->x;
						y1 = current_edge->v[0]->y;

						current_edge->inter_sign++;

						if (current_edge->inter_x == NOT_SET) {
							current_edge->inter_x = x_left;
							current_edge->inter_y = m*x_left+(y1-m*x1);

							to_queue = current_edge->v[0];

							if (to_queue->is_queued == 0) {
								to_queue->is_queued = 1;
								queue_push(q, to_queue);
							}
						}

						else {

							to_queue = current_edge->v[0];

							if (current_edge->inter_sign > 0) {
								current_edge->inter_x = x_left;
								current_edge->inter_y = m*x_left+(y1-m*x1);
							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[1]->is_queued &&
								   !is_still_valid(current_edge->v[1])) {
									queue_remove(q, current_edge->v[1]);
									current_edge->v[1]->is_queued = 0;
								}
							}

							// else if (current_edge->inter_sign < 0) do nada
						}

						if (current_edge == final_edge) 
							break;

						next_edge = current_edge->f[FACE_UP]->e[1];
					}
					
					//Extra check to prevent segfault
					//Further programming may be needed
					if (
						   next_edge->v[0]->x > x2 + hx
						|| next_edge->v[0]->y > y2 + hy
					) {
						iprint_phase_1_segfault(DBL_MAX, swapped,
								x_left, y_left, 
								x_right, y_right,
								x1, y1,
								x2, y2,
								hx, hy,
								nx, ny
						);
						break;
					}
				}
			}
		}
		// Case when the segment is oblique (p != 0 && p != infty)
		else {
			double current_x, current_y;
			double t1, t2;
			double p = num/den;
			face iface, fface;

			//iprintf("p = %f", p);


			if (swapped) {
				// We separate the cases when the segment has
				// slope and when it has negative slope. Notice that
				// the case whe it has zero slope has already been considered
				// in the case when num = 0;
				if (p > 0) {
					// (x1, y1) and (x2, y2) are setted in such way that the segment they define is
					// the smallest rectangle in the grid that contains the segment 
					// (x_left, y_left)-(x_right, y_right).
					// i1, j1, i2, j2 are they respective indexes in the array containing the vertices 


					v1 = get_init_vertex(grid, x_left, y_left, 1);
					v2 = get_end_vertex(grid, x_right, y_right, 1);

					x1 = v1->x;
					y1 = v1->y;

					x2 = v2->x;
					y2 = v2->y;


					x1_eq_x_left = double_eq(x1, x_left, CMP_EPSILON);
					y1_eq_y_left = double_eq(y1, y_left, CMP_EPSILON);
					x2_eq_x_right = double_eq(x2, x_right, CMP_EPSILON);
					y2_eq_y_right = double_eq(y2, y_right, CMP_EPSILON);

					//iprintf("x_left = %f, x1 = %f , x_left == x1 ? %d", x_left, x1, x1_eq_x_left);
					//iprintf("y_left = %f, y1 = %f , y_left == y1 ? %d", y_left, y1, y1_eq_y_left);
					//iprintf("x_right = %f, x2 = %f , x_right == x2 ? %d", x_right, x2, x2_eq_x_right);
					//iprintf("y_right = %f, y2 = %f , y_right == y2 ? %d", y_right, y2, y2_eq_y_right); 

					// Determine the initial edge (stored in current_edge)
					// and the final edge to process (stored in final_edge)

					if (double_eq(p,m, CMP_EPSILON)) {
						// The initial point is the grid point (x1, y1)

						if (x1_eq_x_left && y1_eq_y_left)  {
							/*
							current_edge = v1->e[EDGE_W];

							// Mark the SW edge of v1
							v1->e[EDGE_SW]->inter_sign--;
							if (v1->e[EDGE_SW]->inter_x == NOT_SET ||
							   v1->e[EDGE_SW]->inter_sign < 0) {

								v1->e[EDGE_SW]->inter_x = x_left;
								v1->e[EDGE_SW]->inter_y = y_left;
							}

							else if (v1->e[EDGE_SW]->inter_sign == 0) {
								v1->e[EDGE_SW]->inter_x  = NOT_SET;
								v1->e[EDGE_SW]->inter_y  = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign > 0) do nada

							current_x = x1;
							current_y = y1;
							iface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
							x[(i+1)%npts] -= 2 * CMP_EPSILON;
							y[(i+1)%npts] += 2 * CMP_EPSILON;
							i--;
							continue;
						}

						// The initial point lies in the | edge of a |/ face.
						else if (x1_eq_x_left) {
							current_edge = v1->e[EDGE_N];

							current_x = x1;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the _ edge of a /| face.
						else if (y1_eq_y_left) {
							current_edge = v1->e[EDGE_E];
							current_x = x_left;
							current_y = y1;
							iface = NULL;
						}

						// The initial point lies in the / edge
						else if (double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) {
							v1->e[EDGE_NE]->inter_sign--;
							if (v1->e[EDGE_NE]->inter_x == NOT_SET ||
							   v1->e[EDGE_NE]->inter_sign < 0) {

								v1->e[EDGE_NE]->inter_x = x_left;
								v1->e[EDGE_NE]->inter_y = y_left;
							}

							else if (v1->e[EDGE_NE]->inter_sign == 0) {
								v1->e[EDGE_NE]->inter_x  = NOT_SET;
								v1->e[EDGE_NE]->inter_y  = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign < 0) do nada
							
							current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E];
							current_x = x_left;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the |/ face 
						// but not in the | edge nor the / edge of its.
						else if (y_left > m*x_left+(y1-m*x1)) {
							t1 = (y1+hy-y_left)/p;
							current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E]; 
							current_x = x_left + t1;
							current_y = v1->e[EDGE_N]->v[1]->y;
							iface = current_edge->f[FACE_DOWN];
						}


						// The initial point lies in the /| face
						else { // if (y_left < m*x_left+(y1-m*x1)) 
							t1 = x1+hx-x_left;

							current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];
							current_x = x_left + t1;
							current_y = y_left + t1*p;
							iface = current_edge->f[FACE_LEFT];

						}

						// We now determine the final edge to process
						if (x2_eq_x_right && y2_eq_y_right) {
							/*
							final_edge = v2->e[EDGE_N];


							// Mark the NE edge of v2
							v2->e[EDGE_NE]->inter_sign++;
							if (v2->e[EDGE_NE]->inter_x == NOT_SET ||
							   v2->e[EDGE_NE]->inter_sign > 0) {

								v2->e[EDGE_NE]->inter_x = x_right;
								v2->e[EDGE_NE]->inter_y = y_right;

							}

							else if (v2->e[EDGE_NE]->inter_sign == 0) {
								v2->e[EDGE_NE]->inter_x = NOT_SET;
								v2->e[EDGE_NE]->inter_y = NOT_SET;
							}

							// else if (v2->e[EDGE_NE] < 0) do nada


							fface = NULL;
							*/
							iprintf("Corrected (%f, %f)", x[i], y[i]);
							x[i] -= 2 * CMP_EPSILON;
							y[i] += 2 * CMP_EPSILON;
							i--;
							continue;
						}

						// The final point lies in the | edge of a /| face
						else if (x2_eq_x_right) {
							final_edge = v2->e[EDGE_S];
							fface = NULL;
						}

						// The final point lies in the _ edge of a |/ face
						else if (y2_eq_y_right) {
							final_edge = v2->e[EDGE_W];
							fface = NULL;
						}

						// The final point  lies in the / edge
						else if (double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) {
							v2->e[EDGE_SW]->inter_sign++;
							if (v2->e[EDGE_SW]->inter_x == NOT_SET ||
							   v2->e[EDGE_SW]->inter_sign > 0) {

								v2->e[EDGE_SW]->inter_x = x_left;
								v2->e[EDGE_SW]->inter_y = y_left;
							}

							else if (v2->e[EDGE_SW]->inter_sign == 0) {
								v2->e[EDGE_SW]->inter_x  = NOT_SET;
								v2->e[EDGE_SW]->inter_y  = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign < 0) do nada
							final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
							fface = NULL;
						}

						// The final point lies in the |/ face but not in the / edge
						else if (y_right > m*x_right+(y2-m*x2)) {
							final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
							fface = final_edge->f[FACE_RIGHT];
						}


						// The final point lies in the /| face  but not in the / edge
						// nor the | edge
						else { // if (y_right < m*x_right+(y2-m*x2)) 
							final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
							fface = final_edge->f[FACE_UP];
						}


						// This is to test if the points lay in the
						// same triangle
						if (iface != NULL && fface != NULL && iface == fface) continue;

						next_edge = current_edge;

						//print_edge(final_edge, "fe");

						while(1) {
							current_edge = next_edge;
							//print_edge(current_edge, "ce");

							if (current_edge->type == EDGE_V) {

								current_edge->inter_sign++;
								to_queue = current_edge->v[0];


								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign > 0) {

									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {

									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[1]->is_queued &&
									   !is_still_valid(current_edge->v[1])) {
										queue_remove(q, current_edge->v[1]);
										current_edge->v[1]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign < 0) do nada


								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->y - current_y)/p;

								current_x += t1;
								current_y = current_edge->v[1]->y;

								next_edge = current_edge->v[1]->e[EDGE_E];
							}


							else if (current_edge->type == EDGE_D) {
								next_edge = current_edge->v[1]->e[EDGE_E];
							}

							else { // current_edge->type == EDGE_H
								current_edge->inter_sign--;

								to_queue = current_edge->v[1];

								if (current_edge->inter_x == NOT_SET ||
								    current_edge->inter_sign < 0 ) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[0]->is_queued &&
											!is_still_valid(current_edge->v[0])) {
										queue_remove(q, current_edge->v[0]);
										current_edge->v[0]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign > 0) do nada


								if (current_edge == final_edge) break;

								t1 = current_edge->v[1]->x - current_x;

								current_x += t1;
								current_y += t1*p;

								next_edge = current_edge->v[1]->e[EDGE_N];

							}

							//Extra check to prevent segfault
							//Further programming may be needed
							if (
								   next_edge->v[0]->x > x2 + hx
								|| next_edge->v[0]->y > y2 + hy
							) {
								iprint_phase_1_segfault(p, swapped,
										x_left, y_left, 
										x_right, y_right,
										x1, y1,
										x2, y2,
										hx, hy,
										nx, ny
								);
								break;
							}
						}
					}
					else if (p>m) {
						// The initial point is the grid point (x1, y1)
						if (x1_eq_x_left && y1_eq_y_left)  {
							/*
							current_edge = v1->e[EDGE_W];

							// Mark the SW edge of v1
							v1->e[EDGE_SW]->inter_sign--;
							if (v1->e[EDGE_SW]->inter_x == NOT_SET ||
							   v1->e[EDGE_SW]->inter_sign < 0) {

								v1->e[EDGE_SW]->inter_x = x_left;
								v1->e[EDGE_SW]->inter_y = y_left;
							}

							else if (v1->e[EDGE_SW]->inter_sign == 0) {
								v1->e[EDGE_SW]->inter_x = NOT_SET;
								v1->e[EDGE_SW]->inter_y = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign < 0) do nada

							current_x = x1;
							current_y = y1;
							iface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
							x[(i+1)%npts] -= 2 * CMP_EPSILON;
							y[(i+1)%npts] += 2 * CMP_EPSILON;
							i--;
							continue;
						}
						// The initial point lies in the | edge of a |/ face.
						else if (x1_eq_x_left) {
							current_edge = v1->e[EDGE_N];
							current_x = x1;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the _ edge of a /| face.
						else if (y1_eq_y_left) {
							current_edge = v1->e[EDGE_E];
							current_x = x_left;
							current_y = y1;
							iface = NULL;
						}


						// The initial point lies in the / edge
						else if (double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) {
							current_edge = v1->e[EDGE_NE];
							current_x = x_left;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the |/ face 
						// but not in the | edge nor the / edge of its.
						else if (y_left > m*x_left+(y1-m*x1)) {
							t1 = (y1+hy-y_left)/p;
							current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E]; 
							current_x = x_left + t1;
							current_y = v1->e[EDGE_N]->v[1]->y;
							iface = current_edge->f[FACE_DOWN];
						}


						// The initial point lies in the /| face
						else { // if (y_left < m*x_left+(y1-m*x1)) 
							t1 = x1+hx-x_left;
							t2 = ((x1-x_left)-(y1-y_left)/m)/(1-p/m);


							if (double_eq(t1, t2, CMP_EPSILON)) {
								current_edge = v1->e[EDGE_NE];
								current_x = x_left + t2;
								current_y = y_left + t2*p;
								iface = current_edge->f[FACE_DOWN];
							}

							else if (t1 < t2) {
								current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];
								current_x = x_left + t1;
								current_y = y_left + t1*p;
								iface = current_edge->f[FACE_LEFT];
							}

							else {//  if (t1 > t2)
								current_edge = v1->e[EDGE_NE];
								current_x = x_left + t2;
								current_y = y_left + t2*p;
								iface = current_edge->f[FACE_DOWN];
							}
						}

						// We now determine the final edge to process
						if (x2_eq_x_right && y2_eq_y_right) { 
							/*
							final_edge = v2->e[EDGE_N];

							// Mark the NE edge of v2
							v2->e[EDGE_NE]->inter_sign++;
							if (v2->e[EDGE_NE]->inter_x == NOT_SET ||
									v2->e[EDGE_NE]->inter_sign > 0) {

								v2->e[EDGE_NE]->inter_x = x_right;
								v2->e[EDGE_NE]->inter_y = y_right;
							}

							else if (v2->e[EDGE_NE]->inter_sign == 0) {
								v2->e[EDGE_NE]->inter_x = NOT_SET;
								v2->e[EDGE_NE]->inter_y = NOT_SET;
							}

							// else if (v2->e[EDGE_NE]->inter_sign < 0) do nada

							fface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[i], y[i]);
							x[i] -= 2 * CMP_EPSILON;
							y[i] += 2 * CMP_EPSILON;
							i--;
							continue;
						}

						// The final point lies in the | edge of a /| face
						else if (x2_eq_x_right)  {
							final_edge = v2->e[EDGE_S];
							fface = NULL;
						}

						// The final point lies in the _ edge of a |/ face
						else if (y2_eq_y_right) {
							final_edge = v2->e[EDGE_W];
							fface = NULL;
						}

						// The final point  lies in the / edge
						else if (double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) {
							final_edge = v2->e[EDGE_SW];
							fface = NULL;
						}

						// The final point lies in the |/ face but not in the / edge
						// or the _ edge
						else if (y_right > m*x_right+(y2-m*x2)) {
							t1 = ((x2-x_right)-(y2-y_right)/m)/(1-p/m);
							t2 = x2-hx-x_right;


							if (double_eq(t1, t2, CMP_EPSILON)) {
								final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
								fface = final_edge->f[FACE_RIGHT];
							}

							else if (fabs(t1) < fabs(t2)) {
								final_edge = v2->e[EDGE_SW];
								fface = final_edge->f[FACE_UP]; 
							}

							else  { // if (t1 > t2)
								final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
								fface = final_edge->f[FACE_RIGHT];
							}
						}


						// The final point lies in the /| face  but not in the / edge
						// nor the | edge
						else  { // if (y_right < m*x_right+(y2-m*x2)) 
							final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
							fface = final_edge->f[FACE_UP];
						}

						if (iface != NULL && fface != NULL && iface == fface) continue;

						next_edge = current_edge;

						//print_edge(final_edge, "fe");

						while(1) {
							current_edge = next_edge;
							//print_edge(current_edge, "ce");

							if (current_edge->type == EDGE_V) {
								current_edge->inter_sign++;
								to_queue = current_edge->v[0];


								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign > 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;


									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[1]->is_queued &&
									   !is_still_valid(current_edge->v[1])) {
										queue_remove(q, current_edge->v[1]);
										current_edge->v[1]->is_queued = 0;
									}

								}

								// else if (current_edge->inter_sign < 0) do nada

								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->y - current_y)/p;

								current_x += t1;
								current_y = current_edge->v[1]->y;

								next_edge = current_edge->v[1]->e[EDGE_E];
							}

							else if (current_edge->type == EDGE_D) {
								current_edge->inter_sign--;
								to_queue = current_edge->v[1];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign < 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;

										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[0]->is_queued &&
									   !is_still_valid(current_edge->v[0])) {
										queue_remove(q, current_edge->v[0]);
										current_edge->v[0]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign > 0) do nada


								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->y - current_y)/p;

								current_x += t1;
								current_y = current_edge->v[1]->y;

								next_edge = current_edge->v[1]->e[EDGE_W];
							}

							else { // current_edge->type == EDGE_H
								to_queue = current_edge->v[1];

								current_edge->inter_sign--;

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign < 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;
									if (current_edge->v[0]->is_queued &&
											!is_still_valid(current_edge->v[0])) {
										queue_remove(q, current_edge->v[0]);
										current_edge->v[0]->is_queued = 0;
									}

								}

								// else if (current_edge->inter_sign < 0 ) do nada

								if (current_edge == final_edge) break;

								t1 = current_edge->v[1]->x - current_x;
								t2 = current_edge->v[0]->x - current_x;
								t2 -= (current_edge->v[0]->y - current_y)/m;
								t2 /= 1-p/m;

								if (double_eq(t1, 0.0, CMP_EPSILON)) {
									next_edge = current_edge->v[1]->e[EDGE_N];
								}

								else if (double_eq(t1,t2, CMP_EPSILON)) {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[0]->e[EDGE_NE];

								}
								else if (t1 < t2) {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[1]->e[EDGE_N];
								}

								else if (t1 > t2) {
									current_x += t2;
									current_y += t2*p;
									next_edge = current_edge->v[0]->e[EDGE_NE];
								}

							}
							
							//Extra check to prevent segfault
							//Further programming may be needed
							if (
								   next_edge->v[0]->x > x2 + hx
								|| next_edge->v[0]->y > y2 + hy
							) {
								iprint_phase_1_segfault(p, swapped,
										x_left, y_left, 
										x_right, y_right,
										x1, y1,
										x2, y2,
										hx, hy,
										nx, ny
								);
								break;
							}
						}
					}

					else { // if (0<p<m)
						if (x1_eq_x_left && y1_eq_y_left)  {
							/*
							current_edge = v1->e[EDGE_W];

							// Mark the SW edge of v1
							v1->e[EDGE_SW]->inter_sign--;
							if (v1->e[EDGE_SW]->inter_x == NOT_SET ||
							   v1->e[EDGE_SW]->inter_sign < 0) {

								v1->e[EDGE_SW]->inter_x = x_left;
								v1->e[EDGE_SW]->inter_y = y_left;
							}

							else if (v1->e[EDGE_SW]->inter_sign == 0) {
								v1->e[EDGE_SW]->inter_x = NOT_SET;
								v1->e[EDGE_SW]->inter_y = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign < 0) do nada

							current_x = x1;
							current_y = y1;
							iface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
							x[(i+1)%npts] -= 2 * CMP_EPSILON;
							y[(i+1)%npts] += 2 * CMP_EPSILON;
							i--;
							continue;
						}

						// The initial point lies in the | edge of a |/ face.
						else if (x1_eq_x_left) {
							current_edge = v1->e[EDGE_N];
							current_x = x1;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the _ edge of a /| face.
						else if (y1_eq_y_left) {
							current_edge = v1->e[EDGE_E];
							current_x = x_left;
							current_y = y1;
							iface = NULL;
						}

						// The initial point lies in the / edge
						else if (double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) {
							current_edge = v1->e[EDGE_NE];
							current_x = x_left;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the |/ face 
						// but not in the | edge nor the / edge of its.
						else if (y_left > m*x_left+(y1-m*x1)) {
							t1 = ((x1-x_left)-(y1-y_left)/m)/(1-p/m);
							t2 = (y1+hy-y_left)/p;

							if (double_eq(t1, t2, CMP_EPSILON)) {
								current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E];
								current_x = x_left + t2;
								current_y = y_left + t2*p;
								iface = current_edge->f[FACE_DOWN];
							}

							else if (t1 < t2) {
								current_edge = v1->e[EDGE_NE]; 
								current_x = x_left + t1;
								current_y = y_left + t1*p;
								iface = current_edge->f[FACE_LEFT];
							}

							else { // if (t1 > t2) 
								current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E];
								current_x = x_left + t2;
								current_y = y_left + t2*p;
								iface = current_edge->f[FACE_DOWN];
							}
						}


						// The initial point lies in the /| face
						else { // if (y_left < m*x_left+(y1-m*x1)) 
							t1 = x1+hx-x_left;
							current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];
							current_x = x_left + t1;
							current_y = y_left + t1*p;
							iface = current_edge->f[FACE_LEFT];
						}


						// We now determine the final edge to process
						if (x2_eq_x_right && y2_eq_y_right) {
							/*
							final_edge = v2->e[EDGE_N];

							// Mark the NE edge of v2
							v2->e[EDGE_NE]->inter_sign++;
							if (v2->e[EDGE_NE]->inter_x == NOT_SET ||
							   v2->e[EDGE_NE]->inter_sign > 0) {

								v2->e[EDGE_NE]->inter_x = x_right;
								v2->e[EDGE_NE]->inter_y = y_right;
							}

							fface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[i], y[i]);
							x[i] -= 2 * CMP_EPSILON;
							y[i] += 2 * CMP_EPSILON;
							i--;
							continue;
						}


						// The final point lies in the | edge of a /| face
						else if (x2_eq_x_right) {
							final_edge = v2->e[EDGE_S];
							fface = NULL;
						}

						// The final point lies in the _ edge of a |/ face
						else if (y2_eq_y_right) {
							final_edge = v2->e[EDGE_W];
							fface = NULL;
						}

						// The final point  lies in the / edge
						else if (double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) {
							final_edge = v2->e[EDGE_SW];
							fface = NULL;
						}


						// The final point lies in the |/ face but not in the / edge
						else if (y_right > m*x_right+(y2-m*x2)) {
							t1 = x2-hx-x_right;
							final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
							fface = final_edge->f[FACE_RIGHT];
						}


						// The final point lies in the /| face  but not in the / edge
						// nor the | edge
						else { // if (y_right < m*x_right+(y2-m*x2)) 
							t1 = -(y2-hy-y_right)/p;
							t2 = -((x2-x_right)-(y2-y_right)/m)/(1-p/m);

							if (double_eq(t1, t2, CMP_EPSILON)) {
								final_edge = v2->e[EDGE_SW];
								fface = final_edge->f[FACE_RIGHT];
							}

							else if (t1 < t2) { 
								final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
								fface = final_edge->f[FACE_UP];
							}

							else  {
								final_edge = v2->e[EDGE_SW];
								fface = final_edge->f[FACE_RIGHT];
							}
						}

						if (iface != NULL && fface != NULL && iface == fface) continue;

						next_edge = current_edge;
						//print_edge(final_edge, "fe");


						while(1) {
							current_edge = next_edge;
							//print_edge(current_edge, "ce");

							if (current_edge->type == EDGE_H) {
								current_edge->inter_sign--;
								to_queue = current_edge->v[1];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign < 0) {

									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[0]->is_queued &&
											!is_still_valid(current_edge->v[0])) {
										queue_remove(q, current_edge->v[0]);
										current_edge->v[0]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign > 0) do nada

								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->x - current_x);

								current_x = current_edge->v[1]->x;
								current_y += t1*p;

								next_edge = current_edge->v[1]->e[EDGE_N];
							}

							else if (current_edge->type == EDGE_D) {
								current_edge->inter_sign++;
								to_queue = current_edge->v[0];


								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign > 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[1]->is_queued &&
											!is_still_valid(current_edge->v[1])) {
										queue_remove(q, current_edge->v[1]);
										current_edge->v[1]->is_queued = 0;
									}

								}

								// else if (current_edge->inter_sign < 0) do nada


								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->x - current_x);
								current_x = current_edge->v[1]->x;
								current_y += t1 *p;
								next_edge = current_edge->v[1]->e[EDGE_S];
							}

							else { // current_edge->type == EDGE_V
								current_edge->inter_sign++;
								to_queue = current_edge->v[0];


								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign > 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[1]->is_queued &&
											!is_still_valid(current_edge->v[1])) {
										queue_remove(q, current_edge->v[1]);
										current_edge->v[1]->is_queued = 0;
									}

								}

								// else if (current_edge->inter_sign < 0) do nada

								if (current_edge == final_edge) break;

								t1 = current_edge->v[0]->x - current_x;
								t1 -= (current_edge->v[0]->y - current_y)/m;
								t1 /= 1-p/m;
								t2 = (current_edge->v[1]->y - current_y)/p;

								if (double_eq(t1, 0.0, CMP_EPSILON)) 
									next_edge = current_edge->v[0]->e[EDGE_NE];

								else if (double_eq(t2, 0.0, CMP_EPSILON)) {
									next_edge = current_edge->v[0]->e[EDGE_NE];
								}

								else if (double_eq(t1,t2, CMP_EPSILON)) {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[1]->e[EDGE_E];
								}

								else if (t1 < t2) {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[0]->e[EDGE_NE];
								}

								else if (t1 > t2)  {
									current_x += t2;
									current_y += t2*p;
									next_edge = current_edge->v[1]->e[EDGE_E];
								}

								else  {
									current_x += t1;
									current_y += t1*p;

									next_edge = current_edge->v[1]->e[EDGE_NE];
								}
							}
					
							//Extra check to prevent segfault
							//Further programming may be needed
							if (
								   next_edge->v[0]->x > x2 + hx
								|| next_edge->v[0]->y > y2 + hy
							) {
								iprint_phase_1_segfault(p, swapped,
										x_left, y_left, 
										x_right, y_right,
										x1, y1,
										x2, y2,
										hx, hy,
										nx, ny
								);
								break;
							}
						}
					} 
				}


				else { // if (p < 0) 
					// (x1, y1) and (x2, y2) are setted in such way that the segment they define is
					// the smallest rectangle in the grid that contains the segment 
					// (x_left, y_left)-(x_right, y_right).
					// i1, j1, i2, j2 are they respective indexes in the array containing the vertices 


					v1 = get_init_vertex(grid, x_left, y_left, -1);
					v2 = get_end_vertex(grid, x_right, y_right, -1);

					x1 = v1->x;
					y1 = v1->y;

					x2 = v2->x;
					y2 = v2->y;

					x1_eq_x_left = double_eq(x1, x_left, CMP_EPSILON);
					y1_eq_y_left = double_eq(y1, y_left, CMP_EPSILON);
					x2_eq_x_right = double_eq(x2, x_right, CMP_EPSILON);
					y2_eq_y_right = double_eq(y2, y_right, CMP_EPSILON);

					//iprintf("x_left = %f, x1 = %f , x_left == x1 ? %d", x_left, x1, x1_eq_x_left);
					//iprintf("y_left = %f, y1 = %f , y_left == y1 ? %d", y_left, y1, y1_eq_y_left);
					//iprintf("x_right = %f, x2 = %f , x_right == x2 ? %d", x_right, x2, x2_eq_x_right);
					//iprintf("y_right = %f, y2 = %f , y_right == y2 ? %d", y_right, y2, y2_eq_y_right);

					// The initial point is the grid point (x1, y1)
					if (x1_eq_x_left && y1_eq_y_left) {
						/*
						current_edge = v1->e[EDGE_N];
						current_x = x1;
						current_y = y1;
						iface = NULL;
						*/

						iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
						x[(i+1)%npts] -= 2 * CMP_EPSILON;
						y[(i+1)%npts] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// The initial point lies in the | edge of a |/ face.
					else if (x1_eq_x_left) {
						current_edge = v1->e[EDGE_S];
						current_x = x1;
						current_y = y_left;
						iface = NULL;
					}

					// The initial point lies in the _ edge of a |/ face.
					else if (y1_eq_y_left) {
						current_edge = v1->e[EDGE_E];
						current_x = x_left;
						current_y = y1;
						iface = NULL;
					}

					// The initial point lies in the / edge
					else if (double_eq(y_left, m*x_left+(y1-hy-m*x1), CMP_EPSILON)) {
						current_edge = v1->e[EDGE_S]->v[0]->e[EDGE_NE];
						current_x = x_left;
						current_y = y_left;

						iface = NULL;
					}

					// The initial point lies in the |/ face 
					// but not in the | edge nor the / edge of its.
					else if (y_left > m*x_left+(y1-hy-m*x1)) {
						current_edge = v1->e[EDGE_S]->v[0]->e[EDGE_NE]; 

						t1 = current_edge->v[0]->x - x_left;
						t1 -= (current_edge->v[0]->y - y_left)/m;
						t1 /= 1-p/m;

						current_x = x_left + t1;
						current_y = y_left + t1*p;

						iface = current_edge->f[FACE_UP];
					}

					// The initial point lies in the /| face
					else { // if (y_left < m*x_left+(y1-m*x1)) 
						t1 = (y1-hy-y_left)/p;
						t2 = x1+hx-x_left;

						if (double_eq(t1, t2, CMP_EPSILON)) {
							current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_S];
							current_x = current_edge->v[0]->x;
							current_y = y_left + t2*p;
							iface = current_edge->f[FACE_LEFT];
						}

						else if (t1 < t2) {
							current_edge = v1->e[EDGE_S]->v[0]->e[EDGE_E];
							current_x = x_left + t1;;
							current_y = current_edge->v[0]->y;
							iface = current_edge->f[FACE_UP];
						}

						else {//  if (t1 > t2)
							current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_S];
							current_x = current_edge->v[0]->x;
							current_y = y_left + t2*p;
							iface = current_edge->f[FACE_LEFT];
						}
					}

					// We now determine the final edge to process
					if (x2_eq_x_right && y2_eq_y_right){ 
						/*
						final_edge = v2->e[EDGE_E];
						fface = NULL;
						*/

						iprintf("Corrected (%f, %f)", x[i], y[i]);
						x[i] -= 2 * CMP_EPSILON;
						y[i] += 2 * CMP_EPSILON;
						i--;
						continue;
					}


					// The final point lies in the | edge of a /| face
					else if (x2_eq_x_right) {
						final_edge = v2->e[EDGE_N];
						fface = NULL;
					}

					// The final point lies in the _ edge of a /| face
					else if (y2_eq_y_right) {
						final_edge = v2->e[EDGE_W];
						fface = NULL;
					}

					// The final point  lies in the / edge
					else if (double_eq(y_right,m*x_right+(y2-m*(x2-hx)), CMP_EPSILON)) {
						final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_NE];
						fface = NULL;
					}

					// The final point lies in the |/ face but not in the / edge
					else if (y_right > m*x_right+(y2-m*(x2-hx))) {
						t1 = (y2+hy-y_right)/p;
						t2 = x2-hx-x_right;

						if (double_eq(t1, t2, CMP_EPSILON)) {
							final_edge = v2->e[EDGE_N]->v[1]->e[EDGE_W];
							fface = final_edge->f[FACE_DOWN];
						}

						else if (fabs(t1) < fabs(t2)) {
							final_edge = v2->e[EDGE_N]->v[1]->e[EDGE_W];
							fface = final_edge->f[FACE_DOWN];
						}

						else  {// if (t1 > t2)
							final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_N];
							fface = final_edge->f[FACE_RIGHT];
						}
					}


					// The final point lies in the /| face  but not in the / edge
					// nor the | edge
					else { // if (y_right < m*x_right+(y2-m*x2)) 
						final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_NE];
						fface = final_edge->f[FACE_DOWN];
					}

					if (iface != NULL && fface != NULL && iface == fface) continue;

					next_edge = current_edge;

					//print_edge(final_edge, "fe");

					while(1) {
						current_edge = next_edge;
						//print_edge(current_edge, "ce");

						if (current_edge->type == EDGE_V) {
							current_edge->inter_sign++;
							to_queue = current_edge->v[0];

							if (current_edge->inter_x == NOT_SET ||
							   current_edge->inter_sign > 0) {
								current_edge->inter_x = current_x;
								current_edge->inter_y = current_y;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}
							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;
								current_edge->inter_sign = 0;

								if (current_edge->v[1]->is_queued &&
										!is_still_valid(current_edge->v[1])) {
									queue_remove(q, current_edge->v[1]);
									current_edge->v[1]->is_queued = 0;
								}
							}

							// else if (current_edge->inter_sign < 0) do nada


							if (current_edge == final_edge) break;

							t1 = current_edge->v[0]->x - current_x;
							t1 -= (current_edge->v[0]->y - current_y)/m;
							t1 /= 1-p/m;

							current_x += t1;
							current_y += t1*p;

							next_edge = current_edge->v[0]->e[EDGE_NE];
						}

						else if (current_edge->type == EDGE_D) {
							current_edge->inter_sign++;
							to_queue = current_edge->v[0];

							if (current_edge->inter_x == NOT_SET ||
							   current_edge->inter_sign > 0) {
								current_edge->inter_x = current_x;
								current_edge->inter_y = current_y;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}
							}

							else if (current_edge->inter_sign  == 0) { 

								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[1]->is_queued &&
										!is_still_valid(current_edge->v[1])) {
									queue_remove(q, current_edge->v[1]);
									current_edge->v[1]->is_queued = 0;
								}
							}

							// else if (current_edge->inter_sign < 0) do nada


							if (current_edge == final_edge) break;

							t1 = (current_edge->v[0]->y-current_y)/p;
							t2 = current_edge->v[1]->x-current_x;


							if (double_eq(t1, t2, CMP_EPSILON)) {
								current_x += t1;
								current_y += t1*p;
								next_edge = current_edge->v[1]->e[EDGE_S];
							}

							else if (double_eq(t1, 0.0, CMP_EPSILON)) {
								next_edge = current_edge->v[0]->e[EDGE_E];
							}

							else if (double_eq(t2, 0.0, CMP_EPSILON)) {
								next_edge = current_edge->v[1]->e[EDGE_S];
							}

							else if (t1 < t2) {
								current_x += t1;
								current_y = current_edge->v[0]->y;

								next_edge = current_edge->v[0]->e[EDGE_E];
							}

							else { // if (t1 > t2) 
								current_x = current_edge->v[1]->x;
								current_y += t2*p;

								next_edge = current_edge->v[1]->e[EDGE_S];
							}
						}


						else { // current_edge->type == EDGE_H
							current_edge->inter_sign++;
							to_queue = current_edge->v[0];

							if (current_edge->inter_x == NOT_SET ||
							   current_edge->inter_sign > 0) {

								current_edge->inter_x = current_x;
								current_edge->inter_y = current_y;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}
							}

							else if (current_edge->inter_sign == 0) {

								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[1]->is_queued &&
								   !is_still_valid(current_edge->v[1])) {
									queue_remove(q, current_edge->v[1]);
									current_edge->v[1]->is_queued = 0;
								}
							}

							// else if (current_edge->inter_sign < 0) do nada

							if (current_edge == final_edge) break;

							t1 = current_edge->v[0]->x - current_x;
							t1 -= (current_edge->v[0]->e[EDGE_S]->v[0]->y - current_y)/m;
							t1 /= 1-p/m;

							current_x += t1;
							current_y += t1*p;

							next_edge = current_edge->v[1]->e[EDGE_SW];
						}
				
						//Extra check to prevent segfault
						//Further programming may be needed
						if (
							   next_edge->v[0]->x > x2 + hx
							|| next_edge->v[0]->y < y2 - hy
						) {
							iprint_phase_1_segfault(p, swapped,
									x_left, y_left, 
									x_right, y_right,
									x1, y1,
									x2, y2,
									hx, hy,
									nx, ny
							);
							break;
						}
					}
				}
			}

			else { // if (not swapped)
				// We separate the cases when the segment has
				// slope and when it has negative slope. Notice that
				// the case whe it has zero slope has already been considered
				// in the case when num = 0;
				if (p > 0) {
					// (x1, y1) and (x2, y2) are setted in such way that the segment they define is
					// the smallest rectangle in the grid that contains the segment 
					// (x_left, y_left)-(x_right, y_right).
					// i1, j1, i2, j2 are they respective indexes in the array containing the vertices 

					v1 = get_init_vertex(grid, x_left, y_left, 1);
					v2 = get_end_vertex(grid, x_right, y_right, 1);

					x1 = v1->x;
					y1 = v1->y;

					x2 = v2->x;
					y2 = v2->y;

					x1_eq_x_left = double_eq(x1, x_left, CMP_EPSILON);
					y1_eq_y_left = double_eq(y1, y_left, CMP_EPSILON);
					x2_eq_x_right = double_eq(x2, x_right, CMP_EPSILON);
					y2_eq_y_right = double_eq(y2, y_right, CMP_EPSILON);

					//iprintf("x_left = %f, x1 = %f , x_left == x1 ? %d", x_left, x1, x1_eq_x_left);
					//iprintf("y_left = %f, y1 = %f , y_left == y1 ? %d", y_left, y1, y1_eq_y_left);
					//iprintf("x_right = %f, x2 = %f , x_right == x2 ? %d", x_right, x2, x2_eq_x_right);
					//iprintf("y_right = %f, y2 = %f , y_right == y2 ? %d", y_right, y2, y2_eq_y_right);

					// Determine the initial edge (stored in current_edge)
					// and the final edge to process (stored in final_edge)

					if (double_eq(p,m, CMP_EPSILON)) {
						// The initial point is the grid point (x1, y1)
						if (x1_eq_x_left && y1_eq_y_left)  {
							/*
							current_edge = v1->e[EDGE_S];

							// Mark the SW edge of v1
							v1->e[EDGE_SW]->inter_sign--;
							if (v1->e[EDGE_SW]->inter_x == NOT_SET ||
							   v1->e[EDGE_SW]->inter_sign < 0) {

								v1->e[EDGE_SW]->inter_x = x_left;
								v1->e[EDGE_SW]->inter_y = y_left;
							}

							else if (v1->e[EDGE_SW]->inter_sign == 0) {
								v1->e[EDGE_SW]->inter_x = NOT_SET;
								v1->e[EDGE_SW]->inter_y = NOT_SET;
							}

							current_x = x1;
							current_y = y1;
							iface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[i], y[i]);
							x[i] -= 2 * CMP_EPSILON;
							y[i] += 2 * CMP_EPSILON;
							i--;
							continue;
						}

						// The initial point lies in the | edge of a |/ face.
						else if (x1_eq_x_left) {
							current_edge = v1->e[EDGE_N];

							current_x = x1;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the _ edge of a /| face.
						else if (y1_eq_y_left) {
							current_edge = v1->e[EDGE_E];
							current_x = x_left;
							current_y = y1;
							iface = NULL;
						}

						// The initial point lies in the / edge
						else if (double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) {
							v1->e[EDGE_NE]->inter_sign--;
							if (v1->e[EDGE_NE]->inter_x == NOT_SET ||
							   v1->e[EDGE_NE]->inter_sign < 0) {

								v1->e[EDGE_NE]->inter_x = x_left;
								v1->e[EDGE_NE]->inter_y = y_left;
							}

							else if (v1->e[EDGE_NE]->inter_sign == 0) {
								v1->e[EDGE_NE]->inter_x  = NOT_SET;
								v1->e[EDGE_NE]->inter_y  = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign < 0) do nada
							
							current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];
							current_x = x_left;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the |/ face 
						// but not in the | edge nor the / edge of its.
						else if (y_left > m*x_left+(y1-m*x1)) {
							t1 = (y1+hy-y_left)/p;
							current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E]; 
							current_x = x_left + t1;
							current_y = v1->e[EDGE_N]->v[1]->y;
							iface = current_edge->f[FACE_DOWN];
						}


						// The initial point lies in the /| face
						else { // if (y_left < m*x_left+(y1-m*x1)) 
							t1 = x1+hx-x_left;

							current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];
							current_x = x_left + t1;
							current_y = y_left + t1*p;
							iface = current_edge->f[FACE_LEFT];

						}

						// We now determine the final edge to process
						if (x2_eq_x_right && y2_eq_y_right) {
							/*
							final_edge = v2->e[EDGE_E];

							// Mark the NE edge of v2
							v2->e[EDGE_NE]->inter_sign++;
							if (v2->e[EDGE_NE]->inter_x == NOT_SET ||
							   v2->e[EDGE_NE]->inter_sign > 0) {

								v2->e[EDGE_NE]->inter_x = x_right;
								v2->e[EDGE_NE]->inter_y = y_right;

							}

							else if (v2->e[EDGE_NE]->inter_sign == 0) {
								v2->e[EDGE_NE]->inter_x = NOT_SET;
								v2->e[EDGE_NE]->inter_y = NOT_SET;
							}

							// else if (v2->e[EDGE_NE]->inter_sign--) do nada

							fface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
							x[(i+1)%npts] -= 2 * CMP_EPSILON;
							y[(i+1)%npts] += 2 * CMP_EPSILON;
							i--;
							continue;
						}

						// The final point lies in the | edge of a /| face
						else if (x2_eq_x_right) {
							final_edge = v2->e[EDGE_S];
							fface = NULL;
						}

						// The final point lies in the _ edge of a |/ face
						else if (y2_eq_y_right) {
							final_edge = v2->e[EDGE_W];
							fface = NULL;
						}

						// The final point  lies in the / edge
						else if (double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) {
							v2->e[EDGE_SW]->inter_sign++;
							if (v2->e[EDGE_SW]->inter_x == NOT_SET ||
							   v2->e[EDGE_SW]->inter_sign > 0) {

								v2->e[EDGE_SW]->inter_x = x_left;
								v2->e[EDGE_SW]->inter_y = y_left;
							}

							else if (v2->e[EDGE_SW]->inter_sign == 0) {
								v2->e[EDGE_SW]->inter_x  = NOT_SET;
								v2->e[EDGE_SW]->inter_y  = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign < 0) do nada
							final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
							fface = NULL;
						}

						// The final point lies in the |/ face but not in the / edge
						else if (y_right > m*x_right+(y2-m*x2)) {
							final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
							fface = final_edge->f[FACE_RIGHT];
						}


						// The final point lies in the /| face  but not in the / edge
						// nor the | edge
						else { // if (y_right < m*x_right+(y2-m*x2)) 
							final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
							fface = final_edge->f[FACE_UP];
						}


						// This is to test if the points lay in the
						// same triangle
						if (iface != NULL && fface != NULL && iface == fface) continue;

						next_edge = current_edge;

						//print_edge(final_edge, "fe");
						while(1) {
							current_edge = next_edge;
							//print_edge(current_edge, "ce");

							if (current_edge->type == EDGE_V) {
								current_edge->inter_sign--;
								to_queue = current_edge->v[1];


								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign < 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {

									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[0]->is_queued &&
											!is_still_valid(current_edge->v[0])) {
										queue_remove(q, current_edge->v[0]);
										current_edge->v[0]->is_queued = 0;
									}
								}


								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->y - current_y)/p;

								current_x += t1;
								current_y = current_edge->v[1]->y;

								next_edge = current_edge->v[1]->e[EDGE_E];
							}

							else if (current_edge->type == EDGE_D) {
								next_edge = current_edge->v[1]->e[EDGE_S];
							}

							else { // current_edge->type == EDGE_H

								current_edge->inter_sign++;
								to_queue = current_edge->v[0];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign > 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[1]->is_queued &&
											!is_still_valid(current_edge->v[1])) {
										queue_remove(q, current_edge->v[1]);
										current_edge->v[1]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign < 0) do nada


								if (current_edge == final_edge) break;

								t1 = current_edge->v[1]->x - current_x;

								current_x += t1;
								current_y += t1*p;

								next_edge = current_edge->v[1]->e[EDGE_N];

							}

							//Extra check to prevent segfault
							//Further programming may be needed
							if (
								   next_edge->v[0]->x > x2 + hx
								|| next_edge->v[0]->y > y2 + hy
							) {
								iprint_phase_1_segfault(p, swapped,
										x_left, y_left, 
										x_right, y_right,
										x1, y1,
										x2, y2,
										hx, hy,
										nx, ny
								);
								break;
							}
						}
					}

					else if (p>m) {
						// The initial point is the grid point (x1, y1)
						if (x1_eq_x_left && y1_eq_y_left)  {
							/*
							current_edge = v1->e[EDGE_S];

							// Mark the SW edge of v1
							v1->e[EDGE_SW]->inter_sign--;
							if (v1->e[EDGE_SW]->inter_x == NOT_SET ||
							   v1->e[EDGE_SW]->inter_sign < 0) {

								v1->e[EDGE_SW]->inter_x = x_left;
								v1->e[EDGE_SW]->inter_y = y_left;

							}

							else if (v1->e[EDGE_SW]->inter_sign == 0) {
								v1->e[EDGE_SW]->inter_x = NOT_SET;
								v1->e[EDGE_SW]->inter_y = NOT_SET;
							}

							current_x = x1;
							current_y = y1;
							iface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[i], y[i]);
							x[i] -= 2 * CMP_EPSILON;
							y[i] += 2 * CMP_EPSILON;
							i--;
							continue;
						}

						// The initial point lies in the | edge of a |/ face.
						else if (x1_eq_x_left) {
							current_edge = v1->e[EDGE_N];
							current_x = x1;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the _ edge of a /| face.
						else if (y1_eq_y_left) {
							current_edge = v1->e[EDGE_E];
							current_x = x_left;
							current_y = y1;
							iface = NULL;
						}


						// The initial point lies in the / edge
						else if (double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) {
							current_edge = v1->e[EDGE_NE];
							current_x = x_left;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the |/ face 
						// but not in the | edge nor the / edge of its.
						else if (y_left > m*x_left+(y1-m*x1)) {
							t1 = (y1+hy-y_left)/p;
							current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E]; 
							current_x = x_left + t1;
							current_y = v1->e[EDGE_N]->v[1]->y;
							iface = current_edge->f[FACE_DOWN];
						}


						// The initial point lies in the /| face
						else { // if (y_left < m*x_left+(y1-m*x1)) 
							t1 = x1+hx-x_left;
							t2 = ((x1-x_left)-(y1-y_left)/m)/(1-p/m);

							//iprintf("t1 = %2.30f, t2 = %2.30f", t1, t2);

							if (double_eq(t1, t2, CMP_EPSILON)) {
								current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];
								current_x = x_left + t2;
								current_y = y_left + t2*p;
								iface = current_edge->f[FACE_DOWN];
							}

							else if (t1 < t2) {
								current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];
								current_x = x_left + t1;
								current_y = y_left + t1*p;
								iface = current_edge->f[FACE_LEFT];
							}

							else {//  if (t1 > t2)
								current_edge = v1->e[EDGE_NE];
								current_x = x_left + t2;
								current_y = y_left + t2*p;
								iface = current_edge->f[FACE_DOWN];
							}
						}

						// We now determine the final edge to process
						if (x2_eq_x_right && y2_eq_y_right) { 
							/*
							final_edge = v2->e[EDGE_E];

							// Mark the NE edge of v2
							v2->e[EDGE_NE]->inter_sign++;
							if (v2->e[EDGE_NE]->inter_x == NOT_SET ||
							   v2->e[EDGE_NE]->inter_sign > 0) {

								v2->e[EDGE_NE]->inter_x = x_right;
								v2->e[EDGE_NE]->inter_y = y_right;
							}

							else if (v2->e[EDGE_NE]->inter_sign == 0) {
								v2->e[EDGE_NE]->inter_x = NOT_SET;
								v2->e[EDGE_NE]->inter_y = NOT_SET;
							}

							// else if (v2->e[EDGE_NE]->inter_sign < 0) do nada

							fface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
							x[(i+1)%npts] -= 2 * CMP_EPSILON;
							y[(i+1)%npts] += 2 * CMP_EPSILON;
							i--;
							continue;
						}

						// The final point lies in the | edge of a /| face
						else if (x2_eq_x_right)  {
							final_edge = v2->e[EDGE_S];
							fface = NULL;
						}

						// The final point lies in the _ edge of a |/ face
						else if (y2_eq_y_right) {
							final_edge = v2->e[EDGE_W];
							fface = NULL;
						}

						// The final point  lies in the / edge
						else if (double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) {
							final_edge = v2->e[EDGE_SW];
							fface = NULL;
						}

						// The final point lies in the |/ face but not in the / edge
						// or the _ edge
						else if (y_right > m*x_right+(y2-m*x2)) {
							t1 = ((x2-x_right)-(y2-y_right)/m)/(1-p/m);
							t2 = x2-hx-x_right;

							if (double_eq(t1,t2, CMP_EPSILON)){
								final_edge = v2->e[EDGE_SW];
								fface = final_edge->f[FACE_UP]; 
							}

							else if (fabs(t1) < fabs(t2)) {
								final_edge = v2->e[EDGE_SW];
								fface = final_edge->f[FACE_UP]; 
							}

							else  { // if (t1 > t2)
								final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
								fface = final_edge->f[FACE_RIGHT];
							}
						}


						// The final point lies in the /| face  but not in the / edge
						// nor the | edge
						else  { // if (y_right < m*x_right+(y2-m*x2)) 
							final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
							fface = final_edge->f[FACE_UP];
						}

						if (iface != NULL && fface != NULL && iface == fface) continue;

						next_edge = current_edge;

						//print_edge(final_edge, "fe");
						while(1) {
							current_edge = next_edge;
							//print_edge(current_edge, "ce");

							if (current_edge->type == EDGE_V) {

								current_edge->inter_sign--;
								to_queue = current_edge->v[1];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign < 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[0]->is_queued &&
											!is_still_valid(current_edge->v[0])) {
										queue_remove(q, current_edge->v[0]);
										current_edge->v[0]->is_queued = 0;
									}

								}

								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->y - current_y)/p;

								current_x += t1;
								current_y = current_edge->v[1]->y;

								next_edge = current_edge->v[1]->e[EDGE_E];
							}

							else if (current_edge->type == EDGE_D) {
								current_edge->inter_sign++;
								to_queue = current_edge->v[0];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign > 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[1]->is_queued &&
											!is_still_valid(current_edge->v[1])) {
										queue_remove(q, current_edge->v[1]);
										current_edge->v[1]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign < 0) do nada;


								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->y - current_y)/p;

								current_x += t1;
								current_y = current_edge->v[1]->y;

								next_edge = current_edge->v[1]->e[EDGE_W];
							}

							else { // current_edge->type == EDGE_H
								current_edge->inter_sign++;
								to_queue = current_edge->v[0];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign > 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[1]->is_queued &&
											!is_still_valid(current_edge->v[1])) {
										queue_remove(q, current_edge->v[1]);
										current_edge->v[1]->is_queued = 0;
									}

								}

								// else if (current_edge->inter_sign < 0) do nada


								if (current_edge == final_edge) break;

								t1 = current_edge->v[1]->x - current_x;
								t2 = current_edge->v[0]->x - current_x;
								t2 -= (current_edge->v[0]->y - current_y)/m;
								t2 /= 1-p/m;

								if (double_eq(t1, 0.0, CMP_EPSILON)) {
									next_edge = current_edge->v[1]->e[EDGE_N];
								}

								else if (double_eq(t1,t2, CMP_EPSILON)) {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[1]->e[EDGE_N];

								}
								else if (t1 < t2) {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[1]->e[EDGE_N];
								}

								else if (t1 > t2) {
									current_x += t2;
									current_y += t2*p;
									next_edge = current_edge->v[0]->e[EDGE_NE];
								}

							}
						
							//Extra check to prevent segfault
							//Further programming may be needed
							if (
								   next_edge->v[0]->x > x2 + hx
								|| next_edge->v[0]->y > y2 + hy
							) {
								iprint_phase_1_segfault(p, swapped,
										x_left, y_left, 
										x_right, y_right,
										x1, y1,
										x2, y2,
										hx, hy,
										nx, ny
								);
								break;
							}
						}
					}

					else { // if (0<p<m)
						if (x1_eq_x_left && y1_eq_y_left)  {
							/*
							current_edge = v1->e[EDGE_S];

							// Mark the SW edge of v1
							v1->e[EDGE_SW]->inter_sign--;
							if (v1->e[EDGE_SW]->inter_x == NOT_SET ||
							   v1->e[EDGE_SW]->inter_sign < 0) {

								v1->e[EDGE_SW]->inter_x = x_left;
								v1->e[EDGE_SW]->inter_y = y_left;
							}

							else if (v1->e[EDGE_SW]->inter_sign == 0) {

								v1->e[EDGE_SW]->inter_x = NOT_SET;
								v1->e[EDGE_SW]->inter_y = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign > 0) do nada

							current_x = x1;
							current_y = y1;
							iface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[i], y[i]);
							x[i] -= 2 * CMP_EPSILON;
							y[i] += 2 * CMP_EPSILON;
							i--;
							continue;
						}

						// The initial point lies in the | edge of a |/ face.
						else if (x1_eq_x_left) {
							current_edge = v1->e[EDGE_N];
							current_x = x1;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the _ edge of a /| face.
						else if (y1_eq_y_left) {
							current_edge = v1->e[EDGE_E];
							current_x = x_left;
							current_y = y1;
							iface = NULL;
						}

						// The initial point lies in the / edge
						else if (double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) {
							current_edge = v1->e[EDGE_NE];
							current_x = x_left;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the |/ face 
						// but not in the | edge nor the / edge of its.
						else if (y_left > m*x_left+(y1-m*x1)) {
							t1 = ((x1-x_left)-(y1-y_left)/m)/(1-p/m);
							t2 = (y1+hy-y_left)/p;

							if (double_eq(t1, t2, CMP_EPSILON)) {
								current_edge = v1->e[EDGE_NE]; 
								current_x = x_left + t1;
								current_y = y_left + t1*p;
								iface = current_edge->f[FACE_LEFT];
							}

							else if (t1 < t2) {
								current_edge = v1->e[EDGE_NE]; 
								current_x = x_left + t1;
								current_y = y_left + t1*p;
								iface = current_edge->f[FACE_LEFT];
							}

							else { // if (t1 > t2) 
								current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E];
								current_x = x_left + t2;
								current_y = y_left + t2*p;
								iface = current_edge->f[FACE_DOWN];
							}
						}


						// The initial point lies in the /| face
						else { // if (y_left < m*x_left+(y1-m*x1)) 
							t1 = x1+hx-x_left;
							current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];
							current_x = x_left + t1;
							current_y = y_left + t1*p;
							iface = current_edge->f[FACE_LEFT];
						}


						// We now determine the final edge to process
						if (x2_eq_x_right && y2_eq_y_right) {
							/*
							final_edge = v2->e[EDGE_E];

							// Mark the NE edge of v2
							v2->e[EDGE_NE]->inter_sign++;
							if (v2->e[EDGE_NE]->inter_x == NOT_SET ||
							   v2->e[EDGE_NE]->inter_sign > 0) {

								v2->e[EDGE_NE]->inter_x = x_right;
								v2->e[EDGE_NE]->inter_y = y_right;

							}

							else if (v2->e[EDGE_NE]->inter_sign == 0) {
								v2->e[EDGE_NE]->inter_x = NOT_SET;
								v2->e[EDGE_NE]->inter_y = NOT_SET;
							}

							// else if (v2->e[EDGE_NE]->inter_sign < 0) do nada

							fface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
							x[(i+1)%npts] -= 2 * CMP_EPSILON;
							y[(i+1)%npts] += 2 * CMP_EPSILON;
							i--;
							continue;
						}


						// The final point lies in the | edge of a /| face
						else if (x2_eq_x_right) {
							final_edge = v2->e[EDGE_S];
							fface = NULL;
						}

						// The final point lies in the _ edge of a |/ face
						else if (y2_eq_y_right) {
							final_edge = v2->e[EDGE_W];
							fface = NULL;
						}

						// The final point  lies in the / edge
						else if (double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) {
							final_edge = v2->e[EDGE_SW];
							fface = NULL;
						}


						// The final point lies in the |/ face but not in the / edge
						else if (y_right > m*x_right+(y2-m*x2)) {
							t1 = x2-hx-x_right;
							final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
							fface = final_edge->f[FACE_RIGHT];
						}


						// The final point lies in the /| face  but not in the / edge
						// nor the | edge
						else { // if (y_right < m*x_right+(y2-m*x2)) 
							t1 = -(y2-hy-y_right)/p;
							t2 = -((x2-x_right)-(y2-y_right)/m)/(1-p/m);

							if (double_eq(t1, t2, CMP_EPSILON)) {
								final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
								fface = final_edge->f[FACE_UP];
							}

							else if (t1 < t2) { 
								final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
								fface = final_edge->f[FACE_UP];
							}

							else  {
								final_edge = v2->e[EDGE_SW];
								fface = final_edge->f[FACE_RIGHT];
							}
						}

						if (iface != NULL && fface != NULL && iface == fface) continue;

						next_edge = current_edge;

						//print_edge(final_edge, "fe");
						while(1) {
							current_edge = next_edge;
							//print_edge(current_edge, "ce");

							if (current_edge->type == EDGE_H) {
								current_edge->inter_sign++;
								to_queue = current_edge->v[0];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign > 0) {

									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[1]->is_queued &&
									   !is_still_valid(current_edge->v[1])) {
										queue_remove(q, current_edge->v[1]);
										current_edge->v[1]->is_queued = 0;
									}
								}

								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->x - current_x);

								current_x = current_edge->v[1]->x;
								current_y += t1*p;

								next_edge = current_edge->v[1]->e[EDGE_N];
							}

							else if (current_edge->type == EDGE_D) {
								current_edge->inter_sign--;
								to_queue = current_edge->v[1];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign < 0) {

									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[0]->is_queued &&
											!is_still_valid(current_edge->v[0])) {
										queue_remove(q, current_edge->v[0]);
										current_edge->v[0]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign > 0)  do nada


								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->x - current_x);
								current_x = current_edge->v[1]->x;
								current_y += t1 *p;
								next_edge = current_edge->v[1]->e[EDGE_S];
							}

							else { // current_edge->type == EDGE_V
								current_edge->inter_sign--;
								to_queue = current_edge->v[1];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign < 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}


								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[0]->is_queued &&
											!is_still_valid(current_edge->v[0])) {
										queue_remove(q, current_edge->v[0]);
										current_edge->v[0]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign > 0) do nada


								if (current_edge == final_edge) break;

								t1 = current_edge->v[0]->x - current_x;
								t1 -= (current_edge->v[0]->y - current_y)/m;
								t1 /= 1-p/m;
								t2 = (current_edge->v[1]->y - current_y)/p;

								if (double_eq(t1, 0.0, CMP_EPSILON)) 
									next_edge = current_edge->v[0]->e[EDGE_NE];

								else if (double_eq(t2, 0.0, CMP_EPSILON)) {
									next_edge = current_edge->v[1]->e[EDGE_E];
								}

								else if (double_eq(t1,t2, CMP_EPSILON)) {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[0]->e[EDGE_NE];

								}

								else if (t1 < t2) {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[0]->e[EDGE_NE];
								}

								else if (t1 > t2)  {
									current_x += t2;
									current_y += t2*p;
									next_edge = current_edge->v[1]->e[EDGE_E];
								}

								else  {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[1]->e[EDGE_E];
								}
							}
							//Extra check to prevent segfault
							//Further programming may be needed
							if (
								   next_edge->v[0]->x > x2 + hx
								|| next_edge->v[0]->y > y2 + hy
							) {
								iprint_phase_1_segfault(p, swapped,
										x_left, y_left, 
										x_right, y_right,
										x1, y1,
										x2, y2,
										hx, hy,
										nx, ny
								);
								break;
							}
						}
					} 
				}


				else { // if (p < 0) 
					// (x1, y1) and (x2, y2) are setted in such way that the segment they define is
					// the smallest rectangle in the grid that contains the segment 
					// (x_left, y_left)-(x_right, y_right).
					// i1, j1, i2, j2 are they respective indexes in the array containing the vertices 


					v1 = get_init_vertex(grid, x_left, y_left, -1);
					v2 = get_end_vertex(grid, x_right, y_right, -1);

					x1 = v1->x;
					y1 = v1->y;

					x2 = v2->x;
					y2 = v2->y;

					x1_eq_x_left = double_eq(x1, x_left, CMP_EPSILON);
					y1_eq_y_left = double_eq(y1, y_left, CMP_EPSILON);
					x2_eq_x_right = double_eq(x2, x_right, CMP_EPSILON);
					y2_eq_y_right = double_eq(y2, y_right, CMP_EPSILON);

					//iprintf("x_left = %f, x1 = %f , x_left == x1 ? %d", x_left, x1, x1_eq_x_left);
					//iprintf("y_left = %f, y1 = %f , y_left == y1 ? %d", y_left, y1, y1_eq_y_left);
					//iprintf("x_right = %f, x2 = %f , x_right == x2 ? %d", x_right, x2, x2_eq_x_right);
					//iprintf("y_right = %f, y2 = %f , y_right == y2 ? %d", y_right, y2, y2_eq_y_right);

					// The initial point is the grid point (x1, y1)
					if (x1_eq_x_left && y1_eq_y_left) {
						/*
						current_edge = v1->e[EDGE_W];

						current_x = x1;
						current_y = y1;
						iface = NULL;
						*/

						iprintf("Corrected (%f, %f)", x[i], y[i]);
						x[i] -= 2 * CMP_EPSILON;
						y[i] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// The initial point lies in the | edge of a |/ face.
					else if (x1_eq_x_left) {
						current_edge = v1->e[EDGE_S];
						current_x = x1;
						current_y = y_left;
						iface = NULL;
					}

					// The initial point lies in the _ edge of a |/ face.
					else if (y1_eq_y_left) {
						current_edge = v1->e[EDGE_E];
						current_x = x_left;
						current_y = y1;
						iface = NULL;
					}

					// The initial point lies in the / edge
					else if (double_eq(y_left, m*x_left+(y1-hy-m*x1), CMP_EPSILON)) {
						current_edge = v1->e[EDGE_S]->v[0]->e[EDGE_NE];
						current_x = x_left;
						current_y = y_left;

						iface = NULL;
					}

					// The initial point lies in the |/ face 
					// but not in the | edge nor the / edge of its.
					else if (y_left > m*x_left+(y1-hy-m*x1)) {
						current_edge = v1->e[EDGE_S]->v[0]->e[EDGE_NE]; 

						t1 = current_edge->v[0]->x - x_left;
						t1 -= (current_edge->v[0]->y - y_left)/m;
						t1 /= 1-p/m;

						current_x = x_left + t1;
						current_y = y_left + t1*p;

						iface = current_edge->f[FACE_UP];
					}

					// The initial point lies in the /| face
					else { // if (y_left < m*x_left+(y1-m*x1)) 
						t1 = (y1-hy-y_left)/p;
						t2 = x1+hx-x_left;

						if (double_eq(t1, t2, CMP_EPSILON)) {
							current_edge = v1->e[EDGE_S]->v[0]->e[EDGE_E];
							current_x = x_left + t1;;
							current_y = current_edge->v[0]->y;
							iface = current_edge->f[FACE_UP];
						}

						else if (t1 < t2) {
							current_edge = v1->e[EDGE_S]->v[0]->e[EDGE_E];
							current_x = x_left + t1;;
							current_y = current_edge->v[0]->y;
							iface = current_edge->f[FACE_UP];
						}

						else {//  if (t1 > t2)
							current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_S];
							current_x = current_edge->v[0]->x;
							current_y = y_left + t2*p;
							iface = current_edge->f[FACE_LEFT];
						}
					}

					// We now determine the final edge to process
					if (x2_eq_x_right && y2_eq_y_right){ 
						/*
						final_edge = v2->e[EDGE_S];
						fface = NULL;
						*/

						iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
						x[(i+1)%npts] -= 2 * CMP_EPSILON;
						y[(i+1)%npts] += 2 * CMP_EPSILON;
						i--;
						continue;
					}


					// The final point lies in the | edge of a /| face
					else if (x2_eq_x_right) {
						final_edge = v2->e[EDGE_N];
						fface = NULL;
					}

					// The final point lies in the _ edge of a /| face
					else if (y2_eq_y_right) {
						final_edge = v2->e[EDGE_W];
						fface = NULL;
					}

					// The final point  lies in the / edge
					else if (double_eq(y_right,m*x_right+(y2-m*(x2-hx)), CMP_EPSILON)) {
						final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_NE];
						fface = NULL;
					}

					// The final point lies in the |/ face but not in the / edge
					else if (y_right > m*x_right+(y2-m*(x2-hx))) {
						t1 = (y2+hy-y_right)/p;
						t2 = x2-hx-x_right;

						if (double_eq(t1, t2, CMP_EPSILON)){
							final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_N];
							fface = final_edge->f[FACE_RIGHT];
						}

						else if (fabs(t1) < fabs(t2)) {
							final_edge = v2->e[EDGE_N]->v[1]->e[EDGE_W];
							fface = final_edge->f[FACE_DOWN];
						}

						else  {// if (t1 > t2)
							final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_N];
							fface = final_edge->f[FACE_RIGHT];
						}
					}


					// The final point lies in the /| face  but not in the / edge
					// nor the | edge
					else { // if (y_right < m*x_right+(y2-m*x2)) 
						final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_NE];
						fface = final_edge->f[FACE_DOWN];
					}

					if (iface != NULL && fface != NULL && iface == fface) continue;

					next_edge = current_edge;

					//print_edge(final_edge, "fe");
					while(1) {
						current_edge = next_edge;
						//print_edge(current_edge, "ce");

						if (current_edge->type == EDGE_V) {
							current_edge->inter_sign--;
							to_queue = current_edge->v[1];


							if (current_edge->inter_x == NOT_SET ||
							   current_edge->inter_sign < 0) {
								current_edge->inter_x = current_x;
								current_edge->inter_y = current_y;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}
							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[0]->is_queued &&
										!is_still_valid(current_edge->v[0])) {
									queue_remove(q, current_edge->v[0]);
									current_edge->v[0]->is_queued = 0;
								}

							}


							if (current_edge == final_edge) break;

							t1 = current_edge->v[0]->x - current_x;
							t1 -= (current_edge->v[0]->y - current_y)/m;
							t1 /= 1-p/m;

							current_x += t1;
							current_y += t1*p;

							next_edge = current_edge->v[0]->e[EDGE_NE];
						}

						else if (current_edge->type == EDGE_D) {
							current_edge->inter_sign--;
							to_queue = current_edge->v[1];

							if (current_edge->inter_x == NOT_SET ||
							   current_edge->inter_sign < 0) {
								current_edge->inter_x = current_x;
								current_edge->inter_y = current_y;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}
							}

							else if (current_edge->inter_sign == 0) { 
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;
								if (current_edge->v[0]->is_queued &&
										!is_still_valid(current_edge->v[0])) {
									queue_remove(q, current_edge->v[0]);
									current_edge->v[0]->is_queued = 0;
								}

							}

							// else if (current_edge->inter_sign > 0) do nada


							if (current_edge == final_edge) break;

							t1 = (current_edge->v[0]->y-current_y)/p;
							t2 = current_edge->v[1]->x-current_x;


							if (double_eq(t1, t2, CMP_EPSILON)) {
								current_x += t1;
								current_y += t1*p;
								next_edge = current_edge->v[0]->e[EDGE_E];
							}

							else if (double_eq(t1, 0.0, CMP_EPSILON)) {
								next_edge = current_edge->v[0]->e[EDGE_E];
							}

							else if (double_eq(t2, 0.0, CMP_EPSILON)) {
								next_edge = current_edge->v[1]->e[EDGE_S];
							}

							else if (t1 < t2) {
								current_x += t1;
								current_y = current_edge->v[0]->y;

								next_edge = current_edge->v[0]->e[EDGE_E];
							}

							else { // if (t1 > t2) 
								current_x = current_edge->v[1]->x;
								current_y += t2*p;

								next_edge = current_edge->v[1]->e[EDGE_S];
							}
						}


						else { // current_edge->type == EDGE_H
							current_edge->inter_sign--;
							to_queue = current_edge->v[1];

							if (current_edge->inter_x == NOT_SET ||
							   current_edge->inter_sign < 0) {
								current_edge->inter_x = current_x;
								current_edge->inter_y = current_y;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}
							}

							else if (current_edge->inter_sign == 0) {

								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[0]->is_queued &&
										!is_still_valid(current_edge->v[0])) {
									queue_remove(q, current_edge->v[0]);
									current_edge->v[0]->is_queued = 0;
								}

							}


							if (current_edge == final_edge) break;

							t1 = current_edge->v[0]->x - current_x;
							t1 -= (current_edge->v[0]->e[EDGE_S]->v[0]->y - current_y)/m;
							t1 /= 1-p/m;

							current_x += t1;
							current_y += t1*p;

							next_edge = current_edge->v[1]->e[EDGE_SW];
						}

						//Extra check to prevent segfault
						//Further programming may be needed
						if (
							   next_edge->v[0]->x > x2 + hx
							|| next_edge->v[0]->y < y2 - hy
						) {
							iprint_phase_1_segfault(p, swapped,
									x_left, y_left, 
									x_right, y_right,
									x1, y1,
									x2, y2,
									hx, hy,
									nx, ny
							);
							break;
						}
					}
				}
			}
		}
	}
}

void phase_1_out(snake s, mesh grid, queue q) {
	IDL_LONG i, next_i, 
		     nx, ny, npts;

	double x_left, x_right, y_left, y_right, x_max, y_max, 
		   x1, y1, x2, y2, 
		   num, den, m,  hx, hy;

	double *x, *y;

	edge current_edge, final_edge, next_edge;
	vertex v1, v2, to_queue;
	int swapped; 
	int x1_eq_x_left, y1_eq_y_left, x2_eq_x_right, y2_eq_y_right;

	// Saving this variables for less verbosity
	m  = grid->m;
	nx = grid->nx;
	ny = grid->ny;
	hx = grid->hx;
	hy = grid->hy;
	x_max = grid->x_max;
	y_max = grid->y_max;

	npts = s->npts;
	x = s->x;
	y = s->y;

	// We define new variables so (x_left, y_left) is the left point of the segment
	// and (x_right, y_right) is the right point of the segment
	for(i=0; i<npts; i++) {
		next_i = (i+1)%npts;

		//iprintf("======= point %ld of %ld ========", i+1, npts);
		//iprintf("x[i] = (%4.30f,%4.30f)  , x[i+1] = (%4.30f,%4.30f)", x[i], y[i], x[next_i], y[next_i]);
		if (x[i] > x[next_i]) {
			x_left  = x[next_i];
			x_right = x[i];
			y_left  = y[next_i];
			y_right = y[i];
			swapped = 0;
		}

		else {
			x_left  = x[i];
			x_right = x[next_i]; 
			y_left  = y[i];
			y_right = y[next_i]; 
			swapped = 1;
		}


		// num and den are the numerator and the denominator of the slope of the segment
		num = y_right - y_left;
		den = x_right - x_left;

		//iprintf("num = %4.30f, den = %4.30f, num == 0 ? %d, den == 0 ? %d", num, den, double_eq(num, 0.0, CMP_EPSILON), double_eq(den, 0.0, CMP_EPSILON));

		// First if the left points is the same as the right point we ignore it
		// as it is redundant
		if (double_eq(num, 0.0, CMP_EPSILON) && double_eq(den, 0.0, CMP_EPSILON)) continue; 

		// We attack the case when the segment is parallel to the x-axis 
		else if (double_eq(num, 0.0, CMP_EPSILON)) {
			// (x1, y1) and (x2, y2) are setted in such way that the segment they define is
			// the smallest rectangle in the grid that contains the segment 
			// (x_left, y_left)-(x_right, y_right).
			// i1, j1, i2, j2 are they respective indexes in the array 
			// containing the vertices 

			v1 = get_init_vertex(grid, x_left, y_left, 1);
			v2 = get_end_vertex(grid, x_right, y_right, 1);


			x1 = v1->x;
			y1 = v1->y;

			x2 = v2->x;
			y2 = v2->y;


			// Test to see where the initial and final points are
			x1_eq_x_left = double_eq(x1, x_left, CMP_EPSILON);
			y1_eq_y_left = double_eq(y1, y_left, CMP_EPSILON);
			x2_eq_x_right = double_eq(x2, x_right, CMP_EPSILON);
			y2_eq_y_right = double_eq(y2, y_right, CMP_EPSILON);

			//iprintf("swapped = %d", swapped);
			//iprintf("x_left = %f, x1 = %f , x_left == x1 ? %d", x_left, x1, x1_eq_x_left);
			//iprintf("y_left = %f, y1 = %f , y_left == y1 ? %d", y_left, y1, y1_eq_y_left);
			//iprintf("x_right = %f, x2 = %f , x_right == x2 ? %d", x_right, x2, x2_eq_x_right);
			//iprintf("y_right = %f, y2 = %f , y_right == y2 ? %d ", y_right, y2, y2_eq_y_right);

			// We now proceed to determine the initial edge (stored in current_edge)
			// and the final edge to process (stored in final_edge)

			if (swapped) {
				// Case when the segment is contained in a grid line
				if (y1_eq_y_left) { 
					// The initial point is a grid point
					if (x1_eq_x_left)  {
						/*
						current_edge = v1->e[EDGE_N];
						*/

						iprintf("Corrected (%f, %f)", x[i], y[i]);
						x[i] -= 2 * CMP_EPSILON;
						y[i] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// Or not
					else 
						current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];

					// The final point is a grid point
					if (x2_eq_x_right) {
						/*
						final_edge = v2->e[EDGE_NE];
						*/

						iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
						x[(i+1)%npts] -= 2 * CMP_EPSILON;
						y[(i+1)%npts] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// Or not
					else 
						final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_NE];
				}

				else { // The line it's not a grid line
					// The initial point lies in the | edge of a |/ face.

					if (x1_eq_x_left) 
						current_edge = v1->e[EDGE_N];

					// The initial point lies in the |/ face but not in the | edge of its.
					else if (y_left > m*x_left+(y1-m*x1) || 
							double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) 
						current_edge = v1->e[EDGE_NE]; 

					// The initial point lies in the /| face
					else 
						current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];

					// The final point lies in the | edge of a /| face
					if (x2_eq_x_right) 
						final_edge = v2->e[EDGE_S];
					

					// The final point lies in the |/ face but not in the / edge
					else if (y_right > m*x_right+(y2-m*x2) && !double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON))  
						final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];

					// The final point lies in the /| face 
					else 
						final_edge = v2->e[EDGE_SW];
				}


				// This is to test if both points lies in the same triangle
				if (final_edge->f[FACE_RIGHT] == current_edge->f[FACE_LEFT]) 
					continue;
				

				// Do not get confused in the next assigment
				// it's only to preserve the condition in the following loop 
				// which "paints" the grid edges the segment intersects 
				next_edge = current_edge;

				//print_edge(final_edge, "fe");
				while(1) {
					current_edge = next_edge;
					//print_edge(current_edge, "ce");


					// This is the case when the edge is diagonal
					if (current_edge->type == EDGE_D) {
						x1 = current_edge->v[0]->x;
						y1 = current_edge->v[0]->y;

						current_edge->inter_sign++;
						if (current_edge->inter_x == NOT_SET ) {
							current_edge->inter_x = (y_left-(y1-m*x1))/m;
							current_edge->inter_y = y_left;

							to_queue = current_edge->v[0];


							if (to_queue->is_queued == 0) {
								to_queue->is_queued = 1;
								queue_push(q, to_queue);
							}

						}

						else {
							to_queue = current_edge->v[0];

							if (current_edge->inter_sign > 0) {
								current_edge->inter_x = (y_left-(y1-m*x1))/m;
								current_edge->inter_y = y_left;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}

							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[1]->is_queued &&
									!is_still_valid(current_edge->v[1])) {
									queue_remove(q, current_edge->v[1]);
									current_edge->v[1]->is_queued = 0;
								}
							}
						}

						if (current_edge == final_edge) break;

						next_edge = current_edge->f[FACE_RIGHT]->e[1];

					}

					// This is the case when the edge is vertical 
					else { 
						current_edge->inter_sign++;

						if (current_edge->inter_x == NOT_SET) {
							current_edge->inter_x = current_edge->v[0]->x;
							current_edge->inter_y = y_left;

							to_queue = current_edge->v[0];

							if (to_queue->is_queued == 0) {
								to_queue->is_queued = 1;
								queue_push(q, to_queue);
							}

						}

						else {
							to_queue = current_edge->v[0];

							if (current_edge->inter_sign > 0) {
								current_edge->inter_x = current_edge->v[0]->x;
								current_edge->inter_y = y_left;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}

							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[1]->is_queued &&
										!is_still_valid(current_edge->v[1])) {
									queue_remove(q, current_edge->v[1]);
									current_edge->v[1]->is_queued = 0;
								}

							}

							// else if (current_edge->inter_sign < 0) do nada

						}

						if (current_edge == final_edge) break;
						next_edge = current_edge->f[FACE_RIGHT]->e[0];
					}
			
					//Extra check to prevent segfault
					//Further programming may be needed
					if (
						   next_edge->v[0]->x > x2 + hx
						|| next_edge->v[0]->y > y2 + hy
					) {
						iprint_phase_1_segfault(0, swapped,
								x_left, y_left, 
								x_right, y_right,
								x1, y1,
								x2, y2,
								hx, hy,
								nx, ny
						);
						break;
					}
				}
			}

			else {
				// Case when the segment is contained in a grid line
				if (y1_eq_y_left) { 
					// The initial point is a grid point
					if (x1_eq_x_left)  {
						/*
						current_edge = v1->e[EDGE_SW];
						*/

						iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
						x[(i+1)%npts] -= 2 * CMP_EPSILON;
						y[(i+1)%npts] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// Or not
					else
						current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_SW];

					// The final point is a grid point
					if (x2_eq_x_right) {
						/*
						final_edge = v2->e[EDGE_S];
						*/

						iprintf("Corrected (%f, %f)", x[i], y[i]);
						x[i] -= 2 * CMP_EPSILON;
						y[i] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// Or not
					else 
						final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
				}

				else { // The line it's not a grid line
					// The initial point lies in the | edge of a |/ face.

					if (x1_eq_x_left) 
						current_edge = v1->e[EDGE_N];

					// The initial point lies in the |/ face but not in the | edge of its.
					else if (y_left > m*x_left+(y1-m*x1) || double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) 
						current_edge = v1->e[EDGE_NE]; 

					// The initial point lies in the /| face
					else
						current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];

					// The final point lies in the | edge of a /| face
					if (x2_eq_x_right)
						final_edge = v2->e[EDGE_S];

					// The final point lies in the |/ face but not in the / edge
					else if (y_right > m*x_right+(y2-m*x2) && !double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) 
						final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];

					// The final point lies in the /| face 
					else 
						final_edge = v2->e[EDGE_SW];
				}


				// This is to test if both points lies in the same triangle
				if (final_edge->f[FACE_RIGHT] == current_edge->f[FACE_LEFT]) 
					continue;
				

				// Do not get confused in the next assigment
				// it's only to preserve the condition in the following loop 
				// which "paints" the grid edges the segment intersects 
				next_edge = current_edge;

				//print_edge(final_edge, "fe");
				while(1) {
					current_edge = next_edge;
					//print_edge(current_edge, "ce");


					// This is the case when the edge is diagonal
					if (current_edge->type == EDGE_D) {
						x1 = current_edge->v[0]->x;
						y1 = current_edge->v[0]->y;

						current_edge->inter_sign--;

						if (current_edge->inter_x == NOT_SET ) {
							current_edge->inter_x = (y_left-(y1-m*x1))/m;
							current_edge->inter_y = y_left;

							to_queue = current_edge->v[1];

							if (to_queue->is_queued == 0) {
								to_queue->is_queued = 1;
								queue_push(q, to_queue);
							}

						}

						else {
							to_queue = current_edge->v[1];

							if (current_edge->inter_sign < 0) {
								current_edge->inter_x = (y_left-(y1-m*x1))/m;
								current_edge->inter_y = y_left;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}
							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[0]->is_queued &&
										!is_still_valid(current_edge->v[0])) {
									queue_remove(q, current_edge->v[0]);
									current_edge->v[0]->is_queued = 0;
								}
							}

							// else if (current_edge->inter_sign > 0) do nada
						}

						if (current_edge == final_edge) break;

						next_edge = current_edge->f[FACE_RIGHT]->e[1];
					}

					// This is the case when the edge is vertical 
					else {
						current_edge->inter_sign--;

						if (current_edge->inter_x == NOT_SET) {
							current_edge->inter_x = current_edge->v[0]->x;
							current_edge->inter_y = y_left;

							to_queue = current_edge->v[1];

							if (to_queue->is_queued == 0) {
								to_queue->is_queued = 1;
								queue_push(q, to_queue);
							}
						}

						else {

							to_queue = current_edge->v[1];

							if (current_edge->inter_sign < 0) {
								current_edge->inter_x = current_edge->v[0]->x;
								current_edge->inter_y = y_left;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}
							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[0]->is_queued &&
									!is_still_valid(current_edge->v[0])) {
									queue_remove(q, current_edge->v[0]);
									current_edge->v[0]->is_queued = 0;
								}

							}

							// else if (current_edge->inter_sign > 0) do nada

						}
						if (current_edge == final_edge) break;

						next_edge = current_edge->f[FACE_RIGHT]->e[0];
					}
				
					//Extra check to prevent segfault
					//Further programming may be needed
					if (
						   next_edge->v[0]->x > x2 + hx
						|| next_edge->v[0]->y > y2 + hy
					) {
						iprint_phase_1_segfault(0, swapped,
								x_left, y_left, 
								x_right, y_right,
								x1, y1,
								x2, y2,
								hx, hy,
								nx, ny
						);
						break;
					}
				}
			}
		}

		// This is the case when the segment is parallel to the y-axis
		else if (double_eq(den, 0.0, CMP_EPSILON)) { 
			// Switch the variables so (x_left,y_left) is the point on the bottom
			// (there is NO left and right in this case since it is 
			// parallel to the y-axis
			if (y_left > y_right) {
				double aux;
				aux = y_left;
				y_left = y_right;
				y_right  = aux;

				aux = x_left;
				x_left = x_right;
				x_right = x_left;
				swapped = 0;
			}

			// (x1, y1) and (x2, y2) are setted in such way 
			// that the segment they define is the smallest rectangle 
			// in the grid that contains 
			// the segment x_left, y_left)-(x_right, y_right)
			//
			// i1, j1, i2, j2 are they respective indexes 
			// in the array containing the vertices 


			v1 = get_init_vertex(grid, x_left, y_left, 1);
			v2 = get_end_vertex(grid, x_right, y_right, 1);

			x1 = v1->x;
			y1 = v1->y;

			x2 = v2->x;
			y2 = v2->y;

			// Precalculate the comparisons for economy
			x1_eq_x_left = double_eq(x1, x_left, CMP_EPSILON);
			y1_eq_y_left = double_eq(y1, y_left, CMP_EPSILON);
			x2_eq_x_right = double_eq(x2, x_right, CMP_EPSILON);
			y2_eq_y_right = double_eq(y2, y_right, CMP_EPSILON);

			//iprintf("swapped = %d", swapped);
			//iprintf("x_left = %f, x1 = %f , x_left == x1 ? %d", x_left, x1, x1_eq_x_left);
			//iprintf("y_left = %f, y1 = %f , y_left == y1 ? %d", y_left, y1, y1_eq_y_left);
			//iprintf("x_right = %f, x2 = %f , x_right == x2 ? %d", x_right, x2, x2_eq_x_right);
			//iprintf("y_right = %f, y2 = %f , y_right == y2 ? %d ", y_right, y2, y2_eq_y_right);

			if (swapped) {
				// Determine the initial edge (stored in current_edge)
				// and the final edge to process (stored in final_edge)

				// The segment is contained in a grid line
				if (x1_eq_x_left) {
					// The initial point is a grid point
					if (y1_eq_y_left) {
						/*
						current_edge = v1->e[EDGE_SW];
						*/
						iprintf("Corrected (%f, %f)", x[i], y[i]);
						x[i] -= 2 * CMP_EPSILON;
						y[i] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// Or not
					else
						current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_SW];

					// The final point is a grid point
					if (y2_eq_y_right) {
						/*
						final_edge = v2->e[EDGE_W];
						*/

						iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
						x[(i+1)%npts] -= 2 * CMP_EPSILON;
						y[(i+1)%npts] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// Or not
					else 
						final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
				}


				// Case when the line is no a grid line
				else {
					// The initial point lies in the _ edge of a /| face.
					if (y1_eq_y_left) 
						current_edge = v1->e[EDGE_E];

					// The initial point lies in the /| face but not in the _ edge of its.
					else if (y_left < m*x_left+(y1-m*x1) ||
							double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) 
						current_edge = v1->e[EDGE_NE]; 

					// The initial point lies in the |/ face
					else
						current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E];

					// The final point lies in the _ edge of a |/ face
					if (y2_eq_y_right)
						final_edge = v2->e[EDGE_W];

					// The final point lies in the |/ face but not in the _ edge
					else if (y_right > m*x_right+(y2-m*x2) ||
							double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) 
						final_edge = v2->e[EDGE_SW];

					// The final point lies in the /| face 
					else 
						final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
				}

				if (
						final_edge->f[FACE_UP] == current_edge->f[FACE_DOWN] && 
						final_edge->f[FACE_UP] != NULL
				  ) 
					continue;

				// Do not get confused in the next assigment
				// it's only to preserve the condition in the following loop 
				// that "paints" the grid edges that the segment intersects 

				next_edge = current_edge;
				//print_edge(final_edge, "fe");

				while(1) { 
					current_edge = next_edge;
					//print_edge(current_edge, "ce");

					// Case when the edge is Horizontal
					if (current_edge->type == EDGE_H) {
						to_queue = current_edge->v[1];
						current_edge->inter_sign--;

						if (current_edge->inter_x == NOT_SET) {
							current_edge->inter_x = x_left;
							current_edge->inter_y = current_edge->v[0]->y;


							if (to_queue->is_queued == 0) {
								to_queue->is_queued = 1;
								queue_push(q, to_queue);
								//iprintf("queued (%f, %f)", to_queue->x, to_queue->y);
							}
						}

						else {
							if (current_edge->inter_sign < 0) {
								current_edge->inter_x = x_left;
								current_edge->inter_y = current_edge->v[0]->y;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}

							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[0]->is_queued &&
								   !is_still_valid(current_edge->v[0])) {
									queue_remove(q, current_edge->v[0]);
									current_edge->v[0]->is_queued = 0;
								}
							}

							// else if (current_edge->inter_sign > 0) do nada
						}

						if (current_edge == final_edge) 
							break;

						next_edge = current_edge->f[FACE_UP]->e[2];
					}

					else { // Case when the edge is Diagonal
						x1 = current_edge->v[0]->x;
						y1 = current_edge->v[0]->y;

						current_edge->inter_sign--;
						to_queue = current_edge->v[1];

						if (current_edge->inter_x == NOT_SET) {
							current_edge->inter_x = x_left;
							current_edge->inter_y = m*x_left+(y1-m*x1);


							if (to_queue->is_queued == 0) {
								to_queue->is_queued = 1;
								queue_push(q, to_queue);
								//iprintf("queued (%f, %f)", to_queue->x, to_queue->y);

							}
						}

						else {

							if (current_edge->inter_sign < 0) {
								current_edge->inter_x = x_left;
								current_edge->inter_y = m*x_left+(y1-m*x1);
							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[0]->is_queued &&
										!is_still_valid(current_edge->v[0])) {
									queue_remove(q, current_edge->v[0]);
									current_edge->v[0]->is_queued = 0;
								}
							}

							// else if (current_edge->inter_sign > 0) do nada
						}

						if (current_edge == final_edge) 
							break;

						next_edge = current_edge->f[FACE_UP]->e[1];
					}
					
					//Extra check to prevent segfault
					//Further programming may be needed
					if (
						   next_edge->v[0]->x > x2 + hx
						|| next_edge->v[0]->y > y2 + hy
					) {
						iprint_phase_1_segfault(DBL_MAX, swapped,
								x_left, y_left, 
								x_right, y_right,
								x1, y1,
								x2, y2,
								hx, hy,
								nx, ny
						);
						break;
					}
				}
			}

			else { // if (not swapped)
				// Determine the initial edge (stored in current_edge)
				// and the final edge to process (stored in final_edge)

				// The segment is contained in a grid line
				if (x1_eq_x_left) {
					// The initial point is a grid point
					if (y1_eq_y_left) {
						/*
						current_edge = v1->e[EDGE_E];
						*/

						iprintf("Corrected (%f, %f)", x[i], y[i]);
						x[i] -= 2 * CMP_EPSILON;
						y[i] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// Or not
					else
						current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E];

					// The final point is a grid point
					if (y2_eq_y_right) {
						/*
						final_edge = v2->e[EDGE_NE];
						*/

						iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
						x[(i+1)%npts] -= 2 * CMP_EPSILON;
						y[(i+1)%npts] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// Or not
					else 
						final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_NE];
				}


				// Case when the line is no a grid line
				else {
					// The initial point lies in the _ edge of a /| face.
					if (y1_eq_y_left) 
						current_edge = v1->e[EDGE_E];

					// The initial point lies in the /| face but not in the _ edge of its.
					else if (y_left < m*x_left+(y1-m*x1) ||
							double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) 
						current_edge = v1->e[EDGE_NE]; 

					// The initial point lies in the |/ face
					else
						current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E];

					// The final point lies in the _ edge of a |/ face
					if (y2_eq_y_right)
						final_edge = v2->e[EDGE_W];

					// The final point lies in the |/ face but not in the _ edge
					else if (y_right > m*x_right+(y2-m*x2) ||
							double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) 
						final_edge = v2->e[EDGE_SW];

					// The final point lies in the /| face 
					else 
						final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
				}

				if (
						final_edge->f[FACE_UP] == current_edge->f[FACE_DOWN] && 
						final_edge->f[FACE_UP] != NULL
				  ) 
					continue;

				// Do not get confused in the next assigment
				// it's only to preserve the condition in the following loop 
				// that "paints" the grid edges that the segment intersects 

				next_edge = current_edge;

				//print_edge(final_edge, "fe");
				while(1) { 
					current_edge = next_edge;
					//print_edge(current_edge, "ce");

					// Case when the edge is Horizontal
					if (current_edge->type == EDGE_H) {
						current_edge->inter_sign++;
						to_queue = current_edge->v[0];

						if (current_edge->inter_x == NOT_SET) {
							current_edge->inter_x = x_left;
							current_edge->inter_y = current_edge->v[0]->y;


							if (to_queue->is_queued == 0) {
								to_queue->is_queued = 1;
								queue_push(q, to_queue);
								//iprintf("queued (%f, %f)", to_queue->x, to_queue->y);
							}
						}

						else {

							if (current_edge->inter_sign > 0) {
								current_edge->inter_x = x_left;
								current_edge->inter_y = current_edge->v[0]->y;

								if (to_queue->is_queued) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}

							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[1]->is_queued &&
								   !is_still_valid(current_edge->v[1])) {
									queue_remove(q, current_edge->v[1]);
									current_edge->v[1]->is_queued = 0;
								}

							}

							// else if (current_edge->inter_sign < 0) do nada
						}

						if (current_edge == final_edge) 
							break;

						next_edge = current_edge->f[FACE_UP]->e[2];
					}

					else { // Case when the edge is Diagonal
						x1 = current_edge->v[0]->x;
						y1 = current_edge->v[0]->y;

						to_queue = current_edge->v[0];

						current_edge->inter_sign++;

						if (current_edge->inter_x == NOT_SET) {
							current_edge->inter_x = x_left;
							current_edge->inter_y = m*x_left+(y1-m*x1);


							if (to_queue->is_queued == 0) {
								to_queue->is_queued = 1;
								queue_push(q, to_queue);
							}
						}

						else {

							if (current_edge->inter_sign > 0) {
								current_edge->inter_x = x_left;
								current_edge->inter_y = m*x_left+(y1-m*x1);
							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[1]->is_queued &&
								   !is_still_valid(current_edge->v[1])) {
									queue_remove(q, current_edge->v[1]);
									current_edge->v[1]->is_queued = 0;
								}

							}

							// else if (current_edge->inter_sign < 0) do nada
						}

						if (current_edge == final_edge) 
							break;

						next_edge = current_edge->f[FACE_UP]->e[1];
					}
					
					//Extra check to prevent segfault
					//Further programming may be needed
					if (
						   next_edge->v[0]->x > x2 + hx
						|| next_edge->v[0]->y > y2 + hy
					) {
						iprint_phase_1_segfault(DBL_MAX, swapped,
								x_left, y_left, 
								x_right, y_right,
								x1, y1,
								x2, y2,
								hx, hy,
								nx, ny
						);
						break;
					}
				}
			}
		}
		// Case when the segment is oblique (p != 0 && p != infty)
		else {
			double current_x, current_y;
			double t1, t2;
			double p = num/den;
			face iface, fface;

			//iprintf("p = %f", p);

			//iprintf("swapped = %d", swapped);

			if (swapped) {
				// We separate the cases when the segment has
				// slope and when it has negative slope. Notice that
				// the case whe it has zero slope has already been considered
				// in the case when num = 0;
				if (p > 0) {
					// (x1, y1) and (x2, y2) are setted in such way that the segment they define is
					// the smallest rectangle in the grid that contains the segment 
					// (x_left, y_left)-(x_right, y_right).
					// i1, j1, i2, j2 are they respective indexes in the array containing the vertices 


					v1 = get_init_vertex(grid, x_left, y_left, 1);
					v2 = get_end_vertex(grid, x_right, y_right, 1);

					x1 = v1->x;
					y1 = v1->y;

					x2 = v2->x;
					y2 = v2->y;


					x1_eq_x_left = double_eq(x1, x_left, CMP_EPSILON);
					y1_eq_y_left = double_eq(y1, y_left, CMP_EPSILON);
					x2_eq_x_right = double_eq(x2, x_right, CMP_EPSILON);
					y2_eq_y_right = double_eq(y2, y_right, CMP_EPSILON);

					//iprintf("x_left = %4.30f, x1 = %4.30f , x_left == x1 ? %d", x_left, x1, x1_eq_x_left);
					//iprintf("y_left = %4.30f, y1 = %4.30f , y_left == y1 ? %d", y_left, y1, y1_eq_y_left);
					//iprintf("x_right = %4.30f, x2 = %4.30f , x_right == x2 ? %d", x_right, x2, x2_eq_x_right);
					//iprintf("y_right = %4.30f, y2 = %4.30f , y_right == y2 ? %d ", y_right, y2, y2_eq_y_right);

					// Determine the initial edge (stored in current_edge)
					// and the final edge to process (stored in final_edge)

					if (double_eq(p,m, CMP_EPSILON)) {
						// The initial point is the grid point (x1, y1)
						if (x1_eq_x_left && y1_eq_y_left)  {
							/*
							current_edge = v1->e[EDGE_W];

							// Mark the SW edge of v1
							v1->e[EDGE_SW]->inter_sign--;
							if (v1->e[EDGE_SW]->inter_x == NOT_SET ||
							   v1->e[EDGE_SW]->inter_sign < 0) {

								v1->e[EDGE_SW]->inter_x = x_left;
								v1->e[EDGE_SW]->inter_y = y_left;
							}

							else if (v1->e[EDGE_SW]->inter_sign == 0) {
								v1->e[EDGE_SW]->inter_x = NOT_SET;
								v1->e[EDGE_SW]->inter_y = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign > 0) do nada

							current_x = x1;
							current_y = y1;
							iface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[i], y[i]);
							x[i] -= 2 * CMP_EPSILON;
							y[i] += 2 * CMP_EPSILON;
							i--;
							continue;
						}

						// The initial point lies in the | edge of a |/ face.
						else if (x1_eq_x_left) {
							current_edge = v1->e[EDGE_N];

							current_x = x1;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the _ edge of a /| face.
						else if (y1_eq_y_left) {
							current_edge = v1->e[EDGE_E];
							current_x = x_left;
							current_y = y1;
							iface = NULL;
						}

						// The initial point lies in the / edge
						else if (double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) {
							v1->e[EDGE_NE]->inter_sign--;
							if (v1->e[EDGE_NE]->inter_x == NOT_SET ||
							   v1->e[EDGE_NE]->inter_sign < 0) {

								v1->e[EDGE_NE]->inter_x = x_left;
								v1->e[EDGE_NE]->inter_y = y_left;
							}

							else if (v1->e[EDGE_NE]->inter_sign == 0) {
								v1->e[EDGE_NE]->inter_x  = NOT_SET;
								v1->e[EDGE_NE]->inter_y  = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign < 0) do nada
							
							current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E];
							current_x = x_left;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the |/ face 
						// but not in the | edge nor the / edge of its.
						else if (y_left > m*x_left+(y1-m*x1)) {
							t1 = (y1+hy-y_left)/p;
							current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E]; 
							current_x = x_left + t1;
							current_y = v1->e[EDGE_N]->v[1]->y;
							iface = current_edge->f[FACE_DOWN];
						}


						// The initial point lies in the /| face
						else { // if (y_left < m*x_left+(y1-m*x1)) 
							t1 = x1+hx-x_left;

							current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];
							current_x = x_left + t1;
							current_y = y_left + t1*p;
							iface = current_edge->f[FACE_LEFT];

						}

						// We now determine the final edge to process
						if (x2_eq_x_right && y2_eq_y_right) {
							/*
							final_edge = v2->e[EDGE_N];

							// Mark the NE edge of v2
							v2->e[EDGE_NE]->inter_sign++;
							if (v2->e[EDGE_NE]->inter_x == NOT_SET ||
							   v2->e[EDGE_NE]->inter_sign > 0) {

								v2->e[EDGE_NE]->inter_x = x_right;
								v2->e[EDGE_NE]->inter_y = y_right;
							}

							else if (v2->e[EDGE_NE]->inter_sign == 0) {
								v2->e[EDGE_NE]->inter_x = NOT_SET;
								v2->e[EDGE_NE]->inter_y = NOT_SET;
							}

							// else if (v2->e[EDGE_NE]->inter_sign < 0) do nada

							fface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
							x[(i+1)%npts] -= 2 * CMP_EPSILON;
							y[(i+1)%npts] += 2 * CMP_EPSILON;
							i--;
							continue;
						}

						// The final point lies in the | edge of a /| face
						else if (x2_eq_x_right) {
							final_edge = v2->e[EDGE_S];
							fface = NULL;
						}

						// The final point lies in the _ edge of a |/ face
						else if (y2_eq_y_right) {
							final_edge = v2->e[EDGE_W];
							fface = NULL;
						}

						// The final point  lies in the / edge
						else if (double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) {
							v2->e[EDGE_SW]->inter_sign++;
							if (v2->e[EDGE_SW]->inter_x == NOT_SET ||
							   v2->e[EDGE_SW]->inter_sign > 0) {

								v2->e[EDGE_SW]->inter_x = x_left;
								v2->e[EDGE_SW]->inter_y = y_left;
							}

							else if (v2->e[EDGE_SW]->inter_sign == 0) {
								v2->e[EDGE_SW]->inter_x  = NOT_SET;
								v2->e[EDGE_SW]->inter_y  = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign < 0) do nada
							final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
							fface = NULL;
						}

						// The final point lies in the |/ face but not in the / edge
						else if (y_right > m*x_right+(y2-m*x2)) {
							final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
							fface = final_edge->f[FACE_RIGHT];
						}


						// The final point lies in the /| face  but not in the / edge
						// nor the | edge
						else { // if (y_right < m*x_right+(y2-m*x2)) 
							final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
							fface = final_edge->f[FACE_UP];
						}


						// This is to test if the points lay in the
						// same triangle
						if (iface != NULL && fface != NULL && iface == fface) continue;

						next_edge = current_edge;

						//print_edge(final_edge, "fe");
						while(1) {
							current_edge = next_edge;
							//print_edge(current_edge, "ce");

							if (current_edge->type == EDGE_V) {
								current_edge->inter_sign++;
								to_queue = current_edge->v[0];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign > 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[1]->is_queued &&
											!is_still_valid(current_edge->v[1])) {
										queue_remove(q, current_edge->v[1]);
										current_edge->v[1]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign < 0) do nada

								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->y - current_y)/p;

								current_x += t1;
								current_y = current_edge->v[1]->y;

								next_edge = current_edge->v[1]->e[EDGE_E];
							}

							else if (current_edge->type == EDGE_D) {
								//iprintf("if im print you're screwed");
								next_edge = current_edge->v[1]->e[EDGE_S];

							}

							else { // current_edge->type == EDGE_H
								current_edge->inter_sign--;
								to_queue = current_edge->v[1];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign < 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[0]->is_queued &&
											!is_still_valid(current_edge->v[0])) {
										queue_remove(q, current_edge->v[0]);
										current_edge->v[0]->is_queued = 0;
									}
								}


								if (current_edge == final_edge) break;

								t1 = current_edge->v[1]->x - current_x;
								current_x += t1;
								current_y += t1*p;

								next_edge = current_edge->v[1]->e[EDGE_N];

							}
					
							//Extra check to prevent segfault
							//Further programming may be needed
							if (
								   next_edge->v[0]->x > x2 + hx
								|| next_edge->v[0]->y > y2 + hy
							) {
								iprint_phase_1_segfault(p, swapped,
										x_left, y_left, 
										x_right, y_right,
										x1, y1,
										x2, y2,
										hx, hy,
										nx, ny
								);
								break;
							}
						}
					}
					else if (p>m) {
						// The initial point is the grid point (x1, y1)
						if (x1_eq_x_left && y1_eq_y_left)  {
							/*
							current_edge = v1->e[EDGE_W];

							// Mark the SW edge of v1
							v1->e[EDGE_SW]->inter_sign--;
							if (v1->e[EDGE_SW]->inter_x == NOT_SET ||
							   v1->e[EDGE_SW]->inter_sign < 0) {

								v1->e[EDGE_SW]->inter_x = x_left;
								v1->e[EDGE_SW]->inter_y = y_left;
							}

							else if (v1->e[EDGE_SW]->inter_sign == 0) {
								v1->e[EDGE_SW]->inter_x = NOT_SET;
								v1->e[EDGE_SW]->inter_y = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign > 0) do nada 

							current_x = x1;
							current_y = y1;
							iface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[i], y[i]);
							x[i] -= 2 * CMP_EPSILON;
							y[i] += 2 * CMP_EPSILON;
							i--;
							continue;

						}
						// The initial point lies in the | edge of a |/ face.
						else if (x1_eq_x_left) {
							current_edge = v1->e[EDGE_N];
							current_x = x1;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the _ edge of a /| face.
						else if (y1_eq_y_left) {
							current_edge = v1->e[EDGE_E];
							current_x = x_left;
							current_y = y1;
							iface = NULL;
						}


						// The initial point lies in the / edge
						else if (double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) {
							current_edge = v1->e[EDGE_NE];
							current_x = x_left;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the |/ face 
						// but not in the | edge nor the / edge of its.
						else if (y_left > m*x_left+(y1-m*x1)) {
							t1 = (y1+hy-y_left)/p;
							current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E]; 
							current_x = x_left + t1;
							current_y = v1->e[EDGE_N]->v[1]->y;
							iface = current_edge->f[FACE_DOWN];
						}


						// The initial point lies in the /| face
						else { // if (y_left < m*x_left+(y1-m*x1)) 
							t1 = x1+hx-x_left;
							t2 = ((x1-x_left)-(y1-y_left)/m)/(1-p/m);

							if (double_eq(t1, t2, CMP_EPSILON)) {
								current_edge = v1->e[EDGE_NE];
								current_x = x_left + t2;
								current_y = y_left + t2*p;
								iface = current_edge->f[FACE_DOWN];
								
							}

							else if (t1 < t2) {
								current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];
								current_x = x_left + t1;
								current_y = y_left + t1*p;
								iface = current_edge->f[FACE_LEFT];
							}

							else {//  if (t1 > t2)
								current_edge = v1->e[EDGE_NE];
								current_x = x_left + t2;
								current_y = y_left + t2*p;
								iface = current_edge->f[FACE_DOWN];
							}
						}

						// We now determine the final edge to process
						if (x2_eq_x_right && y2_eq_y_right) { 
							/*
							final_edge = v2->e[EDGE_N];

							// Mark the NE edge of v2
							v2->e[EDGE_NE]->inter_sign++;
							if (v2->e[EDGE_NE]->inter_x == NOT_SET ||
							   v2->e[EDGE_NE]->inter_sign > 0) {

								v2->e[EDGE_NE]->inter_x = x_right;
								v2->e[EDGE_NE]->inter_y = y_right;

							}

							else if (v2->e[EDGE_NE]->inter_sign == 0) {
								v2->e[EDGE_NE]->inter_x = NOT_SET;
								v2->e[EDGE_NE]->inter_y = NOT_SET;
							}

							// else if (v2->e[EDGE_NE]->inter_sign < 0) do nada

							fface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
							x[(i+1)%npts] -= 2 * CMP_EPSILON;
							y[(i+1)%npts] += 2 * CMP_EPSILON;
							i--;
							continue;
						}

						// The final point lies in the | edge of a /| face
						else if (x2_eq_x_right)  {
							final_edge = v2->e[EDGE_S];
							fface = NULL;
						}

						// The final point lies in the _ edge of a |/ face
						else if (y2_eq_y_right) {
							final_edge = v2->e[EDGE_W];
							fface = NULL;
						}

						// The final point  lies in the / edge
						else if (double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) {
							final_edge = v2->e[EDGE_SW];
							fface = NULL;
						}

						// The final point lies in the |/ face but not in the / edge
						// or the _ edge
						else if (y_right > m*x_right+(y2-m*x2)) {
							t1 = ((x2-x_right)-(y2-y_right)/m)/(1-p/m);
							t2 = x2-hx-x_right;

							if (double_eq(t1, t2, CMP_EPSILON)) {
								final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
								fface = final_edge->f[FACE_RIGHT];
							}

							else if (fabs(t1) < fabs(t2)) {
								final_edge = v2->e[EDGE_SW];
								fface = final_edge->f[FACE_UP]; 
							}

							else  { // if (t1 > t2)
								final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
								fface = final_edge->f[FACE_RIGHT];
							}
						}


						// The final point lies in the /| face  but not in the / edge
						// nor the | edge
						else  { // if (y_right < m*x_right+(y2-m*x2)) 
							final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
							fface = final_edge->f[FACE_UP];
						}

						if (iface != NULL && fface != NULL && iface == fface) continue;

						next_edge = current_edge;

						//print_edge(final_edge, "fe");
						while(1) {
							current_edge = next_edge;
							//print_edge(current_edge, "ce");
							if (current_edge->type == EDGE_V) {
								current_edge->inter_sign++;
								to_queue = current_edge->v[0];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign > 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0){
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[1]->is_queued &&
											!is_still_valid(current_edge->v[1])) {
										queue_remove(q, current_edge->v[1]);
										current_edge->v[1]->is_queued = 0;
									}

								}

								// else if (current_edge->inter_sign < 0) do nada

								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->y - current_y)/p;

								current_x += t1;
								current_y = current_edge->v[1]->y;

								next_edge = current_edge->v[1]->e[EDGE_E];
							}

							else if (current_edge->type == EDGE_D) {
								current_edge->inter_sign--;
								to_queue = current_edge->v[1];


								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign < 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;

										queue_push(q, to_queue);
										//iprintf("queued (%f, %f)", to_queue->x, to_queue->y);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[0]->is_queued &&
								   	   !is_still_valid(current_edge->v[0])) {
										queue_remove(q, current_edge->v[0]);
										current_edge->v[0]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign > 0) do nada


								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->y - current_y)/p;

								current_x += t1;
								current_y = current_edge->v[1]->y;

								next_edge = current_edge->v[1]->e[EDGE_W];
							}

							else { // current_edge->type == EDGE_H
								current_edge->inter_sign--;
								to_queue = current_edge->v[1];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign < 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
										//iprintf("queued (%f, %f)", to_queue->x, to_queue->y);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[0]->is_queued &&
											!is_still_valid(current_edge->v[0])) {
										queue_remove(q, current_edge->v[0]);
										current_edge->v[0]->is_queued = 0;
									}

								}

								// else if (current_edge->inter_sign > 0) do nada


								if (current_edge == final_edge) break;

								t1 = current_edge->v[1]->x - current_x;
								t2 = current_edge->v[0]->x - current_x;
								t2 -= (current_edge->v[0]->y - current_y)/m;
								t2 /= 1-p/m;

								if (double_eq(t1, 0.0, CMP_EPSILON)) {
									next_edge = current_edge->v[1]->e[EDGE_N];
								}

								else if (double_eq(t1,t2, CMP_EPSILON)) {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[0]->e[EDGE_NE];

								}
								else if (t1 < t2) {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[1]->e[EDGE_N];
								}

								else if (t1 > t2) {
									current_x += t2;
									current_y += t2*p;
									next_edge = current_edge->v[0]->e[EDGE_NE];
								}

							}
							
							//Extra check to prevent segfault
							//Further programming may be needed
							if (
								   next_edge->v[0]->x > x2 + hx
								|| next_edge->v[0]->y > y2 + hy
							) {
								iprint_phase_1_segfault(p, swapped,
										x_left, y_left, 
										x_right, y_right,
										x1, y1,
										x2, y2,
										hx, hy,
										nx, ny
								);
								break;
							}
						}
					}

					else { // if (0<p<m)
						if (x1_eq_x_left && y1_eq_y_left)  {
							/*
							current_edge = v1->e[EDGE_W];

							// Mark the SW edge of v1
							v1->e[EDGE_SW]->inter_sign--;
							if (v1->e[EDGE_SW]->inter_x == NOT_SET ||
							   v1->e[EDGE_SW]->inter_sign < 0) {
								v1->e[EDGE_SW]->inter_x = x_left;
								v1->e[EDGE_SW]->inter_y = y_left;
							}

							else if (v1->e[EDGE_SW]->inter_sign == 0) {
								v1->e[EDGE_SW]->inter_x = NOT_SET;
								v1->e[EDGE_SW]->inter_y = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign == 0) do nada

							current_x = x1;
							current_y = y1;
							iface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[i], y[i]);
							x[i] -= 2 * CMP_EPSILON;
							y[i] += 2 * CMP_EPSILON;
							i--;
							continue;
						}

						// The initial point lies in the | edge of a |/ face.
						else if (x1_eq_x_left) {
							current_edge = v1->e[EDGE_N];
							current_x = x1;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the _ edge of a /| face.
						else if (y1_eq_y_left) {
							current_edge = v1->e[EDGE_E];
							current_x = x_left;
							current_y = y1;
							iface = NULL;
						}

						// The initial point lies in the / edge
						else if (double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) {
							current_edge = v1->e[EDGE_NE];
							current_x = x_left;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the |/ face 
						// but not in the | edge nor the / edge of its.
						else if (y_left > m*x_left+(y1-m*x1)) {
							t1 = ((x1-x_left)-(y1-y_left)/m)/(1-p/m);
							t2 = (y1+hy-y_left)/p;

							if (double_eq(t1, t2, CMP_EPSILON)) {
								current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E];
								current_x = x_left + t2;
								current_y = y_left + t2*p;
								iface = current_edge->f[FACE_DOWN];
							}

							else if (t1 < t2) {
								current_edge = v1->e[EDGE_NE]; 
								current_x = x_left + t1;
								current_y = y_left + t1*p;
								iface = current_edge->f[FACE_LEFT];
							}

							else { // if (t1 > t2) 
								current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E];
								current_x = x_left + t2;
								current_y = y_left + t2*p;
								iface = current_edge->f[FACE_DOWN];
							}
						}


						// The initial point lies in the /| face
						else { // if (y_left < m*x_left+(y1-m*x1)) 
							t1 = x1+hx-x_left;
							current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];
							current_x = x_left + t1;
							current_y = y_left + t1*p;
							iface = current_edge->f[FACE_LEFT];
						}


						// We now determine the final edge to process
						if (x2_eq_x_right && y2_eq_y_right) {
							/*
							final_edge = v2->e[EDGE_N];

							// Mark the NE edge of v2
							v2->e[EDGE_NE]->inter_sign++;
							if (v2->e[EDGE_NE]->inter_x == NOT_SET ||
						       v2->e[EDGE_NE]->inter_sign > 0) {

								v2->e[EDGE_NE]->inter_x = x_right;
								v2->e[EDGE_NE]->inter_y = y_right;

							}

							else if (v2->e[EDGE_NE]->inter_sign == 0) {

								v2->e[EDGE_NE]->inter_x = NOT_SET;
								v2->e[EDGE_NE]->inter_y = NOT_SET;
							}

							// else if (v2->e[EDGE_NE]->inter_sign < 0) do nada 

							fface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
							x[(i+1)%npts] -= 2 * CMP_EPSILON;
							y[(i+1)%npts] += 2 * CMP_EPSILON;
							i--;
							continue;
						}


						// The final point lies in the | edge of a /| face
						else if (x2_eq_x_right) {
							final_edge = v2->e[EDGE_S];
							fface = NULL;
						}

						// The final point lies in the _ edge of a |/ face
						else if (y2_eq_y_right) {
							final_edge = v2->e[EDGE_W];
							fface = NULL;
						}

						// The final point  lies in the / edge
						else if (double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) {
							final_edge = v2->e[EDGE_SW];
							fface = NULL;
						}


						// The final point lies in the |/ face but not in the / edge
						else if (y_right > m*x_right+(y2-m*x2)) {
							t1 = x2-hx-x_right;
							final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
							fface = final_edge->f[FACE_RIGHT];
						}


						// The final point lies in the /| face  but not in the / edge
						// nor the | edge
						else { // if (y_right < m*x_right+(y2-m*x2)) 
							t1 = -(y2-hy-y_right)/p;
							t2 = -((x2-x_right)-(y2-y_right)/m)/(1-p/m);

							if (double_eq(t1, t2, CMP_EPSILON)) {
								final_edge = v2->e[EDGE_SW];
								fface = final_edge->f[FACE_RIGHT];
							}

							else if (t1 < t2) { 
								final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
								fface = final_edge->f[FACE_UP];
							}

							else  {
								final_edge = v2->e[EDGE_SW];
								fface = final_edge->f[FACE_RIGHT];
							}
						}

						if (iface != NULL && fface != NULL && iface == fface) continue;

						next_edge = current_edge;

						//print_edge(final_edge, "fe");
						while(1) {
							current_edge = next_edge;
							//print_edge(current_edge, "ce");

							if (current_edge->type == EDGE_H) {
								current_edge->inter_sign--;
								to_queue = current_edge->v[1];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign < 0) {

									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;


									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[0]->is_queued &&
											!is_still_valid(current_edge->v[0])) {
										queue_remove(q, current_edge->v[0]);
										current_edge->v[0]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign > 0) do nada 

								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->x - current_x);

								current_x = current_edge->v[1]->x;
								current_y += t1*p;

								next_edge = current_edge->v[1]->e[EDGE_N];
							}

							else if (current_edge->type == EDGE_D) {
								current_edge->inter_sign++;
								to_queue = current_edge->v[0];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign > 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[1]->is_queued &&
									   !is_still_valid(current_edge->v[1])) {
										queue_remove(q, current_edge->v[1]);
										current_edge->v[1]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign < 0) 

								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->x - current_x);
								current_x = current_edge->v[1]->x;
								current_y += t1 *p;
								next_edge = current_edge->v[1]->e[EDGE_S];
							}

							else { // current_edge->type == EDGE_V
								current_edge->inter_sign++;
								to_queue = current_edge->v[0];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign > 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[1]->is_queued &&
											!is_still_valid(current_edge->v[1])) {
										queue_remove(q, current_edge->v[1]);
										current_edge->v[1]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign < 0) do nada

								if (current_edge == final_edge) break;

								t1 = current_edge->v[0]->x - current_x;
								t1 -= (current_edge->v[0]->y - current_y)/m;
								t1 /= 1-p/m;
								t2 = (current_edge->v[1]->y - current_y)/p;

								if (double_eq(t1, 0.0, CMP_EPSILON)) 
									next_edge = current_edge->v[0]->e[EDGE_NE];

								else if (double_eq(t2, 0.0, CMP_EPSILON)) {
									next_edge = current_edge->v[0]->e[EDGE_NE];
								}

								else if (double_eq(t1,t2, CMP_EPSILON)) {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[1]->e[EDGE_E];
								}

								else if (t1 < t2) {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[0]->e[EDGE_NE];
								}

								else if (t1 > t2)  {
									current_x += t2;
									current_y += t2*p;
									next_edge = current_edge->v[1]->e[EDGE_E];
								}

								else  {
									current_x += t1;
									current_y += t1*p;

									next_edge = current_edge->v[1]->e[EDGE_NE];
								}
							}
					
							//Extra check to prevent segfault
							//Further programming may be needed
							if (
								   next_edge->v[0]->x > x2 + hx
								|| next_edge->v[0]->y > y2 + hy
							) {
								iprint_phase_1_segfault(p, swapped,
										x_left, y_left, 
										x_right, y_right,
										x1, y1,
										x2, y2,
										hx, hy,
										nx, ny
								);
								break;
							}
						}
					} 
				}


				else { // if (p < 0) 
					// (x1, y1) and (x2, y2) are setted in such way that the segment they define is
					// the smallest rectangle in the grid that contains the segment 
					// (x_left, y_left)-(x_right, y_right).
					// i1, j1, i2, j2 are they respective indexes in the array containing the vertices 


					v1 = get_init_vertex(grid, x_left, y_left, -1);
					v2 = get_end_vertex(grid, x_right, y_right, -1);

					x1 = v1->x;
					y1 = v1->y;

					x2 = v2->x;
					y2 = v2->y;

					x1_eq_x_left = double_eq(x1, x_left, CMP_EPSILON);
					y1_eq_y_left = double_eq(y1, y_left, CMP_EPSILON);
					x2_eq_x_right = double_eq(x2, x_right, CMP_EPSILON);
					y2_eq_y_right = double_eq(y2, y_right, CMP_EPSILON);

					//iprintf("x_left = %f, x1 = %f , x_left == x1 ? %d", x_left, x1, x1_eq_x_left);
					//iprintf("y_left = %f, y1 = %f , y_left == y1 ? %d dif=%3.20f", y_left, y1, y1_eq_y_left, y_left-y1);
					//iprintf("x_right = %f, x2 = %f , x_right == x2 ? %d", x_right, x2, x2_eq_x_right);
					//iprintf("y_right = %f, y2 = %f , y_right == y2 ? %d  dif=%3.20f", y_right, y2, y2_eq_y_right, y2-y_right);

					// The initial point is the grid point (x1, y1)
					if (x1_eq_x_left && y1_eq_y_left) {
						/*
						current_edge = v1->e[EDGE_N];
						current_x = x1;
						current_y = y1;
						iface = NULL;
						*/

						iprintf("Corrected (%f, %f)", x[i], y[i]);
						x[i] -= 2 * CMP_EPSILON;
						y[i] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// The initial point lies in the | edge of a |/ face.
					else if (x1_eq_x_left) {
						current_edge = v1->e[EDGE_S];
						current_x = x1;
						current_y = y_left;
						iface = NULL;
					}

					// The initial point lies in the _ edge of a |/ face.
					else if (y1_eq_y_left) {
						current_edge = v1->e[EDGE_E];
						current_x = x_left;
						current_y = y1;
						iface = NULL;
					}

					// The initial point lies in the / edge
					else if (double_eq(y_left, m*x_left+(y1-hy-m*x1), CMP_EPSILON)) {
						current_edge = v1->e[EDGE_S]->v[0]->e[EDGE_NE];
						current_x = x_left;
						current_y = y_left;

						iface = NULL;
					}

					// The initial point lies in the |/ face 
					// but not in the | edge nor the / edge of its.
					else if (y_left > m*x_left+(y1-hy-m*x1)) {
						current_edge = v1->e[EDGE_S]->v[0]->e[EDGE_NE]; 

						t1 = current_edge->v[0]->x - x_left;
						t1 -= (current_edge->v[0]->y - y_left)/m;
						t1 /= 1-p/m;

						current_x = x_left + t1;
						current_y = y_left + t1*p;

						iface = current_edge->f[FACE_UP];
					}

					// The initial point lies in the /| face
					else { // if (y_left < m*x_left+(y1-m*x1)) 
						t1 = (y1-hy-y_left)/p;
						t2 = x1+hx-x_left;

						if (double_eq(t1, t2, CMP_EPSILON)) {
							current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_S];
							current_x = current_edge->v[0]->x;
							current_y = y_left + t2*p;
							iface = current_edge->f[FACE_LEFT];
						}

						else if (t1 < t2) {
							current_edge = v1->e[EDGE_S]->v[0]->e[EDGE_E];
							current_x = x_left + t1;;
							current_y = current_edge->v[0]->y;
							iface = current_edge->f[FACE_UP];
						}

						else {//  if (t1 > t2)
							current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_S];
							current_x = current_edge->v[0]->x;
							current_y = y_left + t2*p;
							iface = current_edge->f[FACE_LEFT];
						}
					}

					// We now determine the final edge to process
					if (x2_eq_x_right && y2_eq_y_right){ 
						/*
						final_edge = v2->e[EDGE_E];
						fface = NULL;
						*/

						iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
						x[(i+1)%npts] -= 2 * CMP_EPSILON;
						y[(i+1)%npts] += 2 * CMP_EPSILON;
						i--;
						continue;
					}


					// The final point lies in the | edge of a /| face
					else if (x2_eq_x_right) {
						final_edge = v2->e[EDGE_N];
						fface = NULL;
					}

					// The final point lies in the _ edge of a /| face
					else if (y2_eq_y_right) {
						final_edge = v2->e[EDGE_W];
						fface = NULL;
					}

					// The final point  lies in the / edge
					else if (double_eq(y_right,m*x_right+(y2-m*(x2-hx)), CMP_EPSILON)) {
						final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_NE];
						fface = NULL;
					}

					// The final point lies in the |/ face but not in the / edge
					else if (y_right > m*x_right+(y2-m*(x2-hx))) {
						t1 = (y2+hy-y_right)/p;
						t2 = x2-hx-x_right;

						if (double_eq(t1, t2, CMP_EPSILON)) {
							final_edge = v2->e[EDGE_N]->v[1]->e[EDGE_W];
							fface = final_edge->f[FACE_DOWN];
						}

						else if (fabs(t1) < fabs(t2)) {
							final_edge = v2->e[EDGE_N]->v[1]->e[EDGE_W];
							fface = final_edge->f[FACE_DOWN];
						}

						else  {// if (t1 > t2)
							final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_N];
							fface = final_edge->f[FACE_RIGHT];
						}
					}


					// The final point lies in the /| face  but not in the / edge
					// nor the | edge
					else { // if (y_right < m*x_right+(y2-m*x2)) 
						final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_NE];
						fface = final_edge->f[FACE_DOWN];
					}

					if (iface != NULL && fface != NULL && iface == fface) continue;

					next_edge = current_edge;

					//print_edge(final_edge, "fe");
					while(1) {
						current_edge = next_edge;
						//print_edge(current_edge, "ce");

						if (current_edge->type == EDGE_V) {
							current_edge->inter_sign++;
							to_queue = current_edge->v[0];

							if (current_edge->inter_x == NOT_SET ||
							   current_edge->inter_sign > 0) {
								current_edge->inter_x = current_x;
								current_edge->inter_y = current_y;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}
							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[1]->is_queued &&
										!is_still_valid(current_edge->v[1])) {
									queue_remove(q, current_edge->v[1]);
									current_edge->v[1]->is_queued = 0;
								}
							}

							// else if (current_edge->inter_sign < 0) do nada

							if (current_edge == final_edge) break;

							t1 = current_edge->v[0]->x - current_x;
							t1 -= (current_edge->v[0]->y - current_y)/m;
							t1 /= 1-p/m;

							current_x += t1;
							current_y += t1*p;

							next_edge = current_edge->v[0]->e[EDGE_NE];
						}

						else if (current_edge->type == EDGE_D) {
							current_edge->inter_sign++;
							to_queue = current_edge->v[0];

							if (current_edge->inter_x == NOT_SET ||
							   current_edge->inter_sign > 0) {
								current_edge->inter_x = current_x;
								current_edge->inter_y = current_y;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
									//iprintf("queued (%f, %f)", to_queue->x, to_queue->y);
								}
							}

							else if (current_edge->inter_sign == 0) { 
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[1]->is_queued &&
										!is_still_valid(current_edge->v[1])) {
									queue_remove(q, current_edge->v[1]);
									current_edge->v[1]->is_queued = 0;
								}
							}

							// else if (current_edge->inter_sign < 0) do nada

							if (current_edge == final_edge) break;

							t1 = (current_edge->v[0]->y-current_y)/p;
							t2 = current_edge->v[1]->x-current_x;


							if (double_eq(t1, t2, CMP_EPSILON)) {
								current_x += t1;
								current_y += t1*p;
								next_edge = current_edge->v[1]->e[EDGE_S];
							}

							else if (double_eq(t1, 0.0, CMP_EPSILON)) {
								next_edge = current_edge->v[0]->e[EDGE_E];
							}

							else if (double_eq(t2, 0.0, CMP_EPSILON)) {
								next_edge = current_edge->v[1]->e[EDGE_S];
							}

							else if (t1 < t2) {
								current_x += t1;
								current_y = current_edge->v[0]->y;

								next_edge = current_edge->v[0]->e[EDGE_E];
							}

							else { // if (t1 > t2) 
								current_x = current_edge->v[1]->x;
								current_y += t2*p;

								next_edge = current_edge->v[1]->e[EDGE_S];
							}
						}


						else { // current_edge->type == EDGE_H
							current_edge->inter_sign++;
							to_queue = current_edge->v[0];

							if (current_edge->inter_x == NOT_SET ||
							   current_edge->inter_sign > 0) {
								current_edge->inter_x = current_x;
								current_edge->inter_y = current_y;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
									//iprintf("queued (%f, %f)", to_queue->x, to_queue->y);
								}
							}

							else if (current_edge->inter_sign == 0) {

								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[1]->is_queued &&
										!is_still_valid(current_edge->v[1])) {
									queue_remove(q, current_edge->v[1]);
									current_edge->v[1]->is_queued = 0;
								}
							}

							// else if (current_edge->inter_sign < 0) do nada

							if (current_edge == final_edge) break;

							t1 = current_edge->v[0]->x - current_x;
							t1 -= (current_edge->v[0]->e[EDGE_S]->v[0]->y - current_y)/m;
							t1 /= 1-p/m;

							current_x += t1;
							current_y += t1*p;

							next_edge = current_edge->v[1]->e[EDGE_SW];
						}
				
						//Extra check to prevent segfault
						//Further programming may be needed
						if (
							   next_edge->v[0]->x > x2 + hx
							|| next_edge->v[0]->y < y2 - hy
						) {
							iprint_phase_1_segfault(p, swapped,
									x_left, y_left, 
									x_right, y_right,
									x1, y1,
									x2, y2,
									hx, hy,
									nx, ny
							);
							break;
						}
					}
				}
			}

			else { // if (not swapped)
				// We separate the cases when the segment has
				// slope and when it has negative slope. Notice that
				// the case whe it has zero slope has already been considered
				// in the case when num = 0;
				if (p > 0) {
					// (x1, y1) and (x2, y2) are setted in such way that the segment they define is
					// the smallest rectangle in the grid that contains the segment 
					// (x_left, y_left)-(x_right, y_right).
					// i1, j1, i2, j2 are they respective indexes in the array containing the vertices 


					v1 = get_init_vertex(grid, x_left, y_left, 1);
					v2 = get_end_vertex(grid, x_right, y_right, 1);

					x1 = v1->x;
					y1 = v1->y;

					x2 = v2->x;
					y2 = v2->y;


					x1_eq_x_left = double_eq(x1, x_left, CMP_EPSILON);
					y1_eq_y_left = double_eq(y1, y_left, CMP_EPSILON);
					x2_eq_x_right = double_eq(x2, x_right, CMP_EPSILON);
					y2_eq_y_right = double_eq(y2, y_right, CMP_EPSILON);

					//iprintf("x_left = %f, x1 = %f , x_left == x1 ? %d", x_left, x1, x1_eq_x_left);
					//iprintf("y_left = %f, y1 = %f , y_left == y1 ? %d", y_left, y1, y1_eq_y_left);
					//iprintf("x_right = %f, x2 = %f , x_right == x2 ? %d", x_right, x2, x2_eq_x_right);
					//iprintf("y_right = %f, y2 = %f , y_right == y2 ? %d", y_right, y2, y2_eq_y_right);

					// Determine the initial edge (stored in current_edge)
					// and the final edge to process (stored in final_edge)

					if (double_eq(p,m, CMP_EPSILON)) {
						// The initial point is the grid point (x1, y1)
						if (x1_eq_x_left && y1_eq_y_left)  {
							/*
							current_edge = v1->e[EDGE_S];


							// Mark the SW edge of v1
							v1->e[EDGE_SW]->inter_sign--;
							if (v1->e[EDGE_SW]->inter_x == NOT_SET ||
							   v1->e[EDGE_SW]->inter_sign < 0) {

								v1->e[EDGE_SW]->inter_x = x_left;
								v1->e[EDGE_SW]->inter_y = y_left;
							}

							else if (v1->e[EDGE_SW]->inter_sign == 0) {
								v1->e[EDGE_SW]->inter_x = NOT_SET;
								v1->e[EDGE_SW]->inter_y = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign < 0) do nada

							current_x = x1;
							current_y = y1;
							iface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
							x[(i+1)%npts] -= 2 * CMP_EPSILON;
							y[(i+1)%npts] += 2 * CMP_EPSILON;
							i--;
							continue;
						}

						// The initial point lies in the | edge of a |/ face.
						else if (x1_eq_x_left) {
							current_edge = v1->e[EDGE_N];

							current_x = x1;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the _ edge of a /| face.
						else if (y1_eq_y_left) {
							current_edge = v1->e[EDGE_E];
							current_x = x_left;
							current_y = y1;
							iface = NULL;
						}

						// The initial point lies in the / edge
						else if (double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) {
							v1->e[EDGE_NE]->inter_sign--;
							if (v1->e[EDGE_NE]->inter_x == NOT_SET ||
							   v1->e[EDGE_NE]->inter_sign < 0) {

								v1->e[EDGE_NE]->inter_x = x_left;
								v1->e[EDGE_NE]->inter_y = y_left;
							}

							else if (v1->e[EDGE_NE]->inter_sign == 0) {
								v1->e[EDGE_NE]->inter_x  = NOT_SET;
								v1->e[EDGE_NE]->inter_y  = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign < 0) do nada
							
							current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];
							current_x = x_left;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the |/ face 
						// but not in the | edge nor the / edge of its.
						else if (y_left > m*x_left+(y1-m*x1)) {
							t1 = (y1+hy-y_left)/p;
							current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E]; 
							current_x = x_left + t1;
							current_y = v1->e[EDGE_N]->v[1]->y;
							iface = current_edge->f[FACE_DOWN];
						}


						// The initial point lies in the /| face
						else { // if (y_left < m*x_left+(y1-m*x1)) 
							t1 = x1+hx-x_left;

							current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];
							current_x = x_left + t1;
							current_y = y_left + t1*p;
							iface = current_edge->f[FACE_LEFT];

						}

						// We now determine the final edge to process
						if (x2_eq_x_right && y2_eq_y_right) {
							/*
							final_edge = v2->e[EDGE_E];

							// Mark the NE edge of v2
							v2->e[EDGE_NE]->inter_sign--;
							if (v2->e[EDGE_NE]->inter_x == NOT_SET ||
							   v2->e[EDGE_NE]->inter_sign < 0) {

								v2->e[EDGE_NE]->inter_x = x_right;
								v2->e[EDGE_NE]->inter_y = y_right;

							}

							else if (v2->e[EDGE_NE]->inter_sign == 0) {
								v2->e[EDGE_NE]->inter_x = NOT_SET;
								v2->e[EDGE_NE]->inter_y = NOT_SET;
							}

							// else if (v2->e[EDGE_NE]->inter_sign > 0) do nada 

							fface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[i], y[i]);
							x[i] -= 2 * CMP_EPSILON;
							y[i] += 2 * CMP_EPSILON;
							i--;
							continue;
						}

						// The final point lies in the | edge of a /| face
						else if (x2_eq_x_right) {
							final_edge = v2->e[EDGE_S];
							fface = NULL;
						}

						// The final point lies in the _ edge of a |/ face
						else if (y2_eq_y_right) {
							final_edge = v2->e[EDGE_W];
							fface = NULL;
						}

						// The final point  lies in the / edge
						else if (double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) {
							v2->e[EDGE_SW]->inter_sign++;
							if (v2->e[EDGE_SW]->inter_x == NOT_SET ||
							   v2->e[EDGE_SW]->inter_sign > 0) {

								v2->e[EDGE_SW]->inter_x = x_left;
								v2->e[EDGE_SW]->inter_y = y_left;
							}

							else if (v2->e[EDGE_SW]->inter_sign == 0) {
								v2->e[EDGE_SW]->inter_x  = NOT_SET;
								v2->e[EDGE_SW]->inter_y  = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign < 0) do nada
							final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
							fface = NULL;
						}

						// The final point lies in the |/ face but not in the / edge
						else if (y_right > m*x_right+(y2-m*x2)) {
							final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
							fface = final_edge->f[FACE_RIGHT];
						}


						// The final point lies in the /| face  but not in the / edge
						// nor the | edge
						else { // if (y_right < m*x_right+(y2-m*x2)) 
							final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
							fface = final_edge->f[FACE_UP];
						}


						// This is to test if the points lay in the
						// same triangle
						if (iface != NULL && fface != NULL && iface == fface) continue;

						next_edge = current_edge;

						//print_edge(final_edge, "fe");
						while(1) {
							current_edge = next_edge;
							//print_edge(current_edge, "ce");

							if (current_edge->type == EDGE_V) {
								current_edge->inter_sign--;
								to_queue = current_edge->v[1];


								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign < 0) {

									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[0]->is_queued &&
									   !is_still_valid(current_edge->v[0])) {
										queue_remove(q, current_edge->v[0]);
										current_edge->v[0]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign > 0) do nada

								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->y - current_y)/p;

								current_x += t1;
								current_y = current_edge->v[1]->y;

								next_edge = current_edge->v[1]->e[EDGE_E];
							}

							else if (current_edge->type == EDGE_D) {
								//iprintf("if im print you're screwed");
								next_edge = current_edge->v[1]->e[EDGE_W];
							}

							else { // current_edge->type == EDGE_H
								current_edge->inter_sign++;
								to_queue = current_edge->v[0];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign > 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[1]->is_queued &&
											!is_still_valid(current_edge->v[1])) {
										queue_remove(q, current_edge->v[1]);
										current_edge->v[1]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign < 0) 

								if (current_edge == final_edge) break;

								t1 = current_edge->v[1]->x - current_x;

								current_x += t1;
								current_y += t1*p;

								next_edge = current_edge->v[1]->e[EDGE_N];

							}
							
							//Extra check to prevent segfault
							//Further programming may be needed
							if (
								   next_edge->v[0]->x > x2 + hx
								|| next_edge->v[0]->y > y2 + hy
							) {
								iprint_phase_1_segfault(p, swapped,
										x_left, y_left, 
										x_right, y_right,
										x1, y1,
										x2, y2,
										hx, hy,
										nx, ny
								);
								break;
							}
						}
					}


					else if (p>m) {
						// The initial point is the grid point (x1, y1)
						if (x1_eq_x_left && y1_eq_y_left)  {
							/*
							current_edge = v1->e[EDGE_S];

							// Mark the SW edge of v1
							v1->e[EDGE_SW]->inter_sign--;
							if (v1->e[EDGE_SW]->inter_x == NOT_SET ||
							   v1->e[EDGE_SW]->inter_sign < 0) {

								v1->e[EDGE_SW]->inter_x = x_left;
								v1->e[EDGE_SW]->inter_y = y_left;
							}

							else if (v1->e[EDGE_SW]->inter_sign == 0) {
								v1->e[EDGE_SW]->inter_x = NOT_SET;
								v1->e[EDGE_SW]->inter_y = NOT_SET;
							}

							current_x = x1;
							current_y = y1;
							iface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
							x[(i+1)%npts] -= 2 * CMP_EPSILON;
							y[(i+1)%npts] += 2 * CMP_EPSILON;
							i--;
							continue;

						}
						// The initial point lies in the | edge of a |/ face.
						else if (x1_eq_x_left) {
							current_edge = v1->e[EDGE_N];
							current_x = x1;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the _ edge of a /| face.
						else if (y1_eq_y_left) {
							current_edge = v1->e[EDGE_E];
							current_x = x_left;
							current_y = y1;
							iface = NULL;
						}


						// The initial point lies in the / edge
						else if (double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) {
							current_edge = v1->e[EDGE_NE];
							current_x = x_left;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the |/ face 
						// but not in the | edge nor the / edge of its.
						else if (y_left > m*x_left+(y1-m*x1)) {
							t1 = (y1+hy-y_left)/p;
							current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E]; 
							current_x = x_left + t1;
							current_y = v1->e[EDGE_N]->v[1]->y;
							iface = current_edge->f[FACE_DOWN];
						}


						// The initial point lies in the /| face
						else { // if (y_left < m*x_left+(y1-m*x1)) 
							t1 = x1+hx-x_left;
							t2 = ((x1-x_left)-(y1-y_left)/m)/(1-p/m);

							if (double_eq(t1, t2, CMP_EPSILON)) {
								current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];
								current_x = x_left + t1;
								current_y = y_left + t1*p;
								iface = current_edge->f[FACE_LEFT];

							}

							else if (t1 < t2) {
								current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];
								current_x = x_left + t1;
								current_y = y_left + t1*p;
								iface = current_edge->f[FACE_LEFT];
							}

							else {//  if (t1 > t2)
								current_edge = v1->e[EDGE_NE];
								current_x = x_left + t2;
								current_y = y_left + t2*p;
								iface = current_edge->f[FACE_DOWN];
							}
						}

						// We now determine the final edge to process
						if (x2_eq_x_right && y2_eq_y_right) { 
							/*
							final_edge = v2->e[EDGE_E];

							// Mark the NE edge of v2
							v2->e[EDGE_NE]->inter_sign++;
							if (v2->e[EDGE_NE]->inter_x == NOT_SET ||
							   v2->e[EDGE_NE]->inter_sign > 0) {

								v2->e[EDGE_NE]->inter_x = x_right;
								v2->e[EDGE_NE]->inter_y = y_right;
							}

							else if (v2->e[EDGE_NE]->inter_sign == 0) {
								v2->e[EDGE_NE]->inter_x = NOT_SET;
								v2->e[EDGE_NE]->inter_y = NOT_SET;
							}

							// else if (v2->e[EDGE_NE]->inter_sign < 0) do nada

							fface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[i], y[i]);
							x[i] -= 2 * CMP_EPSILON;
							y[i] += 2 * CMP_EPSILON;
							i--;
							continue;
						}

						// The final point lies in the | edge of a /| face
						else if (x2_eq_x_right)  {
							final_edge = v2->e[EDGE_S];
							fface = NULL;
						}

						// The final point lies in the _ edge of a |/ face
						else if (y2_eq_y_right) {
							final_edge = v2->e[EDGE_W];
							fface = NULL;
						}

						// The final point  lies in the / edge
						else if (double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) {
							final_edge = v2->e[EDGE_SW];
							fface = NULL;
						}

						// The final point lies in the |/ face but not in the / edge
						// or the _ edge
						else if (y_right > m*x_right+(y2-m*x2)) {
							t1 = ((x2-x_right)-(y2-y_right)/m)/(1-p/m);
							t2 = x2-hx-x_right;

							if (double_eq(t1, t2, CMP_EPSILON)) {
								final_edge = v2->e[EDGE_SW];
								fface = final_edge->f[FACE_UP]; 
							}

							else if (fabs(t1) < fabs(t2)) {
								final_edge = v2->e[EDGE_SW];
								fface = final_edge->f[FACE_UP]; 
							}

							else  { // if (t1 > t2)
								final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
								fface = final_edge->f[FACE_RIGHT];
							}
						}


						// The final point lies in the /| face  but not in the / edge
						// nor the | edge
						else  { // if (y_right < m*x_right+(y2-m*x2)) 
							final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
							fface = final_edge->f[FACE_UP];
						}

						if (iface != NULL && fface != NULL && iface == fface) continue;

						next_edge = current_edge;

						//print_edge(final_edge, "fe");
						while(1) {
							current_edge = next_edge;
							//print_edge(current_edge, "ce");

							if (current_edge->type == EDGE_V) {
								current_edge->inter_sign--;
								to_queue = current_edge->v[1];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign < 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[0]->is_queued &&
											!is_still_valid(current_edge->v[0])) {
										queue_remove(q, current_edge->v[0]);
										current_edge->v[0]->is_queued = 0;
									}

								}

								// else if (current_edge->inter_sign > 0) do nada

								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->y - current_y)/p;

								current_x += t1;
								current_y = current_edge->v[1]->y;

								next_edge = current_edge->v[1]->e[EDGE_E];
							}

							else if (current_edge->type == EDGE_D) {
								current_edge->inter_sign++;
								to_queue = current_edge->v[0];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign > 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
										//iprintf("queued (%f, %f)", to_queue->x, to_queue->y);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[1]->is_queued &&
									   !is_still_valid(current_edge->v[1])) {
										queue_remove(q, current_edge->v[1]);
										current_edge->v[1]->is_queued = 0;
									}

								}

								// else if (current_edge->inter_sign < 0) 

								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->y - current_y)/p;

								current_x += t1;
								current_y = current_edge->v[1]->y;

								next_edge = current_edge->v[1]->e[EDGE_W];
							}

							else { // current_edge->type == EDGE_H
								current_edge->inter_sign++;
								to_queue = current_edge->v[0];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign > 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
										//iprintf("queued (%f, %f)", to_queue->x, to_queue->y);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[1]->is_queued &&
									   !is_still_valid(current_edge->v[1])) {
										queue_remove(q, current_edge->v[1]);
										current_edge->v[1]->is_queued = 0;
									}

								}

								// else if (current_edge->inter_sign < 0) 

								if (current_edge == final_edge) break;

								t1 = current_edge->v[1]->x - current_x;
								t2 = current_edge->v[0]->x - current_x;
								t2 -= (current_edge->v[0]->y - current_y)/m;
								t2 /= 1-p/m;

								if (double_eq(t1, 0.0, CMP_EPSILON)) {
									next_edge = current_edge->v[1]->e[EDGE_N];
								}

								else if (double_eq(t1,t2, CMP_EPSILON)) {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[1]->e[EDGE_N];

								}
								else if (t1 < t2) {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[1]->e[EDGE_N];
								}

								else if (t1 > t2) {
									current_x += t2;
									current_y += t2*p;
									next_edge = current_edge->v[0]->e[EDGE_NE];
								}

							}
							
							//Extra check to prevent segfault
							//Further programming may be needed
							if (
								   next_edge->v[0]->x > x2 + hx
								|| next_edge->v[0]->y > y2 + hy
							) {
								iprint_phase_1_segfault(p, swapped,
										x_left, y_left, 
										x_right, y_right,
										x1, y1,
										x2, y2,
										hx, hy,
										nx, ny
								);
								break;
							}
						}
					}

					else { // if (0<p<m)
						if (x1_eq_x_left && y1_eq_y_left)  {
							/*
							current_edge = v1->e[EDGE_S];

							// Mark the SW edge of v1
							v1->e[EDGE_SW]->inter_sign--;
							if (v1->e[EDGE_SW]->inter_x == NOT_SET ||
							   v1->e[EDGE_SW]->inter_sign < 0) {

								v1->e[EDGE_SW]->inter_x = x_left;
								v1->e[EDGE_SW]->inter_y = y_left;
							} 

							else if (v1->e[EDGE_SW]->inter_sign == 0) {
								v1->e[EDGE_SW]->inter_x = NOT_SET;
								v1->e[EDGE_SW]->inter_y = NOT_SET;
							}

							// else if (v1->e[EDGE_SW]->inter_sign > 0) 

							current_x = x1;
							current_y = y1;
							iface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
							x[(i+1)%npts] -= 2 * CMP_EPSILON;
							y[(i+1)%npts] += 2 * CMP_EPSILON;
							i--;
							continue;

						}

						// The initial point lies in the | edge of a |/ face.
						else if (x1_eq_x_left) {
							current_edge = v1->e[EDGE_N];
							current_x = x1;
							current_y = y_left;
							iface = NULL;
						}

						// The initial point lies in the _ edge of a /| face.
						else if (y1_eq_y_left) {
							current_edge = v1->e[EDGE_E];
							current_x = x_left;
							current_y = y1;
							iface = NULL;
						}

						// The initial point lies in the / edge
						else if (double_eq(y_left, m*x_left+(y1-m*x1), CMP_EPSILON)) {
							current_edge = v1->e[EDGE_NE];
							current_x = x_left;
							current_y = y_left;
							iface = NULL;
						}


						// The initial point lies in the |/ face 
						// but not in the | edge nor the / edge of its.
						else if (y_left > m*x_left+(y1-m*x1)) {
							t1 = ((x1-x_left)-(y1-y_left)/m)/(1-p/m);
							t2 = (y1+hy-y_left)/p;

							if (double_eq(t1, t2, CMP_EPSILON)) {
								current_edge = v1->e[EDGE_NE]; 
								current_x = x_left + t1;
								current_y = y_left + t1*p;
								iface = current_edge->f[FACE_LEFT];
							}

							else if (t1 < t2) {
								current_edge = v1->e[EDGE_NE]; 
								current_x = x_left + t1;
								current_y = y_left + t1*p;
								iface = current_edge->f[FACE_LEFT];
							}

							else { // if (t1 > t2) 
								current_edge = v1->e[EDGE_N]->v[1]->e[EDGE_E];
								current_x = x_left + t2;
								current_y = y_left + t2*p;
								iface = current_edge->f[FACE_DOWN];
							}
						}


						// The initial point lies in the /| face
						else { // if (y_left < m*x_left+(y1-m*x1)) 
							t1 = x1+hx-x_left;
							current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_N];
							current_x = x_left + t1;
							current_y = y_left + t1*p;
							iface = current_edge->f[FACE_LEFT];
						}


						// We now determine the final edge to process
						if (x2_eq_x_right && y2_eq_y_right) {
							/*
							final_edge = v2->e[EDGE_E];

							// Mark the NE edge of v2
							v2->e[EDGE_NE]->inter_sign++;
							if (v2->e[EDGE_NE]->inter_x == NOT_SET ||
							   v2->e[EDGE_NE]->inter_sign > 0) {

								v2->e[EDGE_NE]->inter_x = x_right;
								v2->e[EDGE_NE]->inter_y = y_right;
							}

							else if (v2->e[EDGE_NE]->inter_sign == 0) {
								v2->e[EDGE_NE]->inter_x = NOT_SET;
								v2->e[EDGE_NE]->inter_y = NOT_SET;
							}

							// else if (v2->[EDGE_NE]->inter_sign < 0) 

							fface = NULL;
							*/

							iprintf("Corrected (%f, %f)", x[i], y[i]);
							x[i] -= 2 * CMP_EPSILON;
							y[i] += 2 * CMP_EPSILON;
							i--;
							continue;
						}


						// The final point lies in the | edge of a /| face
						else if (x2_eq_x_right) {
							final_edge = v2->e[EDGE_S];
							fface = NULL;
						}

						// The final point lies in the _ edge of a |/ face
						else if (y2_eq_y_right) {
							final_edge = v2->e[EDGE_W];
							fface = NULL;
						}

						// The final point  lies in the / edge
						else if (double_eq(y_right, m*x_right+(y2-m*x2), CMP_EPSILON)) {
							final_edge = v2->e[EDGE_SW];
							fface = NULL;
						}


						// The final point lies in the |/ face but not in the / edge
						else if (y_right > m*x_right+(y2-m*x2)) {
							t1 = x2-hx-x_right;
							final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_S];
							fface = final_edge->f[FACE_RIGHT];
						}


						// The final point lies in the /| face  but not in the / edge
						// nor the | edge
						else { // if (y_right < m*x_right+(y2-m*x2)) 
							t1 = -(y2-hy-y_right)/p;
							t2 = -((x2-x_right)-(y2-y_right)/m)/(1-p/m);

							if (double_eq(t1, t2, CMP_EPSILON)) {
								final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
								fface = final_edge->f[FACE_UP];
							}

							else if (t1 < t2) { 
								final_edge = v2->e[EDGE_S]->v[0]->e[EDGE_W];
								fface = final_edge->f[FACE_UP];
							}

							else  {
								final_edge = v2->e[EDGE_SW];
								fface = final_edge->f[FACE_RIGHT];
							}
						}

						if (iface != NULL && fface != NULL && iface == fface) continue;

						next_edge = current_edge;

						//print_edge(final_edge, "fe");
						while(1) {
							current_edge = next_edge;
							//print_edge(current_edge, "ce");

							if (current_edge->type == EDGE_H) {
								current_edge->inter_sign++;
								to_queue = current_edge->v[0];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign > 0) {

									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[1]->is_queued &&
											!is_still_valid(current_edge->v[1])) {
										queue_remove(q, current_edge->v[1]);
										current_edge->v[1]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign < 0) 

								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->x - current_x);

								current_x = current_edge->v[1]->x;
								current_y += t1*p;

								next_edge = current_edge->v[1]->e[EDGE_N];
							}

							else if (current_edge->type == EDGE_D) {
								current_edge->inter_sign--;
								to_queue = current_edge->v[1];

								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign < 0) {

									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {

									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[0]->is_queued &&
											!is_still_valid(current_edge->v[0])) {
										queue_remove(q, current_edge->v[0]);
										current_edge->v[0]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign > 0) do nada  

								if (current_edge == final_edge) break;

								t1 = (current_edge->v[1]->x - current_x);
								current_x = current_edge->v[1]->x;
								current_y += t1 *p;
								next_edge = current_edge->v[1]->e[EDGE_S];
							}

							else { // current_edge->type == EDGE_V
								current_edge->inter_sign--;
								to_queue = current_edge->v[1];


								if (current_edge->inter_x == NOT_SET ||
								   current_edge->inter_sign < 0) {
									current_edge->inter_x = current_x;
									current_edge->inter_y = current_y;

									if (to_queue->is_queued == 0) {
										to_queue->is_queued = 1;
										queue_push(q, to_queue);
									}
								}

								else if (current_edge->inter_sign == 0) {
									current_edge->inter_x = NOT_SET;
									current_edge->inter_y = NOT_SET;

									if (current_edge->v[0]->is_queued &&
											!is_still_valid(current_edge->v[0])) {
										queue_remove(q, current_edge->v[0]);
										current_edge->v[0]->is_queued = 0;
									}
								}

								// else if (current_edge->inter_sign > 0) do nada 

								if (current_edge == final_edge) break;

								t1 = current_edge->v[0]->x - current_x;
							t1 -= (current_edge->v[0]->y - current_y)/m;
								t1 /= 1-p/m;
								t2 = (current_edge->v[1]->y - current_y)/p;

								if (double_eq(t1, 0.0, CMP_EPSILON)) 
									next_edge = current_edge->v[0]->e[EDGE_NE];

								else if (double_eq(t2, 0.0, CMP_EPSILON)) {
									next_edge = current_edge->v[1]->e[EDGE_E];
								}

								else if (double_eq(t1,t2, CMP_EPSILON)) {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[0]->e[EDGE_NE];

								}

								else if (t1 < t2) {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[0]->e[EDGE_NE];
								}

								else if (t1 > t2)  {
									current_x += t2;
									current_y += t2*p;
									next_edge = current_edge->v[1]->e[EDGE_E];
								}

								else  {
									current_x += t1;
									current_y += t1*p;
									next_edge = current_edge->v[1]->e[EDGE_E];
								}
							}
							
							//Extra check to prevent segfault
							//Further programming may be needed
							if (
								   next_edge->v[0]->x > x2 + hx
								|| next_edge->v[0]->y > y2 + hy
							) {
								iprint_phase_1_segfault(p, swapped,
										x_left, y_left, 
										x_right, y_right,
										x1, y1,
										x2, y2,
										hx, hy,
										nx, ny
								);
								break;
							}
						}
					} 
				}

				else { // if (p < 0) 
					// (x1, y1) and (x2, y2) are setted in such way that the segment they define is
					// the smallest rectangle in the grid that contains the segment 
					// (x_left, y_left)-(x_right, y_right).
					// i1, j1, i2, j2 are they respective indexes in the array containing the vertices 


					v1 = get_init_vertex(grid, x_left, y_left, -1);
					v2 = get_end_vertex(grid, x_right, y_right, -1);

					x1 = v1->x;
					y1 = v1->y;

					x2 = v2->x;
					y2 = v2->y;

					x1_eq_x_left = double_eq(x1, x_left, CMP_EPSILON);
					y1_eq_y_left = double_eq(y1, y_left, CMP_EPSILON);
					x2_eq_x_right = double_eq(x2, x_right, CMP_EPSILON);
					y2_eq_y_right = double_eq(y2, y_right, CMP_EPSILON);

					//iprintf("x_left = %f, x1 = %f , x_left == x1 ? %d", x_left, x1, x1_eq_x_left);
					//iprintf("y_left = %f, y1 = %f , y_left == y1 ? %d", y_left, y1, y1_eq_y_left);
					//iprintf("x_right = %f, x2 = %f , x_right == x2 ? %d", x_right, x2, x2_eq_x_right);
					//iprintf("y_right = %f, y2 = %f , y_right == y2 ? %d", y_right, y2, y2_eq_y_right);

					// The initial point is the grid point (x1, y1)
					if (x1_eq_x_left && y1_eq_y_left) {
						/*
						current_edge = v1->e[EDGE_W];

						current_x = x1;
						current_y = y1;
						iface = NULL;
						*/

						iprintf("Corrected (%f, %f)", x[(i+1)%npts], y[(i+1)%npts]);
						x[(i+1)%npts] -= 2 * CMP_EPSILON;
						y[(i+1)%npts] += 2 * CMP_EPSILON;
						i--;
						continue;
					}

					// The initial point lies in the | edge of a |/ face.
					else if (x1_eq_x_left) {
						current_edge = v1->e[EDGE_S];
						current_x = x1;
						current_y = y_left;
						iface = NULL;
					}

					// The initial point lies in the _ edge of a |/ face.
					else if (y1_eq_y_left) {
						current_edge = v1->e[EDGE_E];
						current_x = x_left;
						current_y = y1;
						iface = NULL;
					}

					// The initial point lies in the / edge
					else if (double_eq(y_left, m*x_left+(y1-hy-m*x1), CMP_EPSILON)) {
						current_edge = v1->e[EDGE_S]->v[0]->e[EDGE_NE];
						current_x = x_left;
						current_y = y_left;

						iface = NULL;
					}

					// The initial point lies in the |/ face 
					// but not in the | edge nor the / edge of its.
					else if (y_left > m*x_left+(y1-hy-m*x1)) {
						current_edge = v1->e[EDGE_S]->v[0]->e[EDGE_NE]; 

						t1 = current_edge->v[0]->x - x_left;
						t1 -= (current_edge->v[0]->y - y_left)/m;
						t1 /= 1-p/m;

						current_x = x_left + t1;
						current_y = y_left + t1*p;

						iface = current_edge->f[FACE_UP];
					}

					// The initial point lies in the /| face
					else { // if (y_left < m*x_left+(y1-m*x1)) 
						t1 = (y1-hy-y_left)/p;
						t2 = x1+hx-x_left;

						if (double_eq(t1, t2, CMP_EPSILON)) {
							current_edge = v1->e[EDGE_S]->v[0]->e[EDGE_E];
							current_x = x_left + t1;;
							current_y = current_edge->v[0]->y;
							iface = current_edge->f[FACE_UP];
						}

						else if (t1 < t2) {
							current_edge = v1->e[EDGE_S]->v[0]->e[EDGE_E];
							current_x = x_left + t1;;
							current_y = current_edge->v[0]->y;
							iface = current_edge->f[FACE_UP];
						}

						else {//  if (t1 > t2)
							current_edge = v1->e[EDGE_E]->v[1]->e[EDGE_S];
							current_x = current_edge->v[0]->x;
							current_y = y_left + t2*p;
							iface = current_edge->f[FACE_LEFT];
						}
					}

					// We now determine the final edge to process
					if (x2_eq_x_right && y2_eq_y_right){ 
						/*
						final_edge = v2->e[EDGE_S];
						fface = NULL;
						*/

						iprintf("Corrected (%f, %f)", x[i], y[i]);
						x[i] -= 2 * CMP_EPSILON;
						y[i] += 2 * CMP_EPSILON;
						i--;
						continue;
					}


					// The final point lies in the | edge of a /| face
					else if (x2_eq_x_right) {
						final_edge = v2->e[EDGE_N];
						fface = NULL;
					}

					// The final point lies in the _ edge of a /| face
					else if (y2_eq_y_right) {
						final_edge = v2->e[EDGE_W];
						fface = NULL;
					}

					// The final point  lies in the / edge
					else if (double_eq(y_right,m*x_right+(y2-m*(x2-hx)), CMP_EPSILON)) {
						final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_NE];
						fface = NULL;
					}

					// The final point lies in the |/ face but not in the / edge
					else if (y_right > m*x_right+(y2-m*(x2-hx))) {
						t1 = (y2+hy-y_right)/p;
						t2 = x2-hx-x_right;


						if (double_eq(t1, t2, CMP_EPSILON)) {
							final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_N];
							fface = final_edge->f[FACE_RIGHT];
						}

						else if (fabs(t1) < fabs(t2)) {
							final_edge = v2->e[EDGE_N]->v[1]->e[EDGE_W];
							fface = final_edge->f[FACE_DOWN];
						}

						else  {// if (t1 > t2)
							final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_N];
							fface = final_edge->f[FACE_RIGHT];
						}
					}


					// The final point lies in the /| face  but not in the / edge
					// nor the | edge
					else { // if (y_right < m*x_right+(y2-m*x2)) 
						final_edge = v2->e[EDGE_W]->v[0]->e[EDGE_NE];
						fface = final_edge->f[FACE_DOWN];
					}

					if (iface != NULL && fface != NULL && iface == fface) continue;

					next_edge = current_edge;

					//print_edge(final_edge, "fe");
					while(1) {
						current_edge = next_edge;
						//print_edge(current_edge, "ce");

						if (current_edge->type == EDGE_V) {
							current_edge->inter_sign--;
							to_queue = current_edge->v[1];

							if (current_edge->inter_x == NOT_SET ||
							   current_edge->inter_sign < 0) {
								current_edge->inter_x = current_x;
								current_edge->inter_y = current_y;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
								}
							}

							else if (current_edge->inter_sign == 0) {
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[0]->is_queued &&
										!is_still_valid(current_edge->v[0])) {
									queue_remove(q, current_edge->v[0]);
									current_edge->v[0]->is_queued = 0;
								}

							}

							// else if (current_edge->inter_sign > 0) do nada 

							if (current_edge == final_edge) break;

							t1 = current_edge->v[0]->x - current_x;
							t1 -= (current_edge->v[0]->y - current_y)/m;
							t1 /= 1-p/m;

							current_x += t1;
							current_y += t1*p;

							next_edge = current_edge->v[0]->e[EDGE_NE];
						}

						else if (current_edge->type == EDGE_D) {
							current_edge->inter_sign--;
							to_queue = current_edge->v[1];

							if (current_edge->inter_x == NOT_SET ||
							   current_edge->inter_sign < 0) {
								current_edge->inter_x = current_x;
								current_edge->inter_y = current_y;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
									//iprintf("queued (%f, %f)", to_queue->x, to_queue->y);
								}
							}

							else if (current_edge->inter_sign == 0) { 
								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[0]->is_queued &&
										!is_still_valid(current_edge->v[0])) {
									queue_remove(q, current_edge->v[0]);
									current_edge->v[0]->is_queued = 0;
								}

							}

							// else if (current_edge->inter_sign > 0) 

							if (current_edge == final_edge) break;

							t1 = (current_edge->v[0]->y-current_y)/p;
							t2 = current_edge->v[1]->x-current_x;


							if (double_eq(t1, t2, CMP_EPSILON)) {
								current_x += t1;
								current_y += t1*p;
								next_edge = current_edge->v[0]->e[EDGE_E];
							}

							else if (double_eq(t1, 0.0, CMP_EPSILON)) {
								next_edge = current_edge->v[0]->e[EDGE_E];
							}

							else if (double_eq(t2, 0.0, CMP_EPSILON)) {
								next_edge = current_edge->v[1]->e[EDGE_S];
							}

							else if (t1 < t2) {
								current_x += t1;
								current_y = current_edge->v[0]->y;

								next_edge = current_edge->v[0]->e[EDGE_E];
							}

							else { // if (t1 > t2) 
								current_x = current_edge->v[1]->x;
								current_y += t2*p;

								next_edge = current_edge->v[1]->e[EDGE_S];
							}
						}


						else { // current_edge->type == EDGE_H
							current_edge->inter_sign--;
							to_queue = current_edge->v[1];


							if (current_edge->inter_x == NOT_SET ||
							   current_edge->inter_sign < 0) {
								current_edge->inter_x = current_x;
								current_edge->inter_y = current_y;

								if (to_queue->is_queued == 0) {
									to_queue->is_queued = 1;
									queue_push(q, to_queue);
									//iprintf("queued (%f, %f)", to_queue->x, to_queue->y);
								}
							}

							else if (current_edge->inter_sign == 0) {

								current_edge->inter_x = NOT_SET;
								current_edge->inter_y = NOT_SET;

								if (current_edge->v[0]->is_queued &&
								   !is_still_valid(current_edge->v[0])) {
									queue_remove(q, current_edge->v[0]);
									current_edge->v[0]->is_queued = 0;
								}

							}

							// else if (current_edge->inter_sign > 0) 

							if (current_edge == final_edge) break;

							t1 = current_edge->v[0]->x - current_x;
							t1 -= (current_edge->v[0]->e[EDGE_S]->v[0]->y - current_y)/m;
							t1 /= 1-p/m;

							current_x += t1;
							current_y += t1*p;

							next_edge = current_edge->v[1]->e[EDGE_SW];
						}
						
						//Extra check to prevent segfault
						//Further programming may be needed
						if (
							   next_edge->v[0]->x > x2 + hx
							|| next_edge->v[0]->y < y2 - hy
						) {
							iprint_phase_1_segfault(p, swapped,
									x_left, y_left, 
									x_right, y_right,
									x1, y1,
									x2, y2,
									hx, hy,
									nx, ny
							);
							break;
						}
					}
				}
			}
		}
	}
}

void iprint_phase_1_segfault(
		double p, int swapped,
		double x_left, double y_left, 
		double x_right, double y_right,
		double x1, double y1,
		double x2, double y2,
		double hx, double hy,
		IDL_LONG nx, IDL_LONG ny
) {
	iprintf("Segfault prevented on phase_1");
	iprintf("x_left = %f", x_left);
	iprintf("y_left = %f", y_left);
	iprintf("x_right = %f", x_right);
	iprintf("y_right = %f", y_right);
	iprintf("x1 = %f", x1);
	iprintf("y1 = %f", y1);
	iprintf("x2 = %f", x2);
	iprintf("y2 = %f", y2);
	iprintf("hx = %f", hx);
	iprintf("hy = %f", hy);
	iprintf("nx = %d", nx);
	iprintf("ny = %d", ny);
	iprintf("p  = %f", p);
	iprintf("sw = %d", swapped);
}
int phase_2(mesh grid, queue q, int limit) {
	vertex v;
	queue q_aux;
	int j,k;

	q_aux = new_queue();


	// Dequeue the vertices and requeue
	// if they are in the boundary
	//iprintf("Marking Queue Nodes");
	j = 0;

	// Discard any interior vertices and singletons
	k = 0;
	while(!is_empty(q)) {
		v = (vertex)queue_pop(q);
		if (is_interior(v)) {
			v->ind = 1;
			v->is_queued = 0;
			continue;
		}

		queue_push(q_aux, v);
		k++;
	}
	//iprintf("Aprox %d nodes were processed on phase_1", k);

	// The boundary vertices are the seed
	// to fill the interior of the curve
	//iprintf("Painting Interior");
	while(!is_empty(q_aux)) {
		v = (vertex)queue_pop(q_aux);
		fill_interior(grid, v, limit);
		if (is_interior(v))
			continue;

		else  {
			v->is_queued = 1;
			v->was_queued = 1;
			queue_push(q,v);
		}
	}

	destroy_queue(q_aux);

	return j;
}

int phase_2_out(mesh grid, queue q, int limit) {
	vertex v;
	queue q_aux;
	int j, k;

	q_aux = new_queue();


	// Dequeue the vertices and requeue
	// if they are in the boundary
	//iprintf("Marking Queue Nodes");
	j = 0;

	k = 0;
	// Discard any exterior vertices
	while(!is_empty(q)) {
		v = (vertex)queue_pop(q);
		if (is_exterior(v)){
			v->ind = 0;
			v->is_queued = 0;
			continue;
		}
		k++;
		queue_push(q_aux, v);
	}
	//iprintf("Aprox %d nodes were processed on phase_1", k);

	// The boundary vertices are the seed
	// to fill the exterior of the curve
	// iprintf("Painting Interior");
	while(!is_empty(q_aux)) {
		v = (vertex)queue_pop(q_aux);
		fill_exterior(grid, v, limit);
		if (is_exterior(v))
			continue;

		else {
			v->is_queued = 1;
			v->was_queued = 1;
			queue_push(q,v);
		}
	}
	
	destroy_queue(q_aux);

	return j;
}

void collect_snakes(mesh grid, queue q, sbox sb) {
	int nsnakes, i, edge_dir, skip, caution_mode, there_was_a_snake_colapsed;
	vertex v, first_v, other_v;
	edge e, first_e;
	snake curr_snake;
	queue qx, qy;
	double *curr_x, *curr_y;
	double prev_x, prev_y;

	IDL_LONG npts;

	nsnakes = 0;

	qx = new_queue();
	qy = new_queue();

	// First we discard the edges whose both extremes are interior
	for(i = 0; i < grid->nedges; i++) {
		e = &(grid->elist[i]); 
		if (e->v[0]->ind == 1 && e->v[1]->ind == 1 && e->inter_x != NOT_SET)  {
			e->inter_x = NOT_SET;
			e->inter_y = NOT_SET;
		}
		
	}

	// Proccess every queued vertex
	while(!is_empty(q)) {
		there_was_a_snake_colapsed = 0;
		first_v = (vertex)queue_pop(q);
		
		// it was already processed because it would
		// lead to a duplicate snake
		if (first_v->is_queued == 0)
			continue;

		// Mark the vertex as already processed
		first_v->is_queued = 0;

		// Search an edge adyacent to the vertex
		// such that has an intersection
		skip = 1; 
		for(i = 0; i<6; i++) {
			other_v = other_side(first_v, i);

			if (other_v == NULL) continue;

			if (
					first_v->e[i]->inter_x != NOT_SET &&
					other_v->ind == 0
			  ) {
				first_e = first_v->e[i];
				edge_dir = i;
				skip = 0;
				break;
			}
		}

		// If skip == 1 then the vertex did not have 
		// an intersection edge. We discard it and process th
		// next vertex queued
		if (skip == 1) continue;


		// Caution mode is to manage interior vertex that fall
		// outside the evoluted snake
		caution_mode = 0;

		// Initializing npts (snake points), the vertex and edge variable
		// iterators and the last point variables
		npts = 0;
		v = first_v;
		e = first_e;

		prev_x = e->inter_x;
		prev_y = e->inter_y;

		// We begin to enqueue the boundary points that will define
		// the new snake. The loop continues until we return 
		// to first_v and first_e
		do {
			// Add only valid points (maybe this check is now innecesary?)
			if (e->inter_x != NOT_SET && e->inter_y != NOT_SET) {
			   // Add the point if it is not the same as the previous
			   if (!double_eq(e->inter_x, prev_x, CMP_EPSILON) || !double_eq(e->inter_y, prev_y, CMP_EPSILON)) {
					npts++;
					queue_push(qx, &(e->inter_x));
					queue_push(qy, &(e->inter_y));
					prev_x = e->inter_x;
					prev_y = e->inter_y;
			   }

			}

			edge_dir = edge_dir - 1 < 0 ? 5 : edge_dir-1;
			other_v = other_side(v, edge_dir);

			// In caution mode off we trace the contour guided by
			// the edges that are intersected
			if (caution_mode == 0) {

				// If the edge has an intersection we set e to it
				// so the next iteration saves it intersection to the
				// queue
				if (v->e[edge_dir]->inter_x != NOT_SET) 
					e = v->e[edge_dir];

				// If the next edge does not have an intersection
				// we pass to the vertex on the other side of the
				// edge. We have to make sure to update e also.
				else {
					v = other_v;
					v->is_queued = 0;

					edge_dir = next_edge_direction_on_vertex_change(edge_dir);
					e = v->e[edge_dir];

					if (v->was_queued == 0) {
						caution_mode = 1;

						if (e->inter_x == NOT_SET) {
							e->inter_x = (e->v[0]->x + e->v[1]->x)/2;
							e->inter_y = (e->v[0]->y + e->v[1]->y)/2;
						}
					}
				}
			}

			// In caution mode on we trace the contour guided by the
			// interior nodes. If no intersection are found in a determined
			// edge then we set it arbitrarily to the mid point of such edge
			else { // if caution_mode == 1	
				if (other_v->ind == 0) {
					e = v->e[edge_dir];
					if (e->inter_x == NOT_SET) {
						e->inter_x = (e->v[0]->x + e->v[1]->x)/2;
						e->inter_y = (e->v[0]->y + e->v[1]->y)/2;
					}

				}

				else { // if other_v->ind == 1
					v = other_v;
					v->is_queued = 0;

					edge_dir = next_edge_direction_on_vertex_change(edge_dir);
					e = v->e[edge_dir];

					if (other_v->was_queued == 1) {
						caution_mode = 0;
					}

				}

			}
		} while(v != first_v || e != first_e);

		if (npts < 12) {
			there_was_a_snake_colapsed = 1;

			while(!is_empty(qx) || !is_empty(qy)) {
				queue_pop(qx);
				queue_pop(qy);
			}
		}

		else {
			// Create the new snake and add it to the box
			//iprintf("Creating new snake of %ld points", npts);
			i = 0;
			curr_snake = new_snake(npts);
			while(!is_empty(qx) || !is_empty(qy)) {
				curr_x = (double *)queue_pop(qx);
				curr_y = (double *)queue_pop(qy);

				// Passing by value!
				curr_snake->x[i] = *curr_x;
				curr_snake->y[i] = *curr_y;
				i++;

			}
			push_snake(sb, curr_snake);
		}
	}
}

void collect_snakes_out(mesh grid, queue q, queue q_in, queue q_out, sbox sb) {
	int nsnakes, i, edge_dir, skip, caution_mode, there_was_a_snake_colapsed;
	vertex v, first_v, other_v;
	edge e, first_e;
	snake curr_snake;
	queue qx, qy;
	double *curr_x, *curr_y;
	double prev_x, prev_y;
	IDL_LONG npts;

	nsnakes = 0;

	qx = new_queue();
	qy = new_queue();

	// First we discard the edges whose both extremes are exterior
	for(i = 0; i < grid->nedges; i++) {
		e = &(grid->elist[i]); 
		if (e->v[0]->ind == 0 && e->v[1]->ind == 0 && e->inter_x != NOT_SET)  {
			e->inter_x = NOT_SET;
			e->inter_y = NOT_SET;
		}
		
	}

	// Proccess every queued vertex
	while(!is_empty(q)) {
		there_was_a_snake_colapsed = 0;
		first_v = (vertex)queue_pop(q);
		
		// it was already processed because it would
		// lead to a duplicate snake
		if (first_v->is_queued == 0)
			continue;

		// Mark the vertex as already processed
		first_v->is_queued = 0;

		// Search an edge adyacent to the vertex
		// such that has an intersection
		skip = 1; 
		for(i = 0; i<6; i++) {
			other_v = other_side(first_v, i);

			if (other_v == NULL) continue;

			if (
					first_v->e[i]->inter_x != NOT_SET &&
					other_v->ind == 1
			  ) {
				first_e = first_v->e[i];
				edge_dir = i;
				skip = 0;
				break;
			}
		}

		// If skip == 1 then the vertex did not have 
		// an intersection edge. We discard it and process th
		// next vertex queued
		if (skip == 1) continue;


		// Caution mode is to manage interior vertex that fall
		// outside the evoluted snake
		caution_mode = 0;

		// Initializing npts (snake points), the vertex and edge variable
		// iterators and the last point variables
		npts = 0;
		v = first_v;
		e = first_e;

		prev_x = e->inter_x;
		prev_y = e->inter_y;

		// We begin to enqueue the boundary points that will define
		// the new snake. The loop continues until we return 
		// to first_v and first_e
		do {
			if (e->inter_x != NOT_SET && e->inter_y != NOT_SET) {
			   if (!double_eq(e->inter_x, prev_x, CMP_EPSILON) || !double_eq(e->inter_y, prev_y, CMP_EPSILON)) {
					npts++;
					queue_push(qx, &(e->inter_x));
					queue_push(qy, &(e->inter_y));
					prev_x = e->inter_x;
					prev_y = e->inter_y;
			   }

			}

			edge_dir = (edge_dir + 1) % 6;
			other_v = other_side(v, edge_dir);

			// In caution mode off we trace the contour guided by
			// the edges that are intersected
			//
			if (caution_mode == 0) {
				// If the edge has an intersection we set e to it
				// so the next iteration saves it intersection to the
				// queue
				if (v->e[edge_dir]->inter_x != NOT_SET) 
					e = v->e[edge_dir];

				// If the next edge does not have an intersection
				// we pass to the vertex on the other side of the
				// edge. We have to make sure to update e also.
				else {
					v = other_v;
					v->is_queued = 0;

					edge_dir = next_edge_direction_on_vertex_change_out(edge_dir);
					e = v->e[edge_dir];

					if (v->was_queued == 0) {
						caution_mode = 1;

						if (e->inter_x == NOT_SET) {
							e->inter_x = (e->v[0]->x + e->v[1]->x)/2;
							e->inter_y = (e->v[0]->y + e->v[1]->y)/2;
						}
					}
				}
			}

			// In caution mode on we trace the contour guided by the
			// interior nodes. If no intersection are found in a determined
			// edge then we set it arbitrarily to the mid point of such edge
			else { // if caution_mode == 1	
				if (other_v->ind == 1) {
					e = v->e[edge_dir];
					if (e->inter_x == NOT_SET) {
						e->inter_x = (e->v[0]->x + e->v[1]->x)/2;
						e->inter_y = (e->v[0]->y + e->v[1]->y)/2;
					}

				}

				else { // if other_v->ind == 0
					v = other_v;
					v->is_queued = 0;


					edge_dir = next_edge_direction_on_vertex_change_out(edge_dir);
					e = v->e[edge_dir];

					if (other_v->was_queued == 1) {
						caution_mode = 0;
					}
				}

			}
		} while(v != first_v || e != first_e);

		// Create the new snake and add it to the box
		//iprintf("Creating new snake of %ld points", npts);
		

		// If the snake is too small is ignored because soon
		// it will colapse 
		if (npts < 12) {
			there_was_a_snake_colapsed = 1;

			while(!is_empty(qx) || !is_empty(qy)) {
				queue_pop(qx);
				queue_pop(qy);
			}
		}

		else {
			i = 0;
			curr_snake = new_snake(npts);
			while(!is_empty(qx) || !is_empty(qy)) {
				curr_x = (double *)queue_pop(qx);
				curr_y = (double *)queue_pop(qy);

				// Passing by value!
				curr_snake->x[i] = *curr_x;
				curr_snake->y[i] = *curr_y;
				i++;
			}

			push_snake(sb, curr_snake);
		}

		//iprint_indicator(grid);
		//iprint_indicator_full(grid);
	}
}

int next_edge_direction_on_vertex_change(int edge_dir) {
	switch(edge_dir)  {
		case EDGE_E:
			return EDGE_SW;

		case EDGE_NE:
			return EDGE_S;

		case EDGE_N:
			return EDGE_E;

		case EDGE_W:
			return EDGE_NE;

		case EDGE_SW:
			return EDGE_N;

		case EDGE_S:
			return EDGE_W;
	}

	return -1;
}

int next_edge_direction_on_vertex_change_out(int edge_dir) {
	switch(edge_dir)  {
		case EDGE_E:
			return EDGE_N;

		case EDGE_NE:
			return EDGE_W;

		case EDGE_N:
			return EDGE_SW;

		case EDGE_W:
			return EDGE_S;

		case EDGE_SW:
			return EDGE_E;

		case EDGE_S:
			return EDGE_NE;
	}

	return -1;
}

void clean_intersections(mesh grid) {
	IDL_LONG i;

	dprintf("nedges = %ld", grid->nedges);
	for(i = 0; i < grid->nedges; i++) {

		grid->elist[i].inter_x = NOT_SET;
		grid->elist[i].inter_y = NOT_SET;
		grid->elist[i].inter_sign = 0;
	}
}

void clean_process_status(mesh grid) {
	IDL_LONG i;
	vertex v;

	for(i = 0; i < grid->nvertices; i++) {
		v = &(grid->vlist[i]);
		v->was_queued = 0;
	}
}

void clean_indicator(mesh grid) {
	IDL_LONG i;
	vertex v;

	for(i = 0; i < grid->nvertices; i++) {
		v = &(grid->vlist[i]);
		v->ind = 0;
	}
}

void reinitialize(mesh grid, queue q) {
	vertex v;
	clean_indicator(grid);
	clean_intersections(grid);
	clean_process_status(grid);

	while(!is_empty(q)) {
		v = (vertex)queue_pop(q);
		v->is_queued = 0;
	}

}

void fill_interior(mesh grid, vertex v, int limit) {
	int i, count;
	vertex cv;
	queue q;

	count = limit;
	q = new_queue();
	queue_push(q, v);

	if (count < 0) {
			while(!is_empty(q)) {
				cv = (vertex)queue_pop(q);

				cv->ind = 1;

				for(i = 0; i < 3; i++) {
					if (cv->e[i] == NULL) continue;

					if (cv->e[i]->v[0]->ind == 0 && cv->e[i]->inter_x == NOT_SET) {
						cv->e[i]->v[0]->ind = 1;
						queue_push(q,cv->e[i]->v[0]);
					}
				}

				for(i = 3; i < 6; i++) {
					if (cv->e[i] == NULL) continue;

					if (cv->e[i]->v[1]->ind == 0 && cv->e[i]->inter_x == NOT_SET) {
						cv->e[i]->v[1]->ind = 1;
						queue_push(q, cv->e[i]->v[1]);
					}
				}
			}
	}


	else {
		while(!is_empty(q) && count--) {
			cv = (vertex)queue_pop(q);

			cv->ind = 1;

			for(i = 0; i < 3; i++) {
				if (cv->e[i] == NULL) continue;

				if (cv->e[i]->v[0]->ind == 0 && cv->e[i]->inter_x == NOT_SET) {
					cv->e[i]->v[0]->ind = 1;
					queue_push(q,cv->e[i]->v[0]);
				}
			}

			for(i = 3; i < 6; i++) {
				if (cv->e[i] == NULL) continue;

				if (cv->e[i]->v[1]->ind == 0 && cv->e[i]->inter_x == NOT_SET) {
					cv->e[i]->v[1]->ind = 1;
					queue_push(q, cv->e[i]->v[1]);
				}
			}
		}
	}
	destroy_queue(q);
}

void fill_exterior(mesh grid, vertex v, int limit) {
	int i, count;
	vertex cv;
	queue q;

	count = limit;
	q = new_queue();
	queue_push(q, v);

	if (count < 0) {
		while (!is_empty(q)) {
			cv = (vertex)queue_pop(q);
			cv->ind = 0;

			for(i = 0; i < 3; i++) {
				if (cv->e[i] == NULL) continue;

				if (cv->e[i]->v[0]->ind == 1 && cv->e[i]->inter_x == NOT_SET) {
					cv->e[i]->v[0]->ind = 0;
					queue_push(q,cv->e[i]->v[0]);
				}
			}

			for(i = 3; i < 6; i++) {
				if (cv->e[i] == NULL) continue;

				if (cv->e[i]->v[1]->ind == 1 && cv->e[i]->inter_x == NOT_SET) {
					cv->e[i]->v[1]->ind = 0;
					queue_push(q, cv->e[i]->v[1]);
				}
			}
		}
	}

	else {
		while (!is_empty(q) && count--) {
			cv = (vertex)queue_pop(q);

			cv->ind = 0;


			for(i = 0; i < 3; i++) {
				if (cv->e[i] == NULL) continue;

				if (cv->e[i]->v[0]->ind == 1 && cv->e[i]->inter_x == NOT_SET) {
					cv->e[i]->v[0]->ind = 0;
					queue_push(q,cv->e[i]->v[0]);
				}
			}

			for(i = 3; i < 6; i++) {
				if (cv->e[i] == NULL) continue;

				if (cv->e[i]->v[1]->ind == 1 && cv->e[i]->inter_x == NOT_SET) {
					cv->e[i]->v[1]->ind = 0;
					queue_push(q, cv->e[i]->v[1]);
				}
			}
		}
	}

	destroy_queue(q);
}

void discard_local_self_intersections(sbox sb) {
	sbox sb_aux;
	snake sn, new_s;
	double num_t, t,  num_s, s, den, x1, y1, x2, y2, x3, y3, x4, y4;
	int i, j, elim_points, total;

	sb_aux = new_sbox();
 
	total = 0;

	while(!sbox_is_empty(sb)) {
		sn = pop_snake(sb);

		elim_points = 0;

		for(i = 0; i < sn->npts-2; i++) {
			x1 = sn->x[i];
			y1 = sn->y[i];
			x2 = sn->x[i+1];
			y2 = sn->y[i+1];
			x3 = sn->x[i+2];
			y3 = sn->y[i+2];
			x4 = sn->x[(i+3)%sn->npts];
			y4 = sn->y[(i+3)%sn->npts];

			num_t = (y4-y3)*(x3-x1)-(y3-y1)*(x4-x3);
			num_s = (y2-y1)*(x3-x1)-(y3-y1)*(x2-x1);
			den   = (y4-y3)*(x2-x1)-(y2-y1)*(x4-x3); 
			

			if (!double_eq(den, 0, CMP_EPSILON)) {
				t = num_t/den; 
				s = num_s/den;


				// It is a local self intersection, we discard 
				// the point
				if (((t < 1 || double_eq(t, 1, CMP_EPSILON))  && 
					 (t > 0 || double_eq(t, 0, CMP_EPSILON))) &&
					((s < 1 || double_eq(s, 1, CMP_EPSILON))  && 
					 (s > 0 || double_eq(s, 0, CMP_EPSILON)))
				) {

					sn->x[(i+1)%sn->npts] = NOT_SET;
					elim_points++;
					i = i+1;
				}

				// If it is not a local intersection then do nothing
				else 
					continue;
			}

			else
				continue;
		}

		// Check the last points
		if(i == sn->npts-2) {
			x1 = sn->x[sn->npts-2];
			y1 = sn->y[sn->npts-2];
			x2 = sn->x[sn->npts-1];
			y2 = sn->y[sn->npts-1];
			x3 = sn->x[0];
			y3 = sn->y[0];

			if (sn->x[1] != NOT_SET) {
				x4 = sn->x[1];
				y4 = sn->y[1];
			}

			else {
				x4 = sn->x[2];
				y4 = sn->y[2];
			}

			num_t = (y4-y3)*(x3-x1)-(y3-y1)*(x4-x3);
			num_s = (y2-y1)*(x3-x1)-(y3-y1)*(x2-x1);
			den   = (y4-y3)*(x2-x1)-(y2-y1)*(x4-x3); 

			if (!double_eq(den, 0, CMP_EPSILON)) {
				t = num_t/den; 
				s = num_s/den;


				// It is a local self intersection, we discard 
				// the point
				if (((t < 1 || double_eq(t, 1, CMP_EPSILON))  && 
					 (t > 0 || double_eq(t, 0, CMP_EPSILON))) &&
					((s < 1 || double_eq(s, 1, CMP_EPSILON))  && 
					 (s > 0 || double_eq(s, 0, CMP_EPSILON)))
				) {

					sn->x[sn->npts-1] = NOT_SET;
					elim_points++;
					i = sn->npts;
				}

				// If it is not a local intersection then do nothing
				else {
					i = sn->npts-1;
				}
			}

			else {
				i = sn->npts-1;
			}
		}

		if (i == sn->npts-1) {
			x1 = sn->x[sn->npts-1];
			y1 = sn->y[sn->npts-1];
			x2 = sn->x[0];
			y2 = sn->y[0];
			if (sn->x[1] != NOT_SET) {
				x3 = sn->x[1];
				y3 = sn->y[1];

				if (sn->x[2] != NOT_SET) {
					x4 = sn->x[2];
					y4 = sn->y[2];
				}

				else {
					x4 = sn->x[3];
					y4 = sn->y[3];
					
				}

				
			}

			else {
				x3 = sn->x[2];
				y3 = sn->y[2];

				if (sn->x[3] != NOT_SET) {
					x4 = sn->x[3];
					y4 = sn->y[3];
				}

				else {
					x4 = sn->x[4];
					y4 = sn->y[4];
					
				}
			}

			num_t = (y4-y3)*(x3-x1)-(y3-y1)*(x4-x3);
			num_s = (y2-y1)*(x3-x1)-(y3-y1)*(x2-x1);
			den   = (y4-y3)*(x2-x1)-(y2-y1)*(x4-x3); 

			if (!double_eq(den, 0, CMP_EPSILON)) {
				t = num_t/den; 
				s = num_s/den;


				// It is a local self intersection, we discard 
				// the point
				if (((t < 1 || double_eq(t, 1, CMP_EPSILON))  && 
					 (t > 0 || double_eq(t, 0, CMP_EPSILON))) &&
					((s < 1 || double_eq(s, 1, CMP_EPSILON))  && 
					 (s > 0 || double_eq(s, 0, CMP_EPSILON)))
				) {

					sn->x[0] = NOT_SET;
					elim_points++;
					i = sn->npts+1;
				}

				// If it is not a local intersection then do nothing
				else {
					i = sn->npts;
				}
			}

			else {
				i = sn->npts;
			}
		}


		if (elim_points > 0) {
			total += elim_points;

			new_s = new_snake(sn->npts-elim_points);

			j = 0;
			for (i = 0; i < sn->npts; i++) {
				if (sn->x[i] == NOT_SET) {
					continue;
				}

				new_s->x[j] = sn->x[i];
				new_s->y[j] = sn->y[i];
				j++;

				if (j == new_s->npts) {
					break;
				}

			}

			push_snake(sb_aux, new_s);
			destroy_snake(sn);
		}

		else 
			push_snake(sb_aux, sn);
	}


	while(!sbox_is_empty(sb_aux))  {
		sn = pop_snake(sb_aux);
		

		elim_points = 0;

		for(i = 0; i < sn->npts-3; i++) {
			x1 = sn->x[i];
			y1 = sn->y[i];
			x2 = sn->x[i+1];
			y2 = sn->y[i+1];
			x3 = sn->x[i+3];
			y3 = sn->y[i+3];
			x4 = sn->x[(i+4)%sn->npts];
			y4 = sn->y[(i+4)%sn->npts];

			num_t = (y4-y3)*(x3-x1)-(y3-y1)*(x4-x3);
			num_s = (y2-y1)*(x3-x1)-(y3-y1)*(x2-x1);
			den   = (y4-y3)*(x2-x1)-(y2-y1)*(x4-x3); 
			

			if (!double_eq(den, 0, CMP_EPSILON)) {
				t = num_t/den; 
				s = num_s/den;


				// It is a local self intersection, we discard 
				// the point
				if (((t < 1 || double_eq(t, 1, CMP_EPSILON))  && 
					 (t > 0 || double_eq(t, 0, CMP_EPSILON))) &&
					((s < 1 || double_eq(s, 1, CMP_EPSILON))  && 
					 (s > 0 || double_eq(s, 0, CMP_EPSILON)))
				) {

					//iprintf("intersection found in (%f, %f)-(%f, %f) , (%f, %f)-(%f, %f)  t = %3.30f , s=%3.30f",
					//		x1, y1, x2, y2, x3, y3, x4, y4, t, s);

					sn->x[i+1] = NOT_SET;
					sn->x[i+2] = NOT_SET;
					elim_points += 2;
					i +=  2;
				}

				// If it is not a local intersection then do nothing
				else 
					continue;
			}
		}

		if (elim_points > 0) {
			total += elim_points;

			new_s = new_snake(sn->npts-elim_points);

			j = 0;
			for (i = 0; i < sn->npts; i++) {
				if (sn->x[i] == NOT_SET) {
					continue;
				}

				new_s->x[j] = sn->x[i];
				new_s->y[j] = sn->y[i];
				j++;

				if (j == new_s->npts) {
					break;
				}

			}

			push_snake(sb, new_s);
			destroy_snake(sn);
		}

		else 
			push_snake(sb, sn);
	}

	//iprintf("Discarded %d points", total);
}

void discard_too_close_points(sbox sb) {
	sbox sb_aux;
	snake s, new_s;
	double d, x1, y1, x2, y2;
	int i, j, new_npts, discarded_points;
	queue q;
	int *ip;

	sb_aux = new_sbox();
	
	q = new_queue();

	discarded_points = 0;
	while(!sbox_is_empty(sb)) {
		s = pop_snake(sb);
		new_npts = s->npts;

		for(i = 0; i < s->npts; i++) {
			x1 = s->x[i];
			y1 = s->y[i];
			x2 = s->x[(i+1)%s->npts];
			y2 = s->y[(i+1)%s->npts];

			d = sqrt(pow(x1-x2,2)+pow(y1-y2,2));

			if (double_eq(d,0, NEAR_EPSILON))  {
				ip = (int *)IDL_MemAlloc(sizeof(int), 
						"Could Not Allocate Memory for ip and that's a shame",
						IDL_MSG_INFO);

				queue_push(q, ip);
				new_npts--;
				discarded_points++;
				i++;
			}

			else 
				continue;


		}

		if (new_npts < s->npts && new_npts > 0) {
			int to_elim;

			new_s = new_snake(new_npts);

			ip = (int *)queue_pop(q);
			to_elim = *ip;

			IDL_MemFree(
					ip, 
					"Could Not free Memory for ip", 
					IDL_MSG_INFO
			);

			j = 0;
			for(i = 0; i < s->npts; i++) {

				if (i == to_elim) {
					if (!is_empty(q)) {
						ip = (int *)queue_pop(q);
						to_elim =  *ip;
						IDL_MemFree(
							ip, 
							"Could Not free Memory for ip", 
							IDL_MSG_INFO
						);
					}

					else {
						to_elim = s->npts;
					}

					continue;
				}

				new_s->x[j] = s->x[i];
				new_s->y[j] = s->y[i];
				j++;

			}

			push_snake(sb_aux, new_s);

			destroy_snake(s);
		}

		else 
			push_snake(sb_aux, s);
	}

	while(!sbox_is_empty(sb_aux))  {
		s = pop_snake(sb_aux);
		push_snake(sb, s);
	}

	//iprintf("Discarded %d points", discarded_points);
}

int is_interior(vertex v) {
	if (v->ind == 0) return 0;

	if (v->e[EDGE_S] != NULL) 
		if (v->e[EDGE_S]->v[0]->ind == 0) return 0;

	if (v->e[EDGE_SW] != NULL) 
		if (v->e[EDGE_SW]->v[0]->ind == 0) return 0;

	if (v->e[EDGE_W] != NULL) 
		if (v->e[EDGE_W]->v[0]->ind == 0) return 0;

	if (v->e[EDGE_N] != NULL) 
		if (v->e[EDGE_N]->v[1]->ind == 0) return 0;

	if (v->e[EDGE_NE] != NULL) 
		if (v->e[EDGE_NE]->v[1]->ind == 0) return 0;

	if (v->e[EDGE_E] != NULL) 
		if (v->e[EDGE_E]->v[1]->ind == 0) return 0;

	return 1;
}

int is_exterior(vertex v) {
	if (v->ind == 1) return 0;

	if (v->e[EDGE_S] != NULL) 
		if (v->e[EDGE_S]->v[0]->ind == 1) return 0;

	if (v->e[EDGE_SW] != NULL) 
		if (v->e[EDGE_SW]->v[0]->ind == 1) return 0;

	if (v->e[EDGE_W] != NULL) 
		if (v->e[EDGE_W]->v[0]->ind == 1) return 0;

	if (v->e[EDGE_N] != NULL) 
		if (v->e[EDGE_N]->v[1]->ind == 1) return 0;

	if (v->e[EDGE_NE] != NULL) 
		if (v->e[EDGE_NE]->v[1]->ind == 1) return 0;

	if (v->e[EDGE_E] != NULL) 
		if (v->e[EDGE_E]->v[1]->ind == 1) return 0;

	return 1;
}

int is_singleton(vertex v) {
	return  v->e[EDGE_E] ->inter_x  != NOT_SET && 
			v->e[EDGE_NE]->inter_x  != NOT_SET &&
			v->e[EDGE_N] ->inter_x  != NOT_SET && 
			v->e[EDGE_W] ->inter_x  != NOT_SET && 
			v->e[EDGE_SW]->inter_x  != NOT_SET && 
			v->e[EDGE_S] ->inter_x  != NOT_SET;
}

int is_still_valid(vertex v) {
	dprintf("Want to remove %p", v);
	dprintf("S");
	if (v->e[EDGE_S] != NULL) 
		if (v->e[EDGE_S]->inter_sign == -1) return 1;

	dprintf("SW");
	if (v->e[EDGE_SW] != NULL) 
		if (v->e[EDGE_SW]->inter_sign == -1) return 1;

	dprintf("W");
	if (v->e[EDGE_W] != NULL) 
		if (v->e[EDGE_W]->inter_sign == -1) return 1;

	dprintf("N");
	if (v->e[EDGE_N] != NULL) 
		if (v->e[EDGE_N]->inter_sign == 1) return 1;

	dprintf("NE");
	if (v->e[EDGE_NE] != NULL) 
		if (v->e[EDGE_NE]->inter_sign == 1) return 1;

	dprintf("E");
	if (v->e[EDGE_E] != NULL) 
		if (v->e[EDGE_E]->inter_sign == 1) return 1;

	return 0;
}

vertex other_side(vertex v, int edge_dir) {
	if (edge_dir < 3 && v->e[edge_dir] != NULL) {
		return v->e[edge_dir]->v[0];
	}

	else if (edge_dir >= 3 && v->e[edge_dir] != NULL) {
		return v->e[edge_dir]->v[1];
	}

	return NULL;
}

void init_indicator(double *x, double *y, int npts, mesh grid) {
	IDL_LONG i, inext, j,k;
	IDL_LONG y_inf, y_sup, x_ini;
	double x_down, x_up, y_down, y_up, quo, m, c;

	for(i=0; i<npts; i++) {
		inext = (i+1)%npts;


		// If the edge is horizontal then the ray does not cross it
		// and is discarded
		if (y[i] == y[inext]) continue;

		if (y[i] > y[inext]) {
			x_down = x[inext];
			y_down = y[inext];
			x_up   = x[i];
			y_up   = y[i];

		}

		else {
			x_up     = x[inext];
			y_up     = y[inext];
			x_down   = x[i];
			y_down   = y[i];
		}

		y_inf = (IDL_LONG)ceil(y_down/(grid->hy));

		// This is for the convention that a ray cross the edge
		// if one end point is STRICTLY above the ray and the other
		// down or on the edge
		y_sup = (IDL_LONG)ceil(y_up/(grid->hy));


		quo = x_up-x_down;

		// Handle the case when the vertex is vertical
		if (quo == 0){
			if (x_down == 0.0) continue;


			for(k=y_inf; k<y_sup; k++){
				x_ini = (IDL_LONG)floor(x_down/(grid->hx));

				for(j=k*(grid->nx)+x_ini; j>=k*(grid->nx); j--) 
					grid->vlist[j].ind++;
			}
		}

		// Case Vertex non vertical (horizontal is discarded before)
		else {

			m = (y_up - y_down)/quo;
			c = (y_down*x_up - y_up*x_down)/quo;


			for(k=y_inf; k<y_sup; k++){
				x_ini = (IDL_LONG)floor((k*(grid->hy)-c)/(m*grid->hx));

				for(j=k*(grid->nx)+x_ini; j>=k*(grid->nx); j--) 
					grid->vlist[j].ind++;
			}
		}
	}

	// If the ray from the point crosses an even number of times then 
	// the point is out the ROI, else is in
	for(i=0; i< grid->nvertices; i++) 
		grid->vlist[i].ind %= 2;
}

snake new_snake(IDL_LONG npts) {
	snake new;

	new = (snake)IDL_MemAlloc(
			sizeof(struct Snake),
			"No Memory : snake",
			IDL_MSG_LONGJMP);

	new->x = (double *)IDL_MemAlloc(
			sizeof(double) * npts,
			"No Memory : snake->x",
			IDL_MSG_LONGJMP);

	new->y = (double *)IDL_MemAlloc(
			sizeof(double) * npts,
			"No Memory : snake->x",
			IDL_MSG_LONGJMP);

	new->npts = npts;

	return new;
}

void destroy_snake(snake s) {
	IDL_MemFree(
			s->x,
			"destroy_snake: Couldn't free memory for x",
			IDL_MSG_INFO
			);

	IDL_MemFree(
			s->y,
			"destroy_snake: Couldn't free memory for y",
			IDL_MSG_INFO
			);

	IDL_MemFree(
			s,
			"destroy_snake: Couldn't free memory for s",
			IDL_MSG_INFO
			);
}

sbox new_sbox() {
	sbox new;

	new = (sbox)IDL_MemAlloc(
			sizeof(struct SnakeBox),
			"No Memory : sbox",
			IDL_MSG_LONGJMP);

	new->snakes = new_queue();
	new->nsnakes = 0;

	return new;
}

void push_snake(sbox sb, snake s) {
	queue_push(sb->snakes,s);
	sb->nsnakes++;
}

snake pop_snake(sbox sb) {
	snake ret;

	sb->nsnakes--;
	ret = (snake)queue_pop(sb->snakes);

	return ret;
}

int sbox_is_empty(sbox sb) {
	return sb->nsnakes == 0;
}

void destroy_sbox(sbox sb) {
	destroy_queue(sb->snakes);

	IDL_MemFree(
			sb,
			"destroy_sbox: Couldn't free memory for sb",
			IDL_MSG_INFO
			);
}



// Mesh Structure Related Functions/Methods
mesh new_mesh(IDL_LONG nx, IDL_LONG ny, IDL_LONG x_max, IDL_LONG y_max, IDL_LONG bfx, IDL_LONG bfy){
	mesh new;
	IDL_LONG i,j;
	int k;
	double hx, hy;
	vertex vlist, curr_vertex;
	edge   elist, curr_edge;
	face   flist, curr_face;

	new = (mesh)IDL_MemAlloc(
			sizeof(struct Mesh),
			"No Memory : mesh",
			IDL_MSG_LONGJMP
			);

	new->nx = nx; new->ny = ny;
	hx = (double)x_max/(nx-1); 
	hy = (double)y_max/(ny-1);
	new->hx = hx;
	new->hy = hy;
	new->m  = hy/hx;

	new->bfx = bfx;
	new->bfy = bfy;

	new->x_max = (double)x_max;
	new->y_max = (double)y_max;

	//new->nvertices = (nx+2)*(ny+2);
	//new->nedges    = 3*(nx+1)*(ny+1)+nx+1+ny+1;
	//new->nfaces    = 2*(nx+1)*(ny+1);

	new->nvertices = (nx+2*bfx)*(ny+2*bfy);
	new->nedges    = 3*(nx+2*bfx-1)*(ny+2*bfy-1)+nx+2*bfx-1+ny+2*bfy-1;
	new->nfaces    = 2*(nx+2*bfx-1)*(ny+2*bfy-1);

	iprintf("nvert = %ld, nedges = %ld, nfaces = %ld", new->nvertices, new->nedges, new->nfaces); 
	iprintf("mem vert = %f, mem edg = %f, mem faces = %f",(double)new->nvertices * sizeof(struct Vertex)/1048576,(double) new->nedges * sizeof(struct Edge)/1048576,(double) new->nfaces * sizeof(struct Face)/1048576);

	iprintf(" bfx = %ld, bfy = %ld ", bfx, bfy);



	// Allocating Memory for the arrays 
	new->vlist = (vertex)IDL_MemAlloc(
			sizeof(struct Vertex)*(new->nvertices),
			"No Memory : vlist", 
			IDL_MSG_LONGJMP
			);

	new->elist = (edge)IDL_MemAlloc(
			sizeof(struct Edge)*(new->nedges),
			"No Memory : elist", 
			IDL_MSG_LONGJMP
			);

	new->flist = (face)IDL_MemAlloc(
			sizeof(struct Face)*(new->nfaces),
			"No Memory : flist",
			IDL_MSG_LONGJMP)
		;

	vlist = new->vlist;
	elist = new->elist;
	flist = new->flist;


	// Fill the vertices, edges and faces will be filled when they are created
	curr_vertex = vlist;

	iprintf("Filling Vertices");
	for(j = 0; j < ny+2*bfy; j++) {
		for(i = 0; i < nx+2*bfx; i++) {
			curr_vertex->x        = (double)(i-bfx)*hx;
			curr_vertex->y        = (double)(j-bfy)*hy;
			curr_vertex->ind      = 0;
			curr_vertex->is_queued   = 0;
			curr_vertex->was_queued = 0;

			for(k = 0; k < 6; k++) {
				curr_vertex->e[k] = NULL;
				curr_vertex->f[k] = NULL;
			}
			curr_vertex++;
		}
	}


	iprintf("Filling Interior Edges");
	// Now we fill the interior edges, filling properly they links to vertices
	curr_vertex = vlist;
	curr_edge   = elist;

	for(j = 0; j < ny+2*bfy-1; j++) {
		for(i = 0; i < nx+2*bfx-1; i++) {

			//    NAMING CONVENTION FOR EDGES ADJACENT TO A VERTEX 
			//
			//    (the naming is relative to the vertex)
			//                ________________
			//                |      /|      /|
			//                |     / |     / |
			//                |    /  |N   /  |       
			//                |   /   |   /   |
			//                |  /    |  /NE  |
			//                | /     | /     |
			//                |/___W__|/______|
			//                |      /|v  E  /| 
			//                |   SW/ |     / |
			//                |    /  |    /  |
			//                |   /  S|   /   |
			//                |  /    |  /    |
			//                | /     | /     |
			//                |/______|/______|
			//
			//
			//    FILLING ORDER
			// 
			//   curr_v+nx     curr_v+nx+1
			//        |         /
			//        |        /
			//        |       /
			//        |      /
			//  first |     /
			//        |    / second
			//        |   /
			//        |  /
			//        | /
			//        |/_________
			//   curr_v  third   curr_v+1

			// First
			curr_edge->v[0]            = curr_vertex;
			curr_edge->v[1]            = curr_vertex+(nx+2*bfx);
			curr_edge->f[FACE_LEFT]    = NULL;
			curr_edge->f[FACE_RIGHT]   = NULL;
			curr_edge->v[0]->e[EDGE_N] = curr_edge;
			curr_edge->v[1]->e[EDGE_S] = curr_edge;
			curr_edge->inter_x         = NOT_SET;
			curr_edge->inter_y         = NOT_SET;
			curr_edge->inter_sign      = 0;
			curr_edge->type            = EDGE_V;
			curr_edge++;

			// Second
			curr_edge->v[0]             = curr_vertex;
			curr_edge->v[1]             = curr_vertex+(nx+2*bfx)+1;
			curr_edge->f[FACE_LEFT]     = NULL;
			curr_edge->f[FACE_RIGHT]    = NULL;
			curr_edge->v[0]->e[EDGE_NE] = curr_edge;
			curr_edge->v[1]->e[EDGE_SW] = curr_edge;
			curr_edge->inter_x          = NOT_SET;
			curr_edge->inter_y          = NOT_SET;
			curr_edge->inter_sign       = 0;
			curr_edge->type             = EDGE_D;
			curr_edge++;


			// Last
			curr_edge->v[0]            = curr_vertex;
			curr_edge->v[1]            = curr_vertex+1;
			curr_edge->f[FACE_UP]      = NULL;
			curr_edge->f[FACE_DOWN]    = NULL;
			curr_edge->v[0]->e[EDGE_E] = curr_edge;
			curr_edge->v[1]->e[EDGE_W] = curr_edge;
			curr_edge->inter_x         = NOT_SET;
			curr_edge->inter_y         = NOT_SET;
			curr_edge->inter_sign      = 0;
			curr_edge->type            = EDGE_H;
			curr_edge++;

			curr_vertex++;
		}

		curr_vertex++; // Skip the last in the row, will be processed later
	}


	iprintf("Filling Boundary Edges");
	// Same for the upper boundary of the mesh ...
	curr_vertex = vlist+(ny+2*bfy-1)*(nx+2*bfx); 

	iprintf("upper");
	for(i=0; i<nx+2*bfx-1; i++) {
		curr_edge->v[0]             = curr_vertex;
		curr_edge->v[1]             = curr_vertex+1;
		curr_edge->f[FACE_UP]       = NULL;
		curr_edge->f[FACE_DOWN]     = NULL;
		curr_edge->v[0]->e[EDGE_E]  = curr_edge;
		curr_edge->v[1]->e[EDGE_W]  = curr_edge;
		curr_edge->inter_x          = NOT_SET;
		curr_edge->inter_y          = NOT_SET;
		curr_edge->inter_sign       = 0;
		curr_edge->type             = EDGE_H;

		curr_vertex++;
		curr_edge++;
	}

	iprintf("right");
	// ...and the right boundary
	curr_vertex = vlist+nx+2*bfx-1;


	for(j=0; j<ny+2*bfy-1; j++) {
		curr_edge->v[0]                = curr_vertex;
		curr_edge->v[1]                = curr_vertex+(nx+2*bfx);

		curr_edge->f[FACE_LEFT]        = NULL;
		curr_edge->f[FACE_RIGHT]       = NULL;

		curr_edge->v[0]->e[EDGE_N]     = curr_edge;
		curr_edge->v[1]->e[EDGE_S]     = curr_edge;
		curr_edge->inter_x             = NOT_SET;
		curr_edge->inter_y             = NOT_SET;
		curr_edge->inter_sign          = 0;
		curr_edge->type                = EDGE_V;

		curr_vertex += (nx+2*bfx);
		curr_edge++;
	}



	// Finally the faces are filled, filling at the same time the edge and
	// vertices links.
	curr_vertex  = vlist;
	curr_edge    = elist;
	curr_face    = flist;

	iprintf("Filling Faces");
	for(j=0; j<ny+2*bfy-1; j++) {
		for(i=0; i<nx+2*bfx-1; i++) {
			//    NAMING CONVENTION FOR FACES ADJACENT TO A VERTEX 
			//
			//    (the naming is relative to the vertex)
			//                ________________
			//                |      /|      /|
			//                |     / |     / |
			//                |    /  | NE1/  |       
			//                |   /   |   /   |
			//                |  /  W |  / NE2|
			//                | /     | /     |
			//                |/______|/______|
			//                |      /|v     /| 
			//                | SW2 / |     / |
			//                |    /  | E  /  |
			//                |   /   |   /   |
			//                |  / SW1|  /    |
			//                | /     | /     |
			//                |/______|/______|


			//           REFERENCE FOR A ROTATED FACE
			//
			//                              (e[1]->f[FACE_UP])
			//                           
			//                         v[2]____e[1]_____v[1]
			//                             |           /
			//                             |          / 
			//                             |         /
			//                             | curr_f /
			//  (e[2]->f[FACE_LEFT])   e[2]|       /
			//                             |      /
			//                             |     /  e[0]   (e[0]->f[FACE_RIGHT])
			//                             |    /
			//                             |   /
			//                             |  /
			//                             | /
			//                             |/
			//                             v[0]
			//

			curr_face->type = FACE_ROTATED;

			curr_face->v[0] = curr_vertex;
			curr_face->v[1] = curr_vertex+(nx+2*bfx)+1;
			curr_face->v[2] = curr_vertex+(nx+2*bfx);


			curr_face->e[0] = curr_face->v[0]->e[EDGE_NE];
			curr_face->e[1] = curr_face->v[1]->e[EDGE_W];
			curr_face->e[2] = curr_face->v[2]->e[EDGE_S];


			curr_face->v[0]->f[FACE_NE1]  = curr_face;
			curr_face->v[1]->f[FACE_SW2]  = curr_face;
			curr_face->v[2]->f[FACE_SE]   = curr_face;

			curr_face->e[0]->f[FACE_LEFT] = curr_face;
			curr_face->e[1]->f[FACE_DOWN] = curr_face;
			curr_face->e[2]->f[FACE_RIGHT] = curr_face;

			curr_face++;


			//            REFERENCE FOR A NORMAL FACE
			//
			//                                         v[2] 
			//                                        /|
			//                                       / |
			//                                      /  |
			//                                     /   |
			//                                    /    |
			//                                   /     |
			//     (e[2]->f[FACE_LEFT])    e[2] /      |e[1]  (e[1]->f[FACE_RIGHT])
			//                                 /       |
			//                                /        |
			//                               / curr_f  |
			//                              /          |
			//                             /           |
			//                            /            |
			//                       v[0]/____e[0]_____|v[1]
			//
			//                       (e[0]->f[FACE_DOWN])
			//                       
			//                       
			curr_face->type = FACE_NORMAL;


			curr_face->v[0] = curr_vertex;
			curr_face->v[1] = curr_vertex+1;
			curr_face->v[2] = curr_vertex+(nx+2*bfx)+1;

			curr_face->e[0] = curr_face->v[0]->e[EDGE_E];
			curr_face->e[1] = curr_face->v[1]->e[EDGE_N];
			curr_face->e[2] = curr_face->v[2]->e[EDGE_SW];

			curr_face->v[0]->f[FACE_NE2]  = curr_face;
			curr_face->v[1]->f[FACE_NW]   = curr_face;
			curr_face->v[2]->f[FACE_SW1]  = curr_face;

			curr_face->e[0]->f[FACE_UP] = curr_face;
			curr_face->e[1]->f[FACE_LEFT] = curr_face;
			curr_face->e[2]->f[FACE_RIGHT] = curr_face;

			curr_face++;
			curr_vertex++;

		}

		curr_vertex++;
	}

	iprintf("finished");
	return new;
}

void destroy_mesh(mesh m) {
	IDL_MemFree(
			m->vlist,
			"destroyMesh: Couldn't free memory for vlist",
			IDL_MSG_INFO
			);
	IDL_MemFree(
			m->elist,
			"destroyMesh: Couldn't free memory for elist",
			IDL_MSG_INFO
			);
	IDL_MemFree(
			m->flist,
			"destroyMesh: Couldn't free memory for flist",
			IDL_MSG_INFO
			);
	IDL_MemFree(
			m,
			"destroyMesh: Couldn't free memory for m",
			IDL_MSG_INFO
			);
}

void print_mesh(mesh grid) {
	IDL_LONG i;
	for(i=0; i<grid->nvertices; i++) {
		struct Vertex myv = grid->vlist[i];
		dprintf("Vertex : %ld", i);

		dprintf("edge N is %p",myv.e[EDGE_N]); 
		dprintf("edge NE is %p",myv.e[EDGE_NE]); 
		dprintf("edge E is %p",myv.e[EDGE_E]); 
		dprintf("edge S is %p",myv.e[EDGE_S]); 
		dprintf("edge SW is %p",myv.e[EDGE_SW]); 
		dprintf("edge W is %p",myv.e[EDGE_W]); 

	}

	for(i=0; i<grid->nedges; i++) {

		struct Edge mye = grid->elist[i];

		dprintf("Edge : %ld\n v0 = (%f,%f), v1= (%f, %f)", i, mye.v[0]->x, mye.v[0]->y, mye.v[1]->x, mye.v[1]->y);
	}

	for(i=0; i<grid->nfaces; i++) {
		struct Face myf = grid->flist[i];

		dprintf("Face: %ld\nv0=(%f,%f), v1=(%f,%f), v2=(%f,%f)\ne0->v0=(%f,%f), e0->v1=(%f,%f)\ne1->v0=(%f,%f), e1->v1=(%f,%f)\ne2->v0=(%f,%f), e2->v1=(%f,%f)\nf->R=%p", i, 
				myf.v[0]->x, myf.v[0]->y, 
				myf.v[1]->x, myf.v[1]->y, 
				myf.v[2]->x, myf.v[2]->y, 
				myf.e[0]->v[0]->x, myf.e[0]->v[0]->y, 
				myf.e[0]->v[1]->x, myf.e[0]->v[1]->y, 
				myf.e[1]->v[0]->x, myf.e[1]->v[0]->y, 
				myf.e[1]->v[1]->x, myf.e[1]->v[1]->y, 
				myf.e[2]->v[0]->x, myf.e[2]->v[0]->y, 
				myf.e[2]->v[1]->x, myf.e[2]->v[1]->y, 
				myf.type==FACE_ROTATED?myf.e[0]->f[FACE_RIGHT]: myf.e[1]->f[FACE_RIGHT]);
	}
}

// Queue Structure Related Functions/Methods
queue new_queue() {
	queue new;

	new = (queue)IDL_MemAlloc(
			sizeof(struct Queue),
			"No Memory : queue",
			IDL_MSG_LONGJMP);

	new->first = NULL;
	new->last  = NULL;

	return new;
}


int is_empty(queue q) {
	return q->first == NULL;
}

void queue_push(queue q, void *data) {
	qnode new;

	dprintf("pushing %p", data);
	new = (qnode)IDL_MemAlloc(
			sizeof(struct QueueNode),
			"No Memory : qnode",
			IDL_MSG_LONGJMP);

	new->data = data;
	new->next = NULL;

	if (is_empty(q)) {
		q->first = new;
		q->last = new;
	}

	else {
		q->last->next = new;
		q->last = new;
	}

	if (q->first != NULL && q->first != q->last && q->first->next == NULL) {
		dprintf("ALERT queue is corrupted");
	}
}

void *queue_pop(queue q) {
	qnode aux;
	void *data;

	// If there is only one element left 
	if (q->first == q->last && q->first != NULL) { 
		data = q->first->data;
		IDL_MemFree(
				q->first,
				"destroyMesh: Couldn't free memory for aux",
				IDL_MSG_INFO
				);
		q->first = NULL;
		q->last  = NULL;
	}

	else if (q->first == NULL && q->last == NULL) {
		data = NULL;
	}

	else {
		aux = q->first;
		q->first = aux->next;
		data = aux->data;

		IDL_MemFree(
				aux,
				"destroyMesh: Couldn't free memory for aux",
				IDL_MSG_INFO
				);
	}


	if (q->first != NULL && q->first != q->last && q->first->next == NULL) {
		dprintf("ALERT queue is corrupted");
	}
	return data;
}

void queue_remove(queue q, void *data) {
	qnode curr, prev;

	dprintf("removing %p", data);
	if (is_empty(q)) {
		dprintf("Remove Failed : Empty Queue");
		return;
	}


	curr = q->first;


	if (curr->data == data) {
		if (curr == q->last) {
			q->first = NULL;
			q->last  = NULL;
		}

		else {
			q->first = curr->next; // q->first->next
		}

		IDL_MemFree(
				curr,
				"queue_remove: Couldn't free memory for curr",
				IDL_MSG_INFO
				);
		if (q->first != NULL && q->first != q->last && q->first->next == NULL) {
		}
		return;
	}

	prev = curr;
	curr = curr->next;

	while(curr != NULL) {
		if (curr->data == data) {
			prev->next = curr->next;
			if (curr == q->last) {
				q->last = prev;
			}
			IDL_MemFree(
					curr,
					"queue_remove: Couldn't free memory for curr",
					IDL_MSG_INFO
					);
			if (q->first != NULL && q->first != q->last && q->first->next == NULL) {
			}
			return;
		}

		prev = curr;
		curr = curr->next;
	}
	dprintf("Remove Failed: Element Not Found");
}

void destroy_queue(queue q) {
	while(!is_empty(q)) queue_pop(q);

	IDL_MemFree(
			q,
			"destroyMesh: Couldn't free memory for data",
			IDL_MSG_INFO
			);
}

// IDL Linking Functions

int IDL_Load(void) {
	static IDL_SYSFUN_DEF2 main_def[] = { 
		{evolve_snakes, "EVOLVE_SNAKES", 0, 100, 0, 0}
	};
	return IDL_SysRtnAdd(main_def, IDL_TRUE, IDL_CARRAY_ELTS(main_def));
}


// Display Functions
static char msg[1024];
void iprintf(char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	vsprintf(msg, fmt, ap);
	va_end(ap);

	IDL_Message(IDL_M_GENERIC, IDL_MSG_INFO, msg);
}

void dprintf(char *fmt, ...) {
	va_list ap;
	FILE *fp;
	if (DEBUG == 1) {
		fp = fopen("/Users/bunder/tsnake_log.txt", "a");
		va_start(ap, fmt);
		vfprintf(fp, fmt, ap);
		fprintf(fp,"\n");
		va_end(ap);
		fclose(fp);
	}
}
void print_edge(edge e, char *name) {
	if (e != NULL) {
		if (e->type == EDGE_H) {
			if (e->inter_x == NOT_SET)
				iprintf("- %s : (%f, %f)-(%f,%f) sign:%d No Int",
						name, 
						e->v[0]->x, e->v[0]->y, 
						e->v[1]->x, e->v[1]->y,
						e->inter_sign
					   );
			else
				iprintf("- %s : (%f, %f)-(%f,%f) sign:%d Int:(%f,%f)",
						name, 
						e->v[0]->x, e->v[0]->y, 
						e->v[1]->x, e->v[1]->y, 
						e->inter_sign,
						e->inter_x, e->inter_y
					   );
		}
		else if (e->type == EDGE_D) {
			if (e->inter_x == NOT_SET)
				iprintf("/ %s : (%f, %f)-(%f,%f) sign:%d No Int",
						name,
						e->v[0]->x, e->v[0]->y,
						e->v[1]->x, e->v[1]->y,
						e->inter_sign
					   );
			else
				iprintf("/ %s : (%f, %f)-(%f,%f) sign:%d Int:(%f,%f)",
						name,
						e->v[0]->x, e->v[0]->y,
						e->v[1]->x, e->v[1]->y,
						e->inter_sign,
						e->inter_x, e->inter_y
					   );
		}
		else {
			if (e->inter_x == NOT_SET) 
				iprintf("| %s : (%f, %f)-(%f,%f) sign:%d No Int",
						name, 
						e->v[0]->x, e->v[0]->y,
						e->v[1]->x, e->v[1]->y,
						e->inter_sign
					   ); 

			else
				iprintf("| %s : (%f, %f)-(%f,%f) sign:%d Int:(%f,%f)",
						name, 
						e->v[0]->x, e->v[0]->y,
						e->v[1]->x, e->v[1]->y,
						e->inter_sign,
						e->inter_x, e->inter_y
					   );
		}
	}
	else
		iprintf("%s is a NULL edge!", name);
}

void print_indicator(mesh grid) {
	IDL_LONG i,j;

	IDL_LONG nx, ny, bfx, bfy;

	nx = grid->nx;
	ny = grid->ny;
	bfx = grid->bfx;
	bfy = grid->bfy;

	for(j = ny+2*bfy-1; j>= 0; j--) {
		char *line;
		line = (char *)malloc(nx+2*bfx+1 * sizeof(char));
		for(i = 0; i < nx+2*bfx; i++) {
			vertex curr;

			curr = grid->vlist + i + j*(nx+2*bfx);

			if (curr->ind == 0) {
				line[i] = '-';
			}

			else
				line[i] = '*';
		}

		line[grid->nx+2*bfx] = '\0';

		if ((j-bfy)*grid->hy < 10 && (j-bfy)*grid->hy >= 0) 
			dprintf("0%06.4f %s", (j-bfy)*grid->hy, line);
		else
			dprintf("%06.4f %s", (j-bfy)*grid->hy, line);

		free(line);
	}
}

void iprint_indicator(mesh grid) {
	IDL_LONG i,j;

	IDL_LONG nx, ny, bfx, bfy;

	nx = grid->nx;
	ny = grid->ny;
	bfx = grid->bfx;
	bfy = grid->bfy;

	for(j = ny+2*bfy-1; j>= 0; j--) {
		char *line;
		line = (char *)malloc(nx+2*bfx+1 * sizeof(char));
		for(i = 0; i < nx+2*bfx; i++) {
			vertex curr;

			curr = grid->vlist + i + j*(nx+2*bfx);
			if (curr->is_queued == 1 && curr->ind == 1)
				line[i] = 'Q';

			else if (curr->is_queued == 1 && curr->ind == 0)
				line[i] = 'q';

			else if (curr->ind == 0)
				line[i] = '-';
			else
				line[i] = '*';
		}

		line[grid->nx+2*bfx] = '\0';

		if ((j-bfy)*grid->hy < 10 && (j-bfy)*grid->hy >= 0) 
			iprintf("0%06.4f %s", (j-bfy)*grid->hy, line);
		else
			iprintf("%06.4f %s", (j-bfy)*grid->hy, line);

		free(line);
	}
}
void print_indicator_full(mesh grid) {
	IDL_LONG i,j;
	IDL_LONG nx, ny, bfx, bfy;
	vertex curr;
	char *line, *line2;

	nx = grid->nx;
	ny = grid->ny;
	bfx = grid->bfx;
	bfy = grid->bfy;

	line  = (char *)malloc(2*(grid->nx+2*bfx) * sizeof(char));
	line2 = (char *)malloc(2*(grid->nx+2*bfx) * sizeof(char));


	for(i =0; i<nx+2*bfx-1; i++) {
		curr = grid->vlist + i + (ny+2*bfy-1)*(nx+2*bfx);
		if (curr->ind == 0) 
			line[2*i] = ' ';
		else
			line[2*i] = '*';

		if (curr->e[EDGE_E]->inter_x != NOT_SET)
			line[2*i+1] = '+';
		else
			line[2*i+1] = '-';
	}


	curr = grid->vlist + grid->nx+2*bfx-1 + (grid->ny+2*bfy-1)*(grid->nx+2*bfx);
	if (curr->ind == 0) 
		line[2*(nx+2*bfx-1)] = ' ';
	else 
		line[2*(nx+2*bfx-1)] = '*';

	line[2*(nx+2*bfx)-1] = '\0';
	dprintf("%s", line);



	for(j = ny+2*bfy-2; j>= 0; j--) {
		for(i = 0; i<nx+2*bfx-1; i++) {
			vertex curr;
			curr = grid->vlist + i + j*(nx+2*bfx);

			if (curr->ind == 0) 
				line[2*i] = ' ';

			else 
				line[2*i] = '*';

			if (curr->e[EDGE_N]->inter_x != NOT_SET)
				line2[2*i] = '+';

			else
				line2[2*i] = '|';

			if (curr->e[EDGE_NE]->inter_x != NOT_SET)
				line2[2*i+1] = 'X';

			else
				line2[2*i+1] = '/';
			if (curr->e[EDGE_E]->inter_x != NOT_SET)
				line[2*i+1] = '+';

			else
				line[2*i+1] = '-';
		}

		curr = grid->vlist + nx+2*bfx-1 + j*(nx+2*bfx);
		if (curr->ind == 0) 
			line[2*(nx+2*bfx-1)] = ' ';

		else 
			line[2*(nx+2*bfx-1)] = '*';

		if (curr->e[EDGE_N]->inter_x != NOT_SET)
			line2[2*(nx+2*bfx-1)] = '+';
		else
			line2[2*(nx+2*bfx-1)] = '|';

		line[2*(nx+2*bfx)-1] = '\0';
		line2[2*(nx+2*bfx)-1] = '\0';

		dprintf("%s", line2);
		dprintf("%s", line);
	}
	free(line);
	free(line2);
}

void iprint_indicator_full(mesh grid) {
	IDL_LONG i,j;
	IDL_LONG nx, ny, bfx, bfy;
	vertex curr;
	char *line, *line2;

	nx = grid->nx;
	ny = grid->ny;
	bfx = grid->bfx;
	bfy = grid->bfy;

	line  = (char *)malloc(2*(grid->nx+2*bfx) * sizeof(char));
	line2 = (char *)malloc(2*(grid->nx+2*bfx) * sizeof(char));


	for(i =0; i<nx+2*bfx-1; i++) {
		curr = grid->vlist + i + (ny+2*bfy-1)*(nx+2*bfx);
		if (curr->is_queued == 1 && curr->ind == 1)
			line[2*i] = 'Q';
		else if (curr->is_queued == 1 && curr->ind  == 0) 
			line[2*i] = 'q';
		else if (curr->ind == 0) 
			line[2*i] = ' ';
		else 
			line[2*i] = '*';

		if (curr->e[EDGE_E]->inter_x != NOT_SET)
			line[2*i+1] = '+';
		else
			line[2*i+1] = '-';
	}


	curr = grid->vlist + grid->nx+2*bfx-1 + (grid->ny+2*bfy-1)*(grid->nx+2*bfx);

	if (curr->is_queued == 1 && curr->ind == 1)
		line[2*(nx+2*bfx-1)] = 'Q';
	else if (curr->is_queued == 1 && curr->ind == 0)
		line[2*(nx+2*bfx-1)] = 'q';
	else if (curr->ind == 0) 
		line[2*(nx+2*bfx-1)] = ' ';
	else 
		line[2*(nx+2*bfx-1)] = '*';

	line[2*(nx+2*bfx)-1] = '\0';
	iprintf("%s", line);



	for(j = ny+2*bfy-2; j>= 0; j--) {
		for(i = 0; i<nx+2*bfx-1; i++) {
			vertex curr;
			curr = grid->vlist + i + j*(nx+2*bfx);

			if (curr->is_queued == 1 && curr->ind == 1)
				line[2*i] = 'Q';

			else if (curr->is_queued == 1 && curr->ind == 0)
				line[2*i] = 'q';

			else if (curr->ind == 0) 
				line[2*i] = ' ';

			else 
				line[2*i] = '*';


			if (curr->e[EDGE_N]->inter_x != NOT_SET)
				line2[2*i] = '+';

			else
				line2[2*i] = '|';

			if (curr->e[EDGE_NE]->inter_x != NOT_SET)
				line2[2*i+1] = 'X';

			else
				line2[2*i+1] = '/';
			if (curr->e[EDGE_E]->inter_x != NOT_SET)
				line[2*i+1] = '+';

			else
				line[2*i+1] = '-';
		}

		curr = grid->vlist + nx+2*bfx-1 + j*(nx+2*bfx);
		if (curr->is_queued == 1 && curr->ind == 1)
			line[2*(nx+2*bfx-1)] = 'Q';

		else if (curr->is_queued == 1 && curr->ind == 0)
			line[2*(nx+2*bfx-1)] = 'q';

		else if (curr->ind == 0) 
			line[2*(nx+2*bfx-1)] = ' ';

		else 
			line[2*(nx+2*bfx-1)] = '*';

		if (curr->e[EDGE_N]->inter_x != NOT_SET)
			line2[2*(nx+2*bfx-1)] = '+';
		else
			line2[2*(nx+2*bfx-1)] = '|';

		line[2*(nx+2*bfx)-1] = '\0';
		line2[2*(nx+2*bfx)-1] = '\0';

		iprintf("%s", line2);
		iprintf("%s", line);
	}
	free(line);
	free(line2);
}

void print_vertex_full(vertex v) {
	char line1[7], line2[7], line3[7], line4[7], line5[7];
	int i;

	for(i = 0; i < 6; i++) {
		line1[i] = ' ';
		line2[i] = ' ';
		line3[i] = ' ';
		line4[i] = ' ';
		line5[i] = ' ';
	}

	line1[6] = '\0';
	line2[6] = '\0';
	line3[6] = '\0';
	line4[6] = '\0';
	line5[6] = '\0';

	if (v->ind == 1) line3[2] = '*';

	if (v->e[EDGE_S] != NULL) {
		if (v->e[EDGE_S]->inter_x != NOT_SET) line4[2] = '+';
		else line4[2] = '|';

		if (v->e[EDGE_S]->v[0]->ind == 1) line5[2] = '*';

		if (v->e[EDGE_S]->v[0]->e[EDGE_W]->inter_x != NOT_SET) line5[1] = '+';
		else line5[1] = '-';

		if (v->e[EDGE_S]->v[0]->e[EDGE_E]->inter_x != NOT_SET) line5[3] = '+';
		else line5[3] = '-';

		if (v->e[EDGE_S]->v[0]->e[EDGE_NE] != NULL) {
			if (v->e[EDGE_S]->v[0]->e[EDGE_NE]->inter_x != NOT_SET) line4[3] = 'X';
			else line4[3] = '/';
		}

	}

	if (v->e[EDGE_SW] != NULL) {
		if (v->e[EDGE_SW]->inter_x != NOT_SET) line4[1] = 'X';
		else line4[1] = '/';

		if (v->e[EDGE_SW]->v[0]->ind == 1) line5[0] = '*';
	}

	if (v->e[EDGE_W] != NULL) {
		if (v->e[EDGE_W]->inter_x != NOT_SET) line3[1] = '+';
		else line3[1] = '-';

		if (v->e[EDGE_W]->v[0]->ind == 1) line3[0] = '*';

		if (v->e[EDGE_W]->v[0]->e[EDGE_S] != NULL) {
			if (v->e[EDGE_W]->v[0]->e[EDGE_S]->inter_x != NOT_SET) line4[0] = '+';
			else line4[0] = '|';
		}

		if (v->e[EDGE_W]->v[0]->e[EDGE_N] != NULL) {
			if (v->e[EDGE_W]->v[0]->e[EDGE_N]->inter_x != NOT_SET) line2[0] = '+';
			else line2[0] = '|';

			if (v->e[EDGE_W]->v[0]->e[EDGE_N]->v[1]->ind == 1) line1[0] = '*';
		}

		if (v->e[EDGE_W]->v[0]->e[EDGE_NE] != NULL) {
			if (v->e[EDGE_W]->v[0]->e[EDGE_NE]->inter_x != NOT_SET) line2[1] = 'X';
			else line2[1] = '/';

		}
	}

	if (v->e[EDGE_N] != NULL) {
		if (v->e[EDGE_N]->inter_x != NOT_SET) line2[2] = '+';
		else line2[2] = '|';

		if (v->e[EDGE_N]->v[1]->ind == 1) line1[2] = '*';

		if (v->e[EDGE_N]->v[1]->e[EDGE_W]->inter_x != NOT_SET) line1[1] = '+';
		else line1[1] = '-';

		if (v->e[EDGE_N]->v[1]->e[EDGE_E]->inter_x != NOT_SET) line1[3] = '+';
		else line1[3] = '-';
	}

	if (v->e[EDGE_NE] != NULL) {
		if (v->e[EDGE_NE]->inter_x != NOT_SET) line2[3] = 'X';
		else line2[3] = '/';

		if (v->e[EDGE_NE]->v[1]->ind == 1) line1[4] = '*';
	}

	if (v->e[EDGE_E] != NULL) {
		if (v->e[EDGE_E]->inter_x != NOT_SET) line3[3] = '+';
		else line3[3] = '-';

		if (v->e[EDGE_E]->v[1]->ind == 1) line3[4] = '*';

		if (v->e[EDGE_E]->v[1]->e[EDGE_S] != NULL) {
			if (v->e[EDGE_E]->v[1]->e[EDGE_S]->inter_x != NOT_SET) line4[4] = '+';
			else line4[4] = '|';

			if (v->e[EDGE_E]->v[1]->e[EDGE_S]->v[0]->ind == 1) line5[4] = '*';

		}

		if (v->e[EDGE_E]->v[1]->e[EDGE_N] != NULL) {
			if (v->e[EDGE_E]->v[1]->e[EDGE_N]->inter_x != NOT_SET) line2[4] = '+';
			else line2[4] = '|';

		}
	}

	iprintf("%s", line1);
	iprintf("%s", line2);
	iprintf("%s", line3);
	iprintf("%s", line4);
	iprintf("%s", line5);
}

vertex get_init_vertex(mesh g, double x, double y, int sign) {
	double index_aprox; 
	IDL_LONG i, j; 

	index_aprox = x/g->hx;
	if (double_eq(index_aprox, ceil(index_aprox), CMP_EPSILON)) 
		i = (IDL_LONG)ceil(index_aprox)+g->bfx;
	else
		i = (IDL_LONG)floor(index_aprox)+g->bfx;

	index_aprox = y/g->hy;
	if (sign == 1) {
		if (double_eq(index_aprox, ceil(index_aprox), CMP_EPSILON))
			j = (IDL_LONG)ceil(index_aprox)+g->bfy;
		else
			j = (IDL_LONG)floor(index_aprox)+g->bfy;
	}

	else { // if (sign == -1)
		if (double_eq(index_aprox, floor(index_aprox), CMP_EPSILON))
			j = (IDL_LONG)floor(index_aprox)+g->bfy;
		else
			j = (IDL_LONG)ceil(index_aprox)+g->bfy;
	}


	return &(g->vlist[i+(g->nx+2*g->bfx)*j]);
}

vertex get_end_vertex(mesh g, double x, double y, int sign) {
	double index_aprox; 
	IDL_LONG i, j; 

	index_aprox = x/g->hx;
	if (double_eq(index_aprox, floor(index_aprox), CMP_EPSILON)) 
		i = (IDL_LONG)floor(index_aprox)+g->bfx;
	else
		i = (IDL_LONG)ceil(index_aprox)+g->bfx;

	index_aprox = y/g->hy;
	if (sign == 1) {
		if (double_eq(index_aprox, floor(index_aprox), CMP_EPSILON))
			j = (IDL_LONG)floor(index_aprox)+g->bfy;
		else
			j = (IDL_LONG)ceil(index_aprox)+g->bfy;
	}

	else { // if (sign == -1)
		if (double_eq(index_aprox, ceil(index_aprox), CMP_EPSILON))
			j = (IDL_LONG)ceil(index_aprox)+g->bfy;
		else
			j = (IDL_LONG)floor(index_aprox)+g->bfy;
	}

	return &(g->vlist[i+(g->nx+2*g->bfx)*j]);
}

// Floating point number comparison functions

int double_eq(double x, double y, double epsilon) {
	return fabs(x-y) <= epsilon * double_max(1.0, double_max(fabs(x), fabs(y)));
}

double double_max(double x, double y) {
	if (x <= y) return y;
	else return x;
}

