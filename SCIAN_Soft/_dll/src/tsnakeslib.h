/*** Constants ***/

#define NOT_SET -1e100


/* Modes of the deformation step */
enum{EXPAND_ONLY, CONTRACT_ONLY, FREE_MOVE};

/* Values of the indicator function of the ROI */
enum{ROI_OUT, ROI_IN};

/* Orientation of the edges relative to a node */
enum{EDGE_S, EDGE_SW, EDGE_W, EDGE_N, EDGE_NE, EDGE_E};

/* Orientation of the faces relative to a node */
enum{FACE_NE1, FACE_NE2, FACE_SE, FACE_SW1, FACE_SW2, FACE_NW};

/* Type of face */
enum{FACE_ROTATED, FACE_NORMAL};

/* Orientation of the faces relative to a edge */
enum{FACE_LEFT, FACE_RIGHT};
enum{FACE_UP, FACE_DOWN};

/* Type of Edge */
enum{EDGE_H, EDGE_V, EDGE_D};


#define DEBUG 0
#define CMP_EPSILON 1e-12
#define NEAR_EPSILON 1e-7


/* Mesh Structure Definitions */
typedef struct Vertex *vertex;
typedef struct Edge   *edge;
typedef struct Face   *face;
typedef struct Mesh   *mesh;

struct Vertex {
	double x, y;
	edge e[6]; 
	face f[6];
	int ind;
	int is_queued;
	int was_queued;
};

struct Edge{
	vertex v[2];
	face f[2];
	double inter_x, inter_y;
	int inter_sign;
	int type;
};


struct Face{
	vertex v[3];
	edge e[3];
	int is_border;
	int type;
};

struct Mesh{
	vertex vlist;
	edge   elist;
	face   flist;
	double hx, hy, m, x_max, y_max;
	IDL_LONG nx, ny, nvertices, nedges, nfaces, bfx, bfy;
};


// Mesh Data Structure Method
mesh new_mesh(IDL_LONG, IDL_LONG, IDL_LONG, IDL_LONG, IDL_LONG, IDL_LONG);
void destroy_mesh(mesh);
vertex new_vertex(double, double, int);
void print_mesh(mesh);
vertex get_init_vertex(mesh, double, double, int);
vertex get_end_vertex(mesh, double, double, int);
vertex other_side(vertex, int);
int is_interior(vertex);
int is_exterior(vertex);
int is_still_valid(vertex);
int is_singleton(vertex);


// Queue Structure Definitions

typedef struct QueueNode *qnode;
typedef struct Queue *queue;

struct QueueNode {
	qnode next;
	void *data;
};

struct Queue {
	qnode first;
	qnode last;
};

// Snake Structure Definitions
typedef struct Snake *snake;

struct Snake {
	double *x;
	double *y;
	IDL_LONG npts;

};

snake new_snake(IDL_LONG);
void destroy_snake(snake);

typedef struct SnakeBox *sbox;
struct SnakeBox {
	queue snakes;
	int nsnakes;
};

sbox new_sbox();
void push_snake(sbox, snake);
snake pop_snake(sbox);
int sbox_is_empty(sbox);
void destroy_sbox(sbox);

queue new_queue();
int is_empty(queue);
void destroy_queue(queue);
void queue_push(queue, void *);
void *queue_pop(queue);
void queue_remove(queue, void *);




// Tsnake Evolution Functions
IDL_VPTR tsnake_iterations(int, IDL_VPTR *);

void deformation_step(
		IDL_INT *, 
		snake,
		IDL_LONG, IDL_LONG,
		IDL_INT, double, double,
		IDL_INT, IDL_INT,
		IDL_INT, IDL_INT,
		double, double, double, double,
		double *, double *, 
		int);

double inflation_factor(
		double, 
		IDL_INT, IDL_INT,
		IDL_INT, IDL_INT
);

void phase_1(snake, mesh, queue);
void phase_1_out(snake, mesh, queue);
void iprint_phase_1_segfault(
		double, int,
		double, double,
		double, double, 
		double, double,
		double, double,
		double, double,
		IDL_LONG, IDL_LONG
);
		
int phase_2(mesh, queue, int);
int phase_2_out(mesh, queue, int);
void clean_intersections(mesh);
void clean_process_status(mesh);
void clean_indicator(mesh);
void fill_interior(mesh, vertex, int);
void fill_exterior(mesh, vertex, int);
void init_indicator(double *, double *, int, mesh);
void collect_snakes(mesh, queue, sbox);
void collect_snakes_out(mesh, queue, queue, queue, sbox);
int next_edge_direction_on_vertex_change(int);
int next_edge_direction_on_vertex_change_out(int);

void reinitialize(mesh, queue);


// Display Functions
void iprintf(char *, ...); // Requires the static variable char msg[len]
void dprintf(char *, ...);
void print_edge(edge, char *);
void print_indicator(mesh);
void iprint_indicator(mesh);
void print_indicator_full(mesh);
void iprint_indicator_full(mesh);
void print_vertex_full(vertex);

// IDL Load Functions and Other
int IDL_Load(void);


int double_eq(double, double, double);
double double_max(double, double);

void discard_local_self_intersections(sbox);
void discard_too_close_points(sbox);

