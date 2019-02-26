extern "C"
__global__ void surface_area(int size_faces, int* face_verts, double* coord_verts, double* partial_area)
{

    int tid = threadIdx.x + blockIdx.x * blockDim.x;

    if (tid < size_faces) {
        int a_index = face_verts[3*tid + 0];
        int b_index = face_verts[3*tid + 1];
        int c_index = face_verts[3*tid + 2];

        double ax = coord_verts[3*a_index + 0];
        double ay = coord_verts[3*a_index + 1];
        double az = coord_verts[3*a_index + 2];

        double bx = coord_verts[3*b_index + 0];
        double by = coord_verts[3*b_index + 1];
        double bz = coord_verts[3*b_index + 2];

        double cx = coord_verts[3*c_index + 0];
        double cy = coord_verts[3*c_index + 1];
        double cz = coord_verts[3*c_index + 2];

        double s1 = ((ax * (by - cy))
                - (bx * (ay - cy))
                + (cx * (ay - by)));

        double s2 = ((ay * (bz - cz))
                - (by * (az - cz))
                + (cy * (az - bz)));

        double s3 = ((az * (bx - cx))
                - (bz * (ax - cx))
                + (cz * (ax - bx)));

        double face_area = 0.5 * sqrt((s1 * s1) + (s2 * s2) + (s3 * s3));

        partial_area[tid] = face_area;
    }
}

extern "C"
__global__ void multiply(int sizeB, int max, double w1, int* A_indexs, double* A_values, double* B, double* C, double* Displacement)
{
    int tid = threadIdx.x + blockIdx.x * blockDim.x;
    if(tid < sizeB){
        double sum = 0.0;
        int index_neighbor;
        
        for(int i = 0; i < max; i++) {
            index_neighbor = A_indexs[max*tid + i];
            sum = sum + A_values[max*tid + i]*B[index_neighbor];
        }
        sum = sum + w1*B[tid];
        C[tid] = sum;

        Displacement[tid] = B[tid] - C[tid];
    }
}