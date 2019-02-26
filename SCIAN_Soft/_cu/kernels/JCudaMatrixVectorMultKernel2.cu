extern "C"
__global__ void multiply(int sizeB, int max, double** A, double* B, double* C, double* Displacement)
{
    int tid = threadIdx.x + blockIdx.x * blockDim.x;
    if(tid < sizeB){
        double sum = 0.0;
        int index_neighbor;
        
        for(int i = 0; i < max; i++) {
            index_neighbor = (int)A[2*tid][i];
            sum = sum + A[2*tid + 1][i]*B[index_neighbor];
        }
        C[tid] = sum;

        Displacement[tid] = B[tid] - C[tid];
    }
}