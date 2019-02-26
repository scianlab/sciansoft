extern "C"
__global__ void multiply(int colsA, int sizeB, double** A, double* B, double* C, double* Displacement)
{
    int tid = threadIdx.x + blockIdx.x * blockDim.x;
    if(tid < sizeB){
        double sum = 0.0;
        C[tid] = 0;
        for(int i = 0; i < sizeB; i++) {
            sum = sum + A[tid][i]*B[i];
        }
        C[tid] = sum;

        Displacement[tid] = B[tid] - C[tid];
    }
}