#include <stdio.h>
#include <stdlib.h>
#include <cuda.h>
#include <cuda_runtime.h>
#define N 500000000



__global__ void vector_add(float *out, float *a, float *b, int n) {
    int my_number = blockIdx.x+blockIdx.y+threadIdx.x+threadIdx.y+threadIdx.z;
    for(int i = 0; i < n; i++){
        out[i] = a[i] + 2.00*b[i];
        //out[i] = a[i] + 2.00*b[i] + out[i];
        //out[i] = a[i] + 2.00*b[i] + 2.00*out[i];
    }
}



int main(){
    float *a, *b, *out;
    float *d_a, *d_b, *d_out;

    // Allocate memory
    a = (float*)malloc(sizeof(float) * N);
    b = (float*)malloc(sizeof(float) * N);
    out = (float*)malloc(sizeof(float) * N);
    
    // Initialize array
    for(int i = 0; i < N; i++){
        a[i] = 1.0f; b[i] = 2.0f;
    }
    
    // Allocate device memory
    cudaMalloc((void**)&d_a, sizeof(float) * N);
    cudaMalloc((void**)&d_b, sizeof(float) * N);
    cudaMalloc((void**)&d_out, sizeof(float) * N);

    // Transfer data from host to device memory
    cudaMemcpy(d_a, a, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_b, b, sizeof(float) * N, cudaMemcpyHostToDevice);
    
    // Executing kernel 
    dim3 grid(10,10);
    dim3 block(10,10,10);
    vector_add<<<grid, block>>>(d_out, d_a, d_b, N);
    
    // Transfer data back to host memory
    cudaMemcpy(out, d_out, sizeof(float) * N, cudaMemcpyDeviceToHost);
    printf("%f\n", out[0]);
    
    // Deallocate device memory
    cudaFree(d_a);
    cudaFree(d_b);
    cudaFree(d_out);
    
    // Deallocate host memory
    free(a);
    free(b);
    free(out);
}
