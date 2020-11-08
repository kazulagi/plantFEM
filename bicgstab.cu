#include <stdio.h>
#include <stdlib.h>
#include <cuda.h>
#include <cuda_runtime.h>
#define N 1024
#define tol 0.000001f
#define itr_tol 100

__global__ void vector_add(float *out, float *a, float *b, int n) {
    int i = blockIdx.x*blockDim.x + threadIdx.x;
    out[i] = a[i] + 2.00*b[i];
    //out[i] = a[i] + 2.00*b[i] + out[i];
    //out[i] = a[i] + 2.00*b[i] + 2.00*out[i];
    
}


__global__ void get_residual(float *r, float *a, float *b,float *x, int n) {
    // r = b - Ax
    int i = blockIdx.x*blockDim.x + threadIdx.x;
    int ij= 0;
    if(i >= n){
        return;
    }

    for(int j = 0; j < n; j++){
        ij = n*i + j;
        //printf("ij=%i\n",ij);
        r[i] += - a[ij]*x[j];
        //printf("r[%i] = %f, a[i]=%f,b[i]=%f,x[i]=%f \n",i,r[i],a[ij],b[i],x[j]);
    }
    r[i] += b[i];
    //printf("%i, %i, %i, %i, %i,%i\n",blockDim.x, blockDim.y, blockDim.z,gridDim.x,gridDim.y,gridDim.z);
    //printf("%i, %i, %i, %i, %i,%i\n",blockIdx.x, threadIdx.x,i);
    
}


__global__ void cuda_matmul(float *r, float *a,float *x, int n) {
    // r = b - Ax
    int i = blockIdx.x*blockDim.x + threadIdx.x;
    int ij= 0;
    if(i >= n){
        return;
    }

    for(int j = 0; j < n; j++){
        ij = n*i + j;
        //printf("ij=%i\n",ij);
        r[i] += a[ij]*x[j];
    }
    //printf("%i, %i, %i, %i, %i,%i\n",blockDim.x, blockDim.y, blockDim.z,gridDim.x,gridDim.y,gridDim.z);
    //printf("%i, %i, %i, %i, %i,%i\n",blockIdx.x, threadIdx.x,i);
    
}


__global__ void cuda_dot_product(float *r1, float *r2, float *rr,int n) {
    // r = b - Ax
    int i = blockIdx.x*blockDim.x + threadIdx.x;
    int j;
    
    rr[i]=0.0f;

    for(j=0; j<n; j++){
        rr[i] += r1[j]*r2[j];
    }
}


__global__ void cuda_e_residual_bicg(float *e, float *r, float *alp, float *y,int n) {
    // r = b - Ax
    int i = blockIdx.x*blockDim.x + threadIdx.x;
    
    
    e[i] = r[i] - alp[i]*y[i];
    
}

__global__ void cuda_add_x_alp_p_c3_e(float *x, float *alp, float *p, float *c3, float *e, int n) {
    // r = b - Ax
    int i = blockIdx.x*blockDim.x + threadIdx.x;
    
    x[i] = x[i] + alp[i]*p[i] + c3[i]*e[i];
    
}


__global__ void cuda_update_p_bicg(float *p, float *r, float *bet, float *c3, float *y, int n) {
    // r = b - Ax
    int i = blockIdx.x*blockDim.x + threadIdx.x;
    
    p[i] = r[i] + bet[i]*(p[i] - c3[i]*y[i]);
    
}

__global__ void cuda_divide_a_by_b(float *ret, float *a, float *b,int n) {
    // r = b - Ax
    int i = blockIdx.x*blockDim.x + threadIdx.x;
    
    ret[i] = a[i]/b[i];

}

__global__ void cuda_divide_a_by_b_by_c(float *ret, float *a, float *b,float *c,int n) {
    // r = b - Ax
    int i = blockIdx.x*blockDim.x + threadIdx.x;
    
    ret[i] = a[i]/b[i]/c[i];

}


__global__ void cuda_check_float(float *x){
    int i = blockIdx.x*blockDim.x + threadIdx.x;
    printf("x[%i] : %f\n", i,x[i]);
}


__global__ void cuda_copy_vector(float *x, float *copy){
    int i = blockIdx.x*blockDim.x + threadIdx.x;
    copy[i] = x[i];
}



int main(){
    float *a, *b, *x, *out;
    float *r, *r0, *p, *y, *e, *v, *er,*er0;
    float *alp, *bet, *c1,*c2, *c3, *ev, *vv, *rr,*init_rr;

    float *d_a, *d_b, *d_x, *d_out;
    float *d_r, *d_r0, *d_p, *d_y, *d_e, *d_v, *d_er;
    float *d_alp, *d_bet, *d_c1,*d_c2, *d_c3, *d_ev, *d_vv, *d_rr, *d_er0,*d_init_rr;

    // Allocate memory
    a = (float*)malloc(sizeof(float) * N*N);
    b = (float*)malloc(sizeof(float) * N);
    x = (float*)malloc(sizeof(float) * N);
    r = (float*)malloc(sizeof(float) * N);
    r0= (float*)malloc(sizeof(float) * N);
    p = (float*)malloc(sizeof(float) * N);
    y = (float*)malloc(sizeof(float) * N);
    e = (float*)malloc(sizeof(float) * N);
    v = (float*)malloc(sizeof(float) * N);
    
    alp = (float*)malloc(sizeof(float) * N);
    bet = (float*)malloc(sizeof(float) * N);
    c1  = (float*)malloc(sizeof(float) * N);
    c2 = (float*)malloc(sizeof(float) * N);
    c3 = (float*)malloc(sizeof(float) * N);
    ev = (float*)malloc(sizeof(float) * N);
    vv = (float*)malloc(sizeof(float) * N);
    rr= (float*)malloc(sizeof(float) * N);
    er= (float*)malloc(sizeof(float) * N);
    er0= (float*)malloc(sizeof(float) * N);
    init_rr= (float*)malloc(sizeof(float) * N);

    out = (float*)malloc(sizeof(float) * N);
    

    // Initialize array
    int itr;
    itr=0;
    for(int i = 0; i < N*N; i++){
        a[i] = 0.0f;
        if(i==itr*N+itr){
            //if(itr<20){
            //    printf("%i, %i\n",i,itr);    
            //}
            a[i] = 1.0f*float(itr);
            itr = itr + 1;
        }
        
    }
    // a = unit matrix
    
    for(int i = 0; i < N; i++){
        b[i] = 2.0f;
    }

    for(int i = 0; i < N; i++){
        x[i] = 0.0f;
    }
    for(int i = 0; i < N; i++){
        r[i] = 0.0f;
    }
    for(int i = 0; i < N; i++){
        r0[i]= 0.0f;
    }
    for(int i = 0; i < N; i++){
        p[i] = 0.0f;
    }
    for(int i = 0; i < N; i++){
        y[i] = 0.0f;
    }
    for(int i = 0; i < N; i++){
        e[i] = 0.0f;
    }
    for(int i = 0; i < N; i++){
        v[i] = 0.0f;
    }

    for(int i = 0; i < N; i++){
        alp[i] = 0.0f;
    }
    for(int i = 0; i < N; i++){
        bet[i] = 0.0f;
    }
    for(int i = 0; i < N; i++){
        c1[i] = 0.0f;
    }
    for(int i = 0; i < N; i++){
        c2[i] = 0.0f;
    }
    for(int i = 0; i < N; i++){
        c3[i] = 0.0f;
    }
    for(int i = 0; i < N; i++){
        ev[i] = 0.0f;
    }
    for(int i = 0; i < N; i++){
        vv[i] = 0.0f;
    }
    for(int i = 0; i < N; i++){
        rr[i] = 0.0f;
    }
    for(int i = 0; i < N; i++){
        er[i] = 0.0f;
    }
    for(int i = 0; i < N; i++){
        er0[i] = 0.0f;
    }
    for(int i = 0; i < N; i++){
        init_rr[i] = 0.0f;
    }
    

    for(int i = 0; i < N; i++){
        out[i] = 2.0f;
    }

    // Allocate device memory
    cudaMalloc((void**)&d_a, sizeof(float) * N*N);
    cudaMalloc((void**)&d_b, sizeof(float) * N);
    cudaMalloc((void**)&d_x, sizeof(float) * N);
    cudaMalloc((void**)&d_out, sizeof(float) * N);
    cudaMalloc((void**)&d_r, sizeof(float) * N);
    cudaMalloc((void**)&d_r0, sizeof(float) * N);
    cudaMalloc((void**)&d_p, sizeof(float) * N);
    cudaMalloc((void**)&d_y, sizeof(float) * N);
    cudaMalloc((void**)&d_e, sizeof(float) * N);
    cudaMalloc((void**)&d_v, sizeof(float) * N);

    cudaMalloc((void**)&d_alp, sizeof(float) * N);
    cudaMalloc((void**)&d_bet, sizeof(float) * N);
    cudaMalloc((void**)&d_c1, sizeof(float) * N);
    cudaMalloc((void**)&d_c2, sizeof(float) * N);
    cudaMalloc((void**)&d_c3, sizeof(float) * N);
    cudaMalloc((void**)&d_ev, sizeof(float) * N);
    cudaMalloc((void**)&d_vv, sizeof(float) * N);
    cudaMalloc((void**)&d_rr, sizeof(float) * N);
    cudaMalloc((void**)&d_er0, sizeof(float) * N);
    cudaMalloc((void**)&d_er, sizeof(float) * N);
    cudaMalloc((void**)&d_init_rr, sizeof(float) * N);

    
    // Transfer data from host to device memory
    cudaMemcpy(d_a, a, sizeof(float) * N*N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_b, b, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_b, b, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_x, x, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_out, out, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_r, r, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_r0, r0, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_p, p, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_y, y, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_e, e, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_v, v, sizeof(float) * N, cudaMemcpyHostToDevice);

    cudaMemcpy(d_alp, alp, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_bet, bet, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_c1, c1, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_c2, c2, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_c3, c3, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_ev, ev, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_vv, vv, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_rr, rr, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_er0, er0, sizeof(float) * N, cudaMemcpyHostToDevice);
    cudaMemcpy(d_init_rr, init_rr, sizeof(float) * N, cudaMemcpyHostToDevice);

    // Setting thread
    int blocksize = 512;
    dim3 block (blocksize, 1, 1);
    dim3 grid (N / block.x, 1, 1);




    // Executing kernel for BiCGSTAB
    // a=unit matrix
    // b=2.0
    // x=0.0
    // x=0.0
    get_residual<<<grid, block>>>(d_r, d_a, d_b, d_x, N);
    

    //cudaDeviceSynchronize();
    cuda_dot_product<<<grid, block>>>(d_r,d_r, d_c1, N);
    //r[:] = 2
    //d_c1 = 2*2*1024=4096
    
    //cuda_check_float<<<grid, block>>>(d_c1);
    
    cuda_copy_vector<<<grid, block>>>(d_c1, d_init_rr);

    //c1=init_rr=4096

    //printf("%f",d_c1[0]);
    cudaMemcpy(c1, d_c1, sizeof(float) * N, cudaMemcpyDeviceToHost);
    
    if(c1[0]>float(tol) ){

        
        cuda_copy_vector<<<grid, block>>>(d_r, d_p);

        cuda_copy_vector<<<grid, block>>>(d_r, d_r0);
        
        for(int k=0; k<itr_tol;k++){
         
            cuda_dot_product<<<grid, block>>>(d_r, d_r0, d_c1, N);
            //d_c1=4*1024=4096

            cuda_matmul<<<grid, block>>>(d_y, d_a, d_p, N);
            //d_y = d_p = 2

            cuda_dot_product<<<grid, block>>>(d_r0, d_y, d_c2, N);
            //d_c2=4*1024=4096

            cuda_divide_a_by_b<<<grid, block>>>(d_alp, d_c1, d_c2, N);
            // alp = 1.0

            cuda_e_residual_bicg<<<grid, block>>>(d_e, d_r, d_alp, d_y, N);

            cuda_matmul<<<grid, block>>>(d_v, d_a, d_e, N);

            cuda_dot_product<<<grid, block>>>(d_e, d_v, d_ev, N);
            cuda_dot_product<<<grid, block>>>(d_v, d_v, d_vv, N);
            
            cudaMemcpy(vv, d_vv, sizeof(float) * N, cudaMemcpyDeviceToHost);

            if(vv[0]==0.0f ){
                printf("cuda_BiCGSTAB devide by zero\n");
                return 1;
            }else{
                cuda_divide_a_by_b<<<grid, block>>>(d_c3, d_ev, d_vv, N);
                cuda_add_x_alp_p_c3_e<<<grid, block>>>(d_x, d_alp, d_p, d_c3, d_e, N);
                cuda_e_residual_bicg<<<grid, block>>>(d_r, d_e, d_c3, d_v, N); 
                cuda_dot_product<<<grid, block>>>(d_r, d_r, d_rr, N);
                cudaMemcpy(rr, d_rr, sizeof(float) * N, cudaMemcpyDeviceToHost);
                
                if(rr[0]<tol){
                    break;
                }else{
                    cuda_dot_product<<<grid, block>>>(d_r0, d_r, d_c1, N);
                    cuda_divide_a_by_b_by_c<<<grid, block>>>(d_bet, d_c1, d_c2, d_c3, N);
                    cuda_update_p_bicg<<<grid, block>>>(d_p, d_r, d_bet, d_c3,d_y, N);
                }
            }
            cudaMemcpy(rr, d_rr, sizeof(float) * N, cudaMemcpyDeviceToHost);
            printf("itr = %i, residual =%f\n",k,rr[0]);
            if(k+1==itr_tol){
                printf("ERROR :: bicgstab did not converge.\n");
                return 1;
            }
           
        }
        

    }





    //vector_add<<<grid,block>>>(d_out, d_a, d_b, N);
    
    // Transfer data back to host memory
    cudaMemcpy(out, d_out, sizeof(float) * N, cudaMemcpyDeviceToHost);
    cudaMemcpy(rr, d_rr, sizeof(float) * N, cudaMemcpyDeviceToHost);
    cudaMemcpy(x, d_x, sizeof(float) * N, cudaMemcpyDeviceToHost);
    cudaMemcpy(alp, d_alp, sizeof(float) * N, cudaMemcpyDeviceToHost);


    //printf("%f\n", out[0]);
    //printf("alp[0] : %f\n", alp[0]);
    //printf("rr[0] : %f\n", rr[0]);
    //printf("x[0] : %f\n", x[0]);
    
    //printf("%i, %i, %i, %i, %i,%i\n",block.x, block.y, block.z,grid.x,grid.y,grid.z);
    // Deallocate device memory
    cudaFree(d_a);
    cudaFree(d_b);
    cudaFree(d_x);
    cudaFree(d_out);
    
    cudaFree(d_r);
    cudaFree(d_r0);
    cudaFree(d_p);
    cudaFree(d_y);
    cudaFree(d_e);
    cudaFree(d_v);
    cudaFree(d_er);
    cudaFree(d_er0);
    
    cudaFree(d_alp);
    cudaFree(d_bet);
    cudaFree(d_c1);
    cudaFree(d_c2);
    cudaFree(d_c3);
    cudaFree(d_ev);
    cudaFree(d_vv);
    cudaFree(d_rr);
    cudaFree(d_init_rr);

    
    // Deallocate host memory
    free(a);
    free(b);
    free(x);
    free(out);
    
    free(r);
    free(r0);
    free(p);
    free(y);
    free(e);
    free(v);
    free(er);
    free(er0);
    
    free(alp);
    free(bet);
    free(c1);
    free(c2);
    free(c3);
    free(ev);
    free(vv);
    free(rr);
    free(init_rr);
}
