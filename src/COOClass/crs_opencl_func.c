
#include <stdio.h>
#include <stdlib.h>
#define CL_TARGET_OPENCL_VERSION 120
//#define CL_USE_DEPRECATED_OPENCL_1_2_APIS

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif


#define MEM_SIZE (128)
#define MAX_SOURCE_SIZE (0x100000)

void opencl_crs_matvec(int num_row, int num_col_idx, int row_ptr[num_row], int col_idx[num_col_idx],
    double val[num_row], double old_vector[num_row], double new_vector[num_row]
    ){
    
    cl_device_id    device_id = NULL;
    cl_context        context = NULL;
    cl_command_queue command_queue = NULL;


    // (1) Check the number of memobj
    cl_mem mem_row_ptr = NULL;
    cl_mem mem_col_idx = NULL;
    cl_mem mem_val = NULL;
    cl_mem mem_old_vector = NULL;
    cl_mem mem_new_vector = NULL;
  
    cl_program program = NULL;
    cl_kernel kernel = NULL;
    cl_platform_id platform_id = NULL;
    cl_uint ret_num_devices;
    cl_uint ret_num_platforms;
    cl_int ret;

    // (3) Check kernel.cl
    FILE *fp;
    //char fileName[] = "./inc/gpu_dot_product.cl";
    char fileName[] = "gpu_crs_matvec.cl";
    //char fileName[] = "gpu_dot_product.cl";
    char *source_str;
    size_t source_size;


    /* Load the source code containing the kernel*/
    fp = fopen(fileName, "r");
    if (!fp) {
      fprintf(stderr, "Failed to load kernel.\n");
      exit(1);
    }
    source_str = (char*)malloc(MAX_SOURCE_SIZE);
    source_size = fread(source_str, 1, MAX_SOURCE_SIZE, fp);
    fclose(fp);

    /* Get Platform and Device Info */
    ret = clGetPlatformIDs(1, &platform_id, &ret_num_platforms);
    ret = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_DEFAULT, 1, &device_id, &ret_num_devices);


    /* Create OpenCL context */
    context = clCreateContext(NULL, 1, &device_id, NULL, NULL, &ret);
    if (ret != CL_SUCCESS) {
      printf("Error: Failed to CreateContext! %d\n", ret);
      exit(1);
    }

    /* Create Command Queue */
    #ifdef CL_VERSION_2_0
    command_queue = clCreateCommandQueueWithProperties(context, device_id, 0, &ret);
    #else
    command_queue = clCreateCommandQueue(context, device_id, 0, &ret);
    #endif
    if (ret != CL_SUCCESS) {
      printf("Error: Failed to CreateCommandQueueWithProperties! %d\n", ret);
      exit(1);
    }

    // (4) Check memory object (number and name)
    /* Create Memory Buffer */
    //memobj = clCreateBuffer(context, CL_MEM_READ_WRITE,MEM_SIZE * sizeof(double), NULL, &ret);
    //memobj = clCreateBuffer(context, CL_MEM_READ_WRITE,MEM_SIZE * sizeof(char), NULL, &ret);
    mem_row_ptr = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , num_row*sizeof(int), row_ptr, &ret);
    mem_col_idx = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , num_col_idx*sizeof(int), col_idx, &ret);
    
    mem_val        = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , num_col_idx*sizeof(double), val, &ret);
    mem_old_vector = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , num_row*sizeof(double), old_vector, &ret);
    mem_new_vector = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , num_row*sizeof(double), new_vector, &ret);
    
    if (ret != CL_SUCCESS) {
      printf("Error: Failed to CreateBuffer! %d\n", ret);
      exit(1);
    }  



    /* Create Kernel Program from the source */
    //program = clCreateProgramWithSource(context, 1, (const char **)&source_str,
    //                                    (const size_t *)&source_size, &ret);
    program = clCreateProgramWithSource(context, 1, (const char **)&source_str,
                                       (const size_t *)&source_size, &ret);
    if (ret != CL_SUCCESS) {
      printf("Error: Failed to create program with source! %d\n", ret);
      exit(1);
    }  

    /* Build Kernel Program */
    ret = clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);
    if (ret != CL_SUCCESS) {
      size_t len;
      char buffer[2048];
      printf("Error: Failed to build program executable!\n");
      clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG, sizeof(buffer), buffer, &len);
      printf("%s\n", buffer);
      exit(1);
    }

}


int main(){
    int *row_ptr ;
    int col_idx[4] ;
    double val[4]  ;
    double old_vector[2] ;
    double new_vector[2] ;
    int num_row;
    int num_col_idx;
    
    row_ptr = (int *)malloc(sizeof(int)*3 );
    row_ptr[0] = 1;
    row_ptr[1] = 3;
    row_ptr[2] = 5;

    
    col_idx[0] = 1; col_idx[1] = 2; 
    col_idx[2] = 1; col_idx[3] = 2; 

    val[0] = 1.0; val[1] = 2.0; 
    val[2] = 3.0; val[3] = 4.0; 

    old_vector[0] = 1.0; old_vector[1] = 2.0; 
    new_vector[0] = 0.0; new_vector[1] = 0.0; 

    num_row = 2;
    num_col_idx = 4;
    opencl_crs_matvec(num_row,num_col_idx,row_ptr,col_idx,val, old_vector,new_vector);
    return 0;
}