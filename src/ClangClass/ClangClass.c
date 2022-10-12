/*
 * C function that takes a double array, and cubes each element.
 */

#define CL_TARGET_OPENCL_VERSION 220
#define MEM_SIZE (128)
#define MAX_SOURCE_SIZE (0x100000)
#define CL_USE_DEPRECATED_OPENCL_1_2_APIS

#include <stddef.h>
#include <stdio.h>

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

// http://www.e-em.co.jp/tutorial/chap9.htm

void real_cube(double x[],size_t n)
{
  size_t i;
  for (i=0; i<n; i++) x[i] = x[i]*x[i]*x[i];
}

void c_dot_product(double x[],double y[],size_t n,double ret[])
{
  size_t i;
  ret[0] = 0.0;
  for (i=0; i<n; i++){
    ret[0] = ret[0] + x[i]*y[i];
  } ;
}


void c_opencl_dot_product()
{
    // https://us.fixstars.com/products/opencl/book/OpenCLProgrammingBook/first-opencl-program/
    cl_device_id device_id = NULL;
    cl_context context = NULL;
    cl_command_queue command_queue = NULL;
    cl_mem memobj = NULL;
    cl_program program = NULL;
    cl_kernel kernel = NULL;
    cl_platform_id platform_id = NULL;
    cl_uint ret_num_devices;
    cl_uint ret_num_platforms;
    cl_int ret;


    char string[MEM_SIZE];
    
    FILE *fp;
    char fileName[] = "src/ClangClass/hello.cl";
    char *source_str;
    size_t source_size;
    
    /* Load the source code containing the kernel*/
    fp = fopen(fileName, "r");
    if (!fp) {
    fprintf(stderr, "Failed to load kernel.\n");
    exit(1);
    }else{
        fprintf(stderr, "Succeed to load kernel.\n");
    }
    source_str = (char*)malloc(MAX_SOURCE_SIZE);
    source_size = fread(source_str, 1, MAX_SOURCE_SIZE, fp);
    fclose(fp);
 
    /* Get Platform and Device Info */
    //ret = clGetPclatformIDs(1, &platform_id, &ret_num_platforms);
    ret = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_DEFAULT, 1, &device_id, &ret_num_devices);
    
    /* Create OpenCL context */
    context = clCreateContext(NULL, 1, &device_id, NULL, NULL, &ret);
    
    /* Create Command Queue */
    command_queue = clCreateCommandQueue(context, device_id, 0, &ret);
    
    /* Create Memory Buffer */
    memobj = clCreateBuffer(context, CL_MEM_READ_WRITE,MEM_SIZE * sizeof(char), NULL, &ret);
    
    /* Create Kernel Program from the source */
    program = clCreateProgramWithSource(context, 1, (const char **)&source_str,
    (const size_t *)&source_size, &ret);
    
    /* Build Kernel Program */
    ret = clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);
    
    /* Create OpenCL Kernel */
    kernel = clCreateKernel(program, "hello", &ret);
    
    /* Set OpenCL Kernel Parameters */
    ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&memobj);
    
    /* Execute OpenCL Kernel */
    ret = clEnqueueTask(command_queue, kernel, 0, NULL,NULL);
    
    /* Copy results from the memory buffer */
    ret = clEnqueueReadBuffer(command_queue, memobj, CL_TRUE, 0,
    MEM_SIZE * sizeof(char),string, 0, NULL, NULL);
    
    /* Display Result */
    puts(string);
    
    /* Finalization */
    ret = clFlush(command_queue);
    ret = clFinish(command_queue);
    ret = clReleaseKernel(kernel);
    ret = clReleaseProgram(program);
    ret = clReleaseMemObject(memobj);
    ret = clReleaseCommandQueue(command_queue);
    ret = clReleaseContext(context);
    
    free(source_str);
}

void c_opencl_matmul_crs(int *n, int *n_col_idx, int col_idx[],  int row_idx[],  double val[],
     double vec[],  double ret_vec[] )
{
    // https://us.fixstars.com/products/opencl/book/OpenCLProgrammingBook/first-opencl-program/
    cl_device_id device_id = NULL;
    cl_context context = NULL;
    cl_command_queue command_queue = NULL;

    cl_mem memobj_n = NULL;
    cl_mem memobj_n_col_idx = NULL;
    cl_mem memobj_col_idx = NULL;
    cl_mem memobj_row_idx = NULL;
    cl_mem memobj_val = NULL;
    cl_mem memobj_vec = NULL;
    cl_mem memobj_ret_vec = NULL;

    cl_program program = NULL;
    cl_kernel kernel = NULL;
    cl_platform_id platform_id = NULL;
    cl_uint ret_num_devices;
    cl_uint ret_num_platforms;
    cl_int ret;


    char string[MEM_SIZE];
    
    FILE *fp;
    char fileName[] = "src/ClangClass/c_opencl_matmul_crs.cl";
    char *source_str;
    size_t source_size;
    
    /* Load the source code containing the kernel*/
    fp = fopen(fileName, "r");
    if (!fp) {
    fprintf(stderr, "Failed to load kernel.\n");
    exit(1);
    }else{
        fprintf(stderr, "Succeed to load kernel.\n");
    }
    source_str = (char*)malloc(MAX_SOURCE_SIZE);
    source_size = fread(source_str, 1, MAX_SOURCE_SIZE, fp);
    fclose(fp);
 
    /* Get Platform and Device Info */
    //ret = clGetPclatformIDs(1, &platform_id, &ret_num_platforms);
    ret = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_DEFAULT, 1, &device_id, &ret_num_devices);
    
    /* Create OpenCL context */
    context = clCreateContext(NULL, 1, &device_id, NULL, NULL, &ret);
    
    /* Create Command Queue */
    command_queue = clCreateCommandQueue(context, device_id, 0, &ret);
    
    /* Create Memory Buffer */
    //memobj = clCreateBuffer(context, CL_MEM_READ_WRITE,MEM_SIZE * sizeof(char), NULL, &ret);
    memobj_n = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , sizeof(int), n, &ret); 
    memobj_n_col_idx = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , sizeof(int), n_col_idx, &ret); 
    memobj_col_idx = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , *n_col_idx*sizeof(int), col_idx, &ret);
    memobj_row_idx = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , *n*sizeof(int), row_idx, &ret);
    memobj_val = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , *n_col_idx*sizeof(double), val, &ret);
    memobj_vec = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , *n*sizeof(double), vec, &ret);
    memobj_ret_vec = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , *n*sizeof(double), ret_vec, &ret);

    /* Create Kernel Program from the source */
    program = clCreateProgramWithSource(context, 1, (const char **)&source_str,
    (const size_t *)&source_size, &ret);
    
    /* Build Kernel Program */
    ret = clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);
    
    /* Create OpenCL Kernel */
    kernel = clCreateKernel(program, "c_opencl_matmul_crs", &ret);
    
    /* Set OpenCL Kernel Parameters */
    ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&memobj_n);
    ret = clSetKernelArg(kernel, 1, sizeof(cl_mem), (void *)&memobj_n_col_idx);
    ret = clSetKernelArg(kernel, 2, sizeof(cl_mem), (void *)&memobj_col_idx);
    ret = clSetKernelArg(kernel, 3, sizeof(cl_mem), (void *)&memobj_row_idx);
    ret = clSetKernelArg(kernel, 4, sizeof(cl_mem), (void *)&memobj_val);
    ret = clSetKernelArg(kernel, 5, sizeof(cl_mem), (void *)&memobj_vec);
    ret = clSetKernelArg(kernel, 6, sizeof(cl_mem), (void *)&memobj_ret_vec);
    
    /* Execute OpenCL Kernel */
    ret = clEnqueueTask(command_queue, kernel, 0, NULL,NULL);
    
    /* Copy results from the memory buffer */
    ret = clEnqueueReadBuffer(command_queue, memobj_ret_vec, CL_TRUE, 0,
    *n*sizeof(double),ret_vec, 0, NULL, NULL);
    
    /* Display Result */
    //printf("dot product by OpenCL is : %f\n", ret_vec[0]);
    
    /* Finalization */
    ret = clFlush(command_queue);
    ret = clFinish(command_queue);
    ret = clReleaseKernel(kernel);
    ret = clReleaseProgram(program);

    ret = clReleaseMemObject( memobj_n );
    ret = clReleaseMemObject( memobj_n_col_idx );
    ret = clReleaseMemObject( memobj_col_idx );
    ret = clReleaseMemObject( memobj_row_idx );
    ret = clReleaseMemObject( memobj_val );
    ret = clReleaseMemObject( memobj_vec );
    ret = clReleaseMemObject( memobj_ret_vec );

    ret = clReleaseCommandQueue(command_queue);
    ret = clReleaseContext(context);
    
    free(source_str);
}