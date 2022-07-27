
#include <stdio.h>
#include <stdlib.h>
#define CL_TARGET_OPENCL_VERSION 120

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif


#define MEM_SIZE (128)
#define MAX_SOURCE_SIZE (0x100000)

void opencl_crs_matvec(int *row_ptr[], int *col_idx[], double *val[], 
    double *old_vector[], double *new_vecotor[],int num_row, int num_col_idx){
    
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
    char fileName[] = "gpu_dot_product.cl";
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




}

double opencl_dot_product_kernel(double *a[], double *b[],int *dim_num,int petot)
{
  cl_device_id device_id = NULL;
  cl_context context = NULL;
  cl_command_queue command_queue = NULL;
  
  // (1) Check the number of memobj
  cl_mem memobj = NULL;
  cl_mem memobj2 = NULL;
  cl_mem memobjout = NULL;
  cl_mem memobjdn = NULL;
  


  cl_program program = NULL;
  cl_kernel kernel = NULL;
  cl_platform_id platform_id = NULL;
  cl_uint ret_num_devices;
  cl_uint ret_num_platforms;
  cl_int ret;

  // (2) Check variables
  double dp;
  double *dpp;

  dpp=&dp;
  
  // (3) Check kernel.cl
  FILE *fp;
  char fileName[] = "./inc/gpu_dot_product.cl";
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
  command_queue = clCreateCommandQueue(context, device_id, 0, &ret);
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
  memobj = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , *dim_num*sizeof(double), *a, &ret);
  memobj2 = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , *dim_num*sizeof(double), *b, &ret);
  memobjout = clCreateBuffer(context,  CL_MEM_COPY_HOST_PTR  , sizeof(double*), dpp, &ret);
  memobjdn = clCreateBuffer(context,  CL_MEM_COPY_HOST_PTR  , sizeof(int), dim_num, &ret);
  
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

  /* Create OpenCL Kernel */
  kernel = clCreateKernel(program, "gpu_dot_product", &ret);
  if (ret != CL_SUCCESS) {
    printf("Error: Failed to create kernel! %d\n", ret);
    exit(1);
  }
  /* Set OpenCL Kernel Parameters */
  ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&memobj);
  ret = clSetKernelArg(kernel, 1, sizeof(cl_mem), (void *)&memobj2);
  ret = clSetKernelArg(kernel, 2, sizeof(cl_mem), (void *)&memobjout);
  ret = clSetKernelArg(kernel, 3, sizeof(cl_mem), (void *)&memobjdn);

  if (ret != CL_SUCCESS) {
    printf("Error: Failed to SetKernelArg! %d\n", ret);
    exit(1);
  }

  /* Execute OpenCL Kernel */
  size_t globalWorkSize[] = {petot};
  size_t localWorkSize []=  {petot};
  ret = clEnqueueNDRangeKernel(command_queue, kernel, 1, NULL, globalWorkSize, localWorkSize, 0, NULL, NULL);

  if (ret != CL_SUCCESS) {
    printf("Error: Failed to execute kernel! %d\n", ret);
    exit(1);
  }


  /* Copy results from the memory buffer */
  ret = clEnqueueReadBuffer(command_queue, memobjout, CL_TRUE, 0,
                          sizeof(double*),dpp, 0, NULL, NULL);
  if (ret != CL_SUCCESS) {
    printf("Error: Failed to read output array! %d\n", ret);
    exit(1);
  }

  printf("dot product by OpenCL is : %f\n",*dpp);



  /* Display Result */
  //puts(string);
  
  
  /* Finalization */
  ret = clFlush(command_queue);
  ret = clFinish(command_queue);
  ret = clReleaseKernel(kernel);
  ret = clReleaseProgram(program);
  ret = clReleaseMemObject(memobj);
  ret = clReleaseCommandQueue(command_queue);
  ret = clReleaseContext(context);

  free(source_str);

  return *dpp;
}




float opencl_dot_product_kernel_f(float *a[], float *b[],int *dim_num,int petot)
{
  cl_device_id device_id = NULL;
  cl_context context = NULL;
  cl_command_queue command_queue = NULL;
  
  // (1) Check the number of memobj
  cl_mem memobj = NULL;
  cl_mem memobj2 = NULL;
  cl_mem memobjout = NULL;
  cl_mem memobjdn = NULL;
  


  cl_program program = NULL;
  cl_kernel kernel = NULL;
  cl_platform_id platform_id = NULL;
  cl_uint ret_num_devices;
  cl_uint ret_num_platforms;
  cl_int ret;

  // (2) Check variables
  float dp;
  float *dpp;

  dpp=&dp;
  
  // (3) Check kernel.cl
  FILE *fp;
  char fileName[] = "./inc/gpu_dot_product_f.cl";
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
  command_queue = clCreateCommandQueue(context, device_id, 0, &ret);
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
  memobj = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , *dim_num*sizeof(float), *a, &ret);
  memobj2 = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , *dim_num*sizeof(float), *b, &ret);
  memobjout = clCreateBuffer(context,  CL_MEM_COPY_HOST_PTR  , sizeof(float*), dpp, &ret);
  memobjdn = clCreateBuffer(context,  CL_MEM_COPY_HOST_PTR  , sizeof(int), dim_num, &ret);
  
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

  /* Create OpenCL Kernel */
  kernel = clCreateKernel(program, "gpu_dot_product_f", &ret);
  if (ret != CL_SUCCESS) {
    printf("Error: Failed to create kernel! %d\n", ret);
    exit(1);
  }
  /* Set OpenCL Kernel Parameters */
  ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&memobj);
  ret = clSetKernelArg(kernel, 1, sizeof(cl_mem), (void *)&memobj2);
  ret = clSetKernelArg(kernel, 2, sizeof(cl_mem), (void *)&memobjout);
  ret = clSetKernelArg(kernel, 3, sizeof(cl_mem), (void *)&memobjdn);

  if (ret != CL_SUCCESS) {
    printf("Error: Failed to SetKernelArg! %d\n", ret);
    exit(1);
  }

  /* Execute OpenCL Kernel */
  size_t globalWorkSize[] = {petot};
  size_t localWorkSize []=  {petot};
  ret = clEnqueueNDRangeKernel(command_queue, kernel, 1, NULL, globalWorkSize, localWorkSize, 0, NULL, NULL);

  if (ret != CL_SUCCESS) {
    printf("Error: Failed to execute kernel! %d\n", ret);
    exit(1);
  }


  /* Copy results from the memory buffer */
  ret = clEnqueueReadBuffer(command_queue, memobjout, CL_TRUE, 0,
                          sizeof(float*),dpp, 0, NULL, NULL);
  if (ret != CL_SUCCESS) {
    printf("Error: Failed to read output array! %d\n", ret);
    exit(1);
  }

  printf("dot product by OpenCL is : %f\n",*dpp);



  /* Display Result */
  //puts(string);
  
  
  /* Finalization */
  ret = clFlush(command_queue);
  ret = clFinish(command_queue);
  ret = clReleaseKernel(kernel);
  ret = clReleaseProgram(program);
  ret = clReleaseMemObject(memobj);
  ret = clReleaseCommandQueue(command_queue);
  ret = clReleaseContext(context);

  free(source_str);

  return *dpp;
}



 double opencl_dotproduct(double *pa[], double *pa2[],int dim_in)
 {
  double *a;
  double *b;
  double x;
  double y;
  double z;
  double val;
  double dp;
  int m;
  int petot;

  petot = 100;
  //z=opencl_add_kernel(x,y,170);

  dp=opencl_dot_product_kernel(pa,pa2,&dim_in,petot);
  printf("dot_product is : %f",dp);
  return dp;
  //a = (double*) malloc(sizeof(double)*dim_in);
  //a=*pa;


  //b = (double*) malloc(sizeof(double)*dim_in);
  //b=*pa2;

  //printf("Result is %f\n",z);
  //val=0.0;
  //for (m=0;m<dim_in;m++)
  //{
  //  val=val+(a[m])*(b[m]);
  //}
  //printf("%f\n",val);
//
  //return val;

 }

 float opencl_dotproduct_f(float *pa[], float *pa2[],int dim_in)
 {
  float *a;
  float *b;
  float x;
  float y;
  float z;
  float val;
  float dp;
  int m;
  int petot;

  petot = 100;
  //z=opencl_add_kernel(x,y,170);
  

  dp=opencl_dot_product_kernel_f(pa,pa2,&dim_in,petot);
  printf("dot_product is : %f",dp);
  return dp;
  //a = (double*) malloc(sizeof(double)*dim_in);
  //a=*pa;


  //b = (double*) malloc(sizeof(double)*dim_in);
  //b=*pa2;

  //printf("Result is %f\n",z);
  //val=0.0;
  //for (m=0;m<dim_in;m++)
  //{
  //  val=val+(a[m])*(b[m]);
  //}
  //printf("%f\n",val);
//
  //return val;

 }

int main(){
    return 0;
}