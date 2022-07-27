//#include <stdio.h>
//#include <CL/cl.h>
//#include <stdlib.h>
//#include <time.h>
//
//

//#define MAX_SOURCE_SIZE 0x100000
////#define ARAAY_SIZE 10000
////#define ITEM_SIZE 200

//
//int initopencl()
//{
//    // プラットフォーム取得
//    cl_uint platformNumber = 0;
//    cl_platform_id platformIds[8];
//    clGetPlatformIDs(8, platformIds, &platformNumber);
//
//    char string[256];
//    cl_device_type type;
//    cl_uint value;
//    size_t sizes[3];
//    cl_ulong ulvalue;
//    for (int i = 0; i < platformNumber; i++)
//    {
//        printf("platform idx : %d\n", i);
//        cl_platform_id platform = platformIds[i];
//        clGetPlatformInfo(platform, CL_PLATFORM_VENDOR, 256, string, NULL);
//        printf("platform vendor : %s\n", string);
//        clGetPlatformInfo(platform, CL_PLATFORM_NAME, 256, string, NULL);
//        printf("platform name : %s\n", string);
//        clGetPlatformInfo(platform, CL_PLATFORM_VERSION, 256, string, NULL);
//        printf("platform version : %s\n", string);
//
//        // デバイス取得
//        cl_uint deviceNumber = 0;
//        cl_device_id deviceIds[8];
//        clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, 8, deviceIds, &deviceNumber);
//        for (int j = 0; j < deviceNumber; j++)
//        {
//            printf("    device idx : %d\n", j);
//            cl_device_id device = deviceIds[j];
//            clGetDeviceInfo(device, CL_DEVICE_NAME, 256, string, NULL);
//            printf("    device name : %s\n", string);
//            clGetDeviceInfo(device, CL_DEVICE_TYPE, sizeof(cl_device_type), &type, NULL);
//            if (type == CL_DEVICE_TYPE_CPU) printf("    device type : CPU\n");
//            if (type == CL_DEVICE_TYPE_GPU) printf("    device type : GPU\n");
//            if (type == CL_DEVICE_TYPE_ACCELERATOR) printf("    device type : ACCELERATOR\n");
//            clGetDeviceInfo(device, CL_DEVICE_MAX_COMPUTE_UNITS, sizeof(cl_uint), &value, NULL);
//            printf("    device max compute units : %d\n", value);
//            clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_ITEM_SIZES, sizeof(size_t) * 3, sizes, NULL);
//            printf("    device max work item sizes : [%ld][%ld][%ld]\n", sizes[0], sizes[1], sizes[2]);
//            clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof(cl_uint), &value, NULL);
//            printf("    device max work group size : %d\n", value);
//            clGetDeviceInfo(device, CL_DEVICE_MAX_MEM_ALLOC_SIZE, sizeof(cl_ulong), &ulvalue, NULL);
//            printf("    device max mem alloc size : %ld\n", ulvalue);
//            clGetDeviceInfo(device, CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE, sizeof(cl_ulong), &ulvalue, NULL);
//            printf("    device max constant buffer size : %ld\n", ulvalue);
//        }
//    }
//
//}
//
//
//double exec_kernel(double *pa[], double *pa2[] ){
//
//
//    int ARAAY_SIZE; 
//    int ITEM_SIZE; 
//    ARAAY_SIZE=sizeof(*pa2);
//    ITEM_SIZE =sizeof(*pa);
//    cl_device_id device_id = NULL;
//    cl_context context = NULL;
//    cl_command_queue command_queue = NULL;
//    cl_mem memobj = NULL;
//    cl_mem memobjb = NULL;
//    cl_mem memobjout = NULL;
//    cl_mem memor = NULL;
//    cl_mem memosize = NULL;
//    cl_program program = NULL;
//    cl_kernel kernel = NULL;
//    cl_platform_id platform_id = NULL;
//    cl_uint ret_num_devices, ret_num_platforms;
//    cl_int ret;
//
//    double p;
//    double *x ;
//    double *y;
//
//    double *out;
//
//
//    // read kernel code
//    FILE *fp;
//    char fileName[] = "./kernel.cl";
//    char *source_str;
//    size_t source_size;
//
//    x=(double *)malloc( ARAAY_SIZE* sizeof(double));
//    y=(double *)malloc( ARAAY_SIZE* sizeof(double));
//    int size = ARAAY_SIZE;
//    out=(double *)malloc(ARAAY_SIZE*ARAAY_SIZE * sizeof(double));
//
//    for(int i =0; i<ARAAY_SIZE;i++){
//        x[i] = (double)rand() / 32767.0;
//        y[i] = (double)rand() / 32767.0;
//        //x[i] = *pa[i];
//        //y[i] = *pa2[i];
//    }
//
//
//
//      // read kernel code
//      fp = fopen(fileName, "r");
//      if(!fp) {
//        exit(1);
//      }
//      source_str = (char*)malloc(MAX_SOURCE_SIZE);
//      source_size = fread(source_str, 1, MAX_SOURCE_SIZE, fp);
//      fclose(fp);
//
//      // get device check
//      ret = clGetPlatformIDs(1, &platform_id, &ret_num_platforms);
//      ret = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_DEFAULT, 1, &device_id, &ret_num_devices);
//
//      // make context
//      context = clCreateContext(NULL, 1, &device_id, NULL, NULL, &ret);
//
//      // make comand queue
//      //command_queue = clCreateCommandQueue(context, device_id, 0, &ret);
//
//      // make kernel code Obj
//      program = clCreateProgramWithSource(context, 1, (const char **)&source_str, (const size_t *)&source_size, &ret);
//
//      // compile kernel code
//      ret = clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);
//
//      // set func
//      kernel = clCreateKernel(program, "kernel", &ret);
//
//      // make buffer
//      memobj = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(double)*ARAAY_SIZE, NULL, &ret);
//      memobjb = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(double)*ARAAY_SIZE, NULL, &ret);
//      memobjout = clCreateBuffer(context, CL_MEM_READ_WRITE, ARAAY_SIZE*ARAAY_SIZE * sizeof(double), NULL, &ret);
//      memosize = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int), NULL, &ret);
//
//      // write buffer
//      ret = clEnqueueWriteBuffer(command_queue, memobj, CL_TRUE, 0, sizeof(double)*ARAAY_SIZE, x, 0, NULL, NULL);
//      ret = clEnqueueWriteBuffer(command_queue, memobjb, CL_TRUE, 0, sizeof(double)*ARAAY_SIZE, y, 0, NULL, NULL);
//      ret = clEnqueueWriteBuffer(command_queue, memobjout, CL_TRUE, 0, sizeof(double)*ARAAY_SIZE*ARAAY_SIZE, out, 0, NULL, NULL);
//      ret = clEnqueueWriteBuffer(command_queue, memosize, CL_TRUE, 0, sizeof(int), &size, 0, NULL, NULL);
//
//      // set args
//      ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&memobj);
//      ret = clSetKernelArg(kernel, 1, sizeof(cl_mem), (void *)&memobjb);
//      ret = clSetKernelArg(kernel, 2, sizeof(cl_mem), (void *)&memobjout);
//      ret = clSetKernelArg(kernel, 3, sizeof(cl_mem), (void *)&memosize);
//
//      size_t local_item_size = ITEM_SIZE;
//      size_t global_item_size = ITEM_SIZE;
//
//      ret=clEnqueueNDRangeKernel(command_queue,kernel, 1, NULL,&global_item_size, &local_item_size, 0, NULL, NULL);
//
//      ret = clEnqueueReadBuffer(command_queue, memobjout, CL_TRUE, 0, sizeof(double) * size * size, out, 0, NULL, NULL);
//
//
//      ret = clFlush(command_queue);
//      ret = clFinish(command_queue);
//      ret = clReleaseKernel(kernel);
//      ret = clReleaseProgram(program);
//      ret = clReleaseMemObject(memobj);
//      ret = clReleaseCommandQueue(command_queue);
//      ret = clReleaseContext(context);
//
//      free(source_str);
//      free(out);
//      free(x);
//      free(y);
//
//      return 0;
//    }


#include <stdio.h>
#include <stdlib.h>

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

#define MEM_SIZE (128)
#define MAX_SOURCE_SIZE (0x100000)

int hello()
{
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
  char fileName[] = "./inc/hello.cl";
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

  /* Create Command Queue */
  command_queue = clCreateCommandQueueWithProperties(context, device_id, 0, &ret);

  /* Create Memory Buffer */
  memobj = clCreateBuffer(context, CL_MEM_READ_WRITE,MEM_SIZE * sizeof(char), NULL, &ret);
  //memobj = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(double), NULL, &ret);

  /* Create Kernel Program from the source */
  //program = clCreateProgramWithSource(context, 1, (const char **)&source_str,
  //                                    (const size_t *)&source_size, &ret);

  program = clCreateProgramWithSource(context, 1, (const char **)&source_str,
                                      (const size_t *)&source_size, &ret);
  /* Build Kernel Program */
  ret = clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);

  /* Create OpenCL Kernel */
  kernel = clCreateKernel(program, "hello", &ret);

  /* Set OpenCL Kernel Parameters */
  ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&memobj);
  //ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), sizeof(double) );

  /* Execute OpenCL Kernel */
  size_t globalWorkSize[] = {1};
  size_t localWorkSize[] = {1};
  ret = clEnqueueNDRangeKernel(command_queue, kernel, 1, NULL, globalWorkSize, localWorkSize, 0, NULL, NULL);

  /* Copy results from the memory buffer */
  ret = clEnqueueReadBuffer(command_queue, memobj, CL_TRUE, 0,
                            MEM_SIZE * sizeof(char),string, 0, NULL, NULL);
  //ret = clEnqueueReadBuffer(command_queue, memobj, CL_TRUE, 0,
  //                          MEM_SIZE * sizeof(double),val, 0, NULL, NULL);

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

  return 0;
}
















double opencl_add_kernel(double x1, double y1,int petot)
{
  cl_device_id device_id = NULL;
  cl_context context = NULL;
  cl_command_queue command_queue = NULL;
  
  
  cl_mem memobj = NULL;
  cl_mem memobj2 = NULL;
  cl_mem memobjout = NULL;


  cl_program program = NULL;
  cl_kernel kernel = NULL;
  cl_platform_id platform_id = NULL;
  cl_uint ret_num_devices;
  cl_uint ret_num_platforms;
  cl_int ret;

  double x;
  double y;
  double z;
  double *val_in;
  double *val_in2;
  double *val_out;
  int *gws;
  int *lws;

  x=x1;
  y=y1;
  val_in=&x;
  val_in2=&y;
  val_out=&z;



  printf("Imported %f %f %f\n",x1,y1,z);


  char string[MEM_SIZE];
  double val;

  FILE *fp;
  char fileName[] = "./inc/add.cl";
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

  /* Create Memory Buffer */
  //memobj = clCreateBuffer(context, CL_MEM_READ_WRITE,MEM_SIZE * sizeof(double), NULL, &ret);
  //memobj = clCreateBuffer(context, CL_MEM_READ_WRITE,MEM_SIZE * sizeof(char), NULL, &ret);
  memobj = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , sizeof(double*), val_in, &ret);
  memobj2 = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , sizeof(double*), val_in2, &ret);
  memobjout = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR , sizeof(double*), val_out, &ret);
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
  kernel = clCreateKernel(program, "add", &ret);
  if (ret != CL_SUCCESS) {
    printf("Error: Failed to create kernel! %d\n", ret);
    exit(1);
  }
  /* Set OpenCL Kernel Parameters */
  //ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&memobj);
  //ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), (double *)&memobj);

  //ret = clSetKernelArg(kernel, 0, 2, memobj );


  ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&memobj);
  ret = clSetKernelArg(kernel, 1, sizeof(cl_mem), (void *)&memobj2);
  ret = clSetKernelArg(kernel, 2, sizeof(cl_mem), (void *)&memobjout);
  
  if (ret != CL_SUCCESS) {
    printf("Error: Failed to SetKernelArg! %d\n", ret);
    exit(1);
  }


  printf("Sent value kernel x is : %f\n",*val_in);
  printf("Sent value kernel y is : %f\n",*val_in2);
  printf("Sent value kernel z = x + y is : %f\n",*val_out);


  /* Execute OpenCL Kernel */
  size_t globalWorkSize[] = {petot};
  size_t localWorkSize []=  {petot};
  ret = clEnqueueNDRangeKernel(command_queue, kernel, 1, NULL, globalWorkSize, localWorkSize, 0, NULL, NULL);

  if (ret != CL_SUCCESS) {
    printf("Error: Failed to execute kernel! %d\n", ret);
    exit(1);
  }
 
/* Execute OpenCL Kernel */
  //ret = clEnqueueNDRangeKernel(command_queue, kernel, workDim, NULL, globalWorkSize, localWorkSize, 0, NULL, NULL);
  

  //size_t *gws=8;
  //size_t *lws=8;
  //ret = clEnqueueNDRangeKernel(command_queue, kernel, 1, NULL, gws, lws, 0, NULL, NULL);
  
  /* Copy results from the memory buffer */
  //ret = clEnqueueReadBuffer(command_queue, memobj, CL_TRUE, 0,
  //                          MEM_SIZE * sizeof(char),string, 0, NULL, NULL);

  //ret = clEnqueueReadBuffer(command_queue, memobj, CL_TRUE, 0,
  //                          8,val_in, 0, NULL, NULL);
  
  /* Copy results from the memory buffer */
  //ret = clEnqueueReadBuffer(command_queue, memobj, CL_TRUE, 0,
  //                          MEM_SIZE ,string, 0, NULL, NULL);
  //ret = clEnqueueReadBuffer(command_queue, memobj, CL_TRUE, 0,
  //                        8,val_in, 0, NULL, NULL);
  //ret = clEnqueueReadBuffer(command_queue, memobj, CL_TRUE, 0,
  //                        8,val_in2, 0, NULL, NULL);
  ret = clEnqueueReadBuffer(command_queue, memobjout, CL_TRUE, 0,
                          8,val_out, 0, NULL, NULL);
  if (ret != CL_SUCCESS) {
    printf("Error: Failed to read output array! %d\n", ret);
    exit(1);
  }


  printf("Returned value kernel x is : %f\n",*val_in);
  printf("Returned value kernel y is : %f\n",*val_in2);
  printf("Returned value kernel z = x + y is : %f\n",*val_out);
  

  /* Copy results from the memory buffer */
  //ret = clEnqueueReadBuffer(command_queue, matrixRMemObj, CL_TRUE, 0, matrixRMemSize, matrixR, 0, NULL, NULL);
  
  



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

  return *val_out;
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
