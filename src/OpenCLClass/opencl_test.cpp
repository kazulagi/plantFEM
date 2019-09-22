/*--------------------
  opencl_test.cpp
  --------------------*/

#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <numeric>
#include <sys/time.h>

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

#define MAX_SOURCE_SIZE (0x100000)

int main(void){
  const char filename[] = "./kernel.cl";
  std::size_t const n = 10;

  cl_platform_id platform_id = NULL;
  cl_device_id device_id = NULL;
  cl_context context = NULL;
  cl_command_queue command_queue = NULL;
  cl_program program = NULL;
  cl_kernel kernel = NULL;
  cl_uint ret_num_devices;
  cl_uint ret_num_platforms;
  cl_int ret;

  cl_mem x_dev, y_dev, z_dev;//for device memory
  float* x = (float*)malloc(sizeof(float) * n);
  float* y = (float*)malloc(sizeof(float) * n);
  float* z = (float*)malloc(sizeof(float) * n);

  srand( static_cast<unsigned>(time(NULL)) );
  for(std::size_t i=0; i<n; ++i)
    x[i] = static_cast<float>( rand() ) / RAND_MAX;
  for(std::size_t i=0; i<n; ++i)
    y[i] = static_cast<float>( rand() ) / RAND_MAX;
  memset(z, 0x00, sizeof(float) * n);

  //Read a Kernel code
  FILE* fp = fopen(filename, "r");
  if(!fp){
    std::cerr << "Failed to load kernel" << std::endl;
    return -1;
  }
  char* source_str = (char*)malloc(MAX_SOURCE_SIZE);
  std::size_t source_size = fread(source_str, 1, MAX_SOURCE_SIZE, fp);
  fclose(fp);

  //Get the platforms
  ret = clGetPlatformIDs(1, &platform_id, &ret_num_platforms);

  //Get the device
  ret = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_DEFAULT, 1, 
		       &device_id, &ret_num_devices);
  
  //Create context
  context = clCreateContext(NULL, 1, &device_id, NULL, NULL, &ret);
  
  //Create Command Queue
  command_queue = clCreateCommandQueue(context, device_id, 0, &ret);

  //Create Program Object  
  program = clCreateProgramWithSource(context, 1, (const char**)&source_str,
				      (const size_t*)&source_size, &ret);

  //Compile of kernel's source code
  ret = clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);

  //Create Kernel object
  kernel = clCreateKernel(program, "vec_add", &ret);

  //Create memory object
  x_dev = clCreateBuffer(context, CL_MEM_READ_WRITE, 
			 n * sizeof(float), NULL, &ret);

  y_dev = clCreateBuffer(context, CL_MEM_READ_WRITE, 
			 n * sizeof(float), NULL, &ret);

  z_dev = clCreateBuffer(context, CL_MEM_READ_WRITE, 
			 n * sizeof(float), NULL, &ret);

  //memory copy host to device
  ret = clEnqueueWriteBuffer(command_queue, x_dev, CL_TRUE,
			     0, n * sizeof(float), x,
			     0, NULL, NULL);
  ret = clEnqueueWriteBuffer(command_queue, y_dev, CL_TRUE,
			     0, n * sizeof(float), y,
			     0, NULL, NULL);
  ret = clEnqueueWriteBuffer(command_queue, z_dev, CL_TRUE,
			     0, n * sizeof(float), z,
			     0, NULL, NULL);

  //Set args for kernel object 
  ret = clSetKernelArg(kernel, 0, sizeof(std::size_t), &n);
  ret += clSetKernelArg(kernel, 1, sizeof(cl_mem), &z_dev);
  ret += clSetKernelArg(kernel, 2, sizeof(cl_mem), &x_dev);
  ret += clSetKernelArg(kernel, 3, sizeof(cl_mem), &y_dev);

  size_t global_work_size[3] = {n, 0, 0};
  size_t local_work_size[3] = {n, 0, 0};

  //Execute Kernel Program
  ret = clEnqueueNDRangeKernel(command_queue, kernel, 1, NULL,
			       global_work_size, local_work_size,
			       0, NULL, NULL);

  //memory copy device to host
  ret = clEnqueueReadBuffer(command_queue, z_dev, CL_TRUE, 0,
			    n * sizeof(float), z, 0,
			    NULL, NULL);

  for(std::size_t i=0; i<n; ++i){
    std::cout << x[i] << " " << y[i] << " = " << z[i]<< std::endl;
  }

  //memory free
  ret = clFlush(command_queue);
  ret = clFinish(command_queue);
  ret = clReleaseKernel(kernel);
  ret = clReleaseProgram(program);
  ret = clReleaseCommandQueue(command_queue);
  ret = clReleaseContext(context);

  ret = clReleaseMemObject(x_dev);  
  ret = clReleaseMemObject(y_dev);  
  ret = clReleaseMemObject(z_dev);

  free(x);
  free(y);
  free(z);

  free(source_str);

  return 0;
}