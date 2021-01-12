#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <OpenCL/opencl.h>

#define MAX_SOURCE_SIZE 0x100000
#define ARAAY_SIZE 10000
#define ITEM_SIZE 200

int main(void) {
  srand(time(NULL));

  cl_device_id device_id = NULL;
  cl_context context = NULL;
  cl_command_queue command_queue = NULL;
  cl_mem memobj = NULL;
  cl_mem memobjb = NULL;
  cl_mem memobjout = NULL;
  cl_mem memor = NULL;
  cl_mem memosize = NULL;
  cl_program program = NULL;
  cl_kernel kernel = NULL;
  cl_platform_id platform_id = NULL;
  cl_uint ret_num_devices, ret_num_platforms;
  cl_int ret;

  float p;
  float *x ;
  float *y;

  float *out;

  // read kernel code
  FILE *fp;
  char fileName[] = "./test.cl";
  char *source_str;
  size_t source_size;
  x=(float *)malloc( ARAAY_SIZE* sizeof(float));
   y=(float *)malloc( ARAAY_SIZE* sizeof(float));
   int size = ARAAY_SIZE;
  out=(float *)malloc(ARAAY_SIZE*ARAAY_SIZE * sizeof(float));

  for(int i =0; i<ARAAY_SIZE;i++){
      x[i] = (float)rand() / 32767.0;
      y[i] = (float)rand() / 32767.0;
  }

  clock_t start,end;
  printf("start\n");
  start = clock();

  // read kernel code
  fp = fopen(fileName, "r");
  if(!fp) {
    exit(1);
  }
  source_str = (char*)malloc(MAX_SOURCE_SIZE);
  source_size = fread(source_str, 1, MAX_SOURCE_SIZE, fp);
  fclose(fp);

  // get device check
  ret = clGetPlatformIDs(1, &platform_id, &ret_num_platforms);
  ret = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_DEFAULT, 1, &device_id, &ret_num_devices);

  // make context
  context = clCreateContext(NULL, 1, &device_id, NULL, NULL, &ret);

  // make comand queue
  command_queue = clCreateCommandQueue(context, device_id, 0, &ret);

  // make kernel code Obj
  program = clCreateProgramWithSource(context, 1, (const char **)&source_str, (const size_t *)&source_size, &ret);

  // compile kernel code
  ret = clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);

  // set func
  kernel = clCreateKernel(program, "test", &ret);

  // make buffer
  memobj = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(float)*ARAAY_SIZE, NULL, &ret);
  memobjb = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(float)*ARAAY_SIZE, NULL, &ret);
  memobjout = clCreateBuffer(context, CL_MEM_READ_WRITE, ARAAY_SIZE*ARAAY_SIZE * sizeof(float), NULL, &ret);
  memosize = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int), NULL, &ret);

  // write buffer
  ret = clEnqueueWriteBuffer(command_queue, memobj, CL_TRUE, 0, sizeof(float)*ARAAY_SIZE, x, 0, NULL, NULL);
  ret = clEnqueueWriteBuffer(command_queue, memobjb, CL_TRUE, 0, sizeof(float)*ARAAY_SIZE, y, 0, NULL, NULL);
  ret = clEnqueueWriteBuffer(command_queue, memobjout, CL_TRUE, 0, sizeof(float)*ARAAY_SIZE*ARAAY_SIZE, out, 0, NULL, NULL);
  ret = clEnqueueWriteBuffer(command_queue, memosize, CL_TRUE, 0, sizeof(int), &size, 0, NULL, NULL);

  // set args
  ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&memobj);
  ret = clSetKernelArg(kernel, 1, sizeof(cl_mem), (void *)&memobjb);
  ret = clSetKernelArg(kernel, 2, sizeof(cl_mem), (void *)&memobjout);
  ret = clSetKernelArg(kernel, 3, sizeof(cl_mem), (void *)&memosize);

  size_t local_item_size = ITEM_SIZE;
  size_t global_item_size = ITEM_SIZE;

  ret=clEnqueueNDRangeKernel(command_queue,kernel, 1, NULL,&global_item_size, &local_item_size, 0, NULL, NULL);

  ret = clEnqueueReadBuffer(command_queue, memobjout, CL_TRUE, 0, sizeof(float) * size * size, out, 0, NULL, NULL);

  end = clock();
  printf("END\n");
  printf("%lf秒かかりました\n",(double)(end-start)/CLOCKS_PER_SEC);

  ret = clFlush(command_queue);
  ret = clFinish(command_queue);
  ret = clReleaseKernel(kernel);
  ret = clReleaseProgram(program);
  ret = clReleaseMemObject(memobj);
  ret = clReleaseCommandQueue(command_queue);
  ret = clReleaseContext(context);

  free(source_str);
  free(out);
  free(x);
  free(y);

  return 0;
}


__kernel void test(__global float *x ,__global float *y
                    ,__global float *out,__global int *size) {
    size_t gx = get_global_id(0);
    int i, j, k, x, y;
    k = (*size) * (*size) / get_global_size(0);
    for(i = 0; i < k; i++){
        j = i + k * gx;
        a = j / (*size);
        b = j % (*size);
        out[j] = sqrt(pow(x[a] - x[b], 2) + pow(y[a] - y[b], 2));
    }
}
