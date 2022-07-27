/*
 * C function that takes a float array, and cubes each element.
 */
#include <stddef.h>
#include <iostream>
#define CL_TARGET_OPENCL_VERSION 120
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

constexpr size_t MEM_SIZE = 128;

void c_dot_product(double x[],double y[],size_t n,double dp[])
{
    int i;
    dp[0] = 0.0;
    for (i=0; i<n; i++){
        dp[0] += x[i]*y[i];
    };
}


void cl_dot_product(double x[],double y[],size_t n,double dp[])
{
    int i;
    cl_device_id device_id = nullptr;
	cl_context context = nullptr;
	cl_command_queue command_queue = nullptr;
	cl_mem memobj = nullptr;
	cl_program program = nullptr;
	cl_kernel kernel = nullptr;
	cl_platform_id platform_id = nullptr;
	cl_uint ret_num_devices;
	cl_uint ret_num_platforms;
	cl_int ret;


	char string[MEM_SIZE];


	const char source_str[] =
    " __kernel void hello(__global char* string)
        {
       string[0] = 'H';
       string[1] = 'e';
       string[2] = 'l';
       string[3] = 'l';
       string[4] = 'o';
       string[5] = ',';
       string[6] = ' ';
       string[7] = 'W';
       string[8] = 'o';
       string[9] = 'r';
       string[10] = 'l';
       string[11] = 'd';
       string[12] = '!';
       string[13] = '\0';
    }";

	size_t source_size = sizeof(source_str) - 1;
	const char* source_list[] = { source_str };


	/* プラットフォーム・デバイスの情報の取得 */
	ret = clGetPlatformIDs(1, &platform_id, &ret_num_platforms);
	ret = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_DEFAULT, 1, &device_id, &ret_num_devices);


	/* OpenCLコンテキストの作成 */
	context = clCreateContext(nullptr, 1, &device_id, nullptr, nullptr, &ret);

	/* コマンドキューの作成 */
	command_queue = clCreateCommandQueue(context, device_id, 0, &ret);

	/* メモリバッファの作成 */
	memobj = clCreateBuffer(context, CL_MEM_READ_WRITE, MEM_SIZE * sizeof(char), nullptr, &ret);



    dp[0] = 0.0;
    for (i=0; i<n; i++){
        dp[0] += x[i]*y[i];
    };
}

void c_sparse_matvec(int row_ptr[],int col_idx[], double val[],double x[],size_t n,size_t n_col,double ret[])
{
    int i;
    int col_i, row;
    
    for (i=0; i<n; i++){
        ret[0] = 0.0;
    };

    // matvec
    for (row=0; row<n; row++){ // for each row 
        for (col_i=row_ptr[row]; col_i<row_ptr[row+1]-1; col_i++){ // for each row
            ret[col_idx[col_i]] = val[col_i]*x[row] ;
        };
    };
    
}