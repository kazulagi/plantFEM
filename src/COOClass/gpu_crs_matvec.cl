__kernel void gpu_crs_matvec(__global int *mem_row_ptr,
    __global int *mem_col_idx,__global double *mem_val,__global double *mem_old_vector,
    __global double *mem_new_vector){
    
    return;
    // __global double *, __global double *b,
    //__global double *dpp, __global int *dim_num) {
/*  
   __local double partial_dot[sizeof(a) ];
   __local double total;
   __local int m;

  int gid = get_global_id(0);
  int lid = get_local_id(0);
  int group_size = get_local_size(0);

  

  partial_dot[lid] = a[gid] * b[gid];
  
  barrier(CLK_LOCAL_MEM_FENCE);

  if(lid == 0){
    for (m=0;m<group_size-1 ;m++)
    {
      total=total+partial_dot[m];
    }
    *dpp=total;
} */
    }