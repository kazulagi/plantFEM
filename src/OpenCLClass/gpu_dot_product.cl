__kernel void gpu_dot_product(__global double *a, __global double *b,
    __global double *dpp, __global int *dim_num) {
  
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
  }
  

  //*dpp=*dpp+partial_dot[lid];
  //printf("Imported value into kernel x is : %f\n",*val_in);
  //printf("Imported value into kernel y is : %f\n",*val_in2);
  //for(int i = group_size/2; i>0; i >>= 1) {
  //  if(lid < i) {
  //    partial_dot[lid] += partial_dot[lid + i];
  //  }
  //  barrier(CLK_LOCAL_MEM_FENCE);
  //}

  //for (m=0;m<dim_in;m++)
  //{
  //  val=val+(a[m])*(b[m]);
  //}

  //*dpp = partial_dot[group_size];
  //if(lid == 0) {
  //  dpp[get_group_id(0)] = dot(partial_dot[0], (double)(1.0f));
  //}


  //*dp = *dp + *dp_local;
  //printf("Imported value into kernel z = x + y is : %f\n",*val_out);
  //printf("dp == %f\n",z);
  
}



//__kernel void gpu_dot_product(__global double *a, __global double *b,
//    __global double *dpp, __global int *dim_num) {
//
//
//  int gid = get_global_id(0);
//  int lid = get_local_id(0);
//  int group_size = get_local_size(0);
//
//    int dim_loc;
//    dim_loc=*dim_num;
//   __local double total;
//   __local int m;
//
//
//  __local int interval;
//
//  interval = *dim_num/group_size;
//  //printf("interval %d\n",interval);
//
//  
//
//  a[gid] = a[gid] * b[gid];
//  for(m=gid+interval;m<=*dim_num-1 ;m=m+interval){
//    a[gid] = a[gid] + a[m] * b[m];
//    a[m]=0.0;
//  }
//  //partial_dot[lid] = a[gid] * b[gid];
//  //printf("local sum = %f\n",a[lid]);
//  
//
//  barrier(CLK_LOCAL_MEM_FENCE);
//
//  if(lid == 0){
//    total = 0.0;
//    for (m=0;m<group_size-1 ;m=m+1)
//    {
//      total=total+a[m];
//    }
//    *dpp=total;
//  }
  