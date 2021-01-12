__kernel void add(__global double *val_in,__global double *val_in2,__global double *val_out  ) {
  
  int gid = get_global_id(0);
  int lid = get_local_id(0);
  int group_size = get_local_size(0);
  
  printf("MyGID is : %d\n",gid);
  printf("MyLID is : %d\n",lid);

  //printf("Imported value into kernel x is : %f\n",*val_in);
  //printf("Imported value into kernel y is : %f\n",*val_in2);
  *val_out= *val_in2 + *val_in ;
  //printf("Imported value into kernel z = x + y is : %f\n",*val_out);
  //printf("dp == %f\n",z);
  
}

//__kernel double gpu_dot_product(__global double x ) {
//  double z;
//  z = 2.0 * x ;
//  printf("oafsijo\n");
//  //printf("dp == %f\n",z);
//  return z;
//}
