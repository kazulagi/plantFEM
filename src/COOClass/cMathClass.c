 #include <stdio.h>
 #include <malloc.h>

 void testc(double **pa)
 {
  double b;
  double *a;
  int m;

  a = (double*) malloc(sizeof(double)*5);
  a[0]=1.23;
  a[1]=2.46;
  a[2]=3.69;
  a[3]=4.11;
  a[4]=7.21;
  *pa=a;
  for (m=0;m<5;m++)
  {
    b=a[m];
    b=b+1.0;
    a[m]=b;
  }
 }


 void setzerovec(double **pa, int dim_in)
 {
  double b;
  double *a;
  int m;
  int dim_re;

  a = (double*) malloc(sizeof(double)*dim_in);
  *pa=a;
  for (m=0;m<dim_in;m++)
  {
    a[m]=0.00;
  }

 }


 void initvec(double **pa, int dim_in)
 {
  double b;
  double *a;
  int m;
  int dim_re;

  a = (double*) malloc(sizeof(double)*dim_in);
  *pa=a;
  

 }


 void c_allocatev(double *pa[], int dim_in)
 {
  double *a;

  a = (double*) malloc(sizeof(double)*dim_in);
  *pa=a;
  

 }

 void addvalvec(double *pa[], int dim_in, int loc, double val_in)
 {
  double *a;
  int m;

  a = (double*) malloc(sizeof(double)*dim_in);
  a=*pa;
  a[loc-1]=a[loc-1]+val_in;
  *pa=a;
 }


 void putvalvec(double **pa, int dim_in, int loc, double val_in)
 {
  double b;
  double *a;
  int m;

  a = (double*) malloc(sizeof(double)*dim_in);
  a=*pa;
  a[loc-1]=val_in;
  *pa=a;
 }

 double dotproduct(double *pa[], double *pa2[],int dim_in)
 {
  double *a;
  double *b;
  double val;
  int m;

  //a = (double*) malloc(sizeof(double)*dim_in);
  a=*pa;


  //b = (double*) malloc(sizeof(double)*dim_in);
  b=*pa2;

  val=0.0;
  for (m=0;m<dim_in;m++)
  {
    val=val+(a[m])*(b[m]);
  }
  printf("%f\n",val);

  return val;

 }

// void setvec(double **pa, double **pa_in, int dim_in)
// {
//  double b;
//  double *a;
//  double *a_in;
//  int m;
//  
//  printf("%lf",**pa_in );
//  return;
//  
//  a = (double*) malloc(sizeof(double)*dim_in);
//  *pa=a;
//  a_in=*pa_in;
//  for (m=0;m<dim_in;m++)
//  {
//    a[m]=a_in[m];
//  }
//
// }