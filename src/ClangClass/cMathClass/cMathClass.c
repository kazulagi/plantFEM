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


 void c_allocatev(double **pa, int dim_in)
 {
  double b;
  double *a;
  int m;
  int dim_re;

  a = (double*) malloc(sizeof(double)*dim_in);
  *pa=a;
  

 }

 void addvalvec(double **pa, int dim_in, int loc, double val_in)
 {
  double b;
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

 double dotproduct(double **pa, double **pa2,int dim_in)
 {
  double *a;
  double *b;
  double val;
  int m;

  a = (double*) malloc(sizeof(double)*dim_in);
  a=*pa;


  b = (double*) malloc(sizeof(double)*dim_in);
  b=*pa2;

  val=0.0;
  for (m=0;m<dim_in;m++)
  {
    val=val+b[m]*a[m];
  }
  printf("%f\n",val);

  return val;

 }


void test( int a[] ){
  double *hoge_p;
  double hoge[10];

  printf("pointer address %p\n",hoge_p);
  printf("pointer address %p\n",&hoge[0]);
  printf("pointer address %p\n",&hoge[1]);
  printf("pointer address %p\n",&hoge[2]);
  printf("pointer address %p\n",&hoge[3]);
  printf("pointer address %p\n",&hoge[4]);
  printf("pointer address %p\n",&hoge[5]);

  int hoge1=5;
  void *hoge_p1;

  hoge_p1=&hoge1;
  printf("%d\n",*(int*)hoge_p1); //cast hoge_p to int*
  printf("%p\n",hoge_p1);

  //int *int_p;
  //double double_variable;
  //int_p=&double_variable;

  printf("pointer address %p\n",hoge_p);
  hoge_p++;
  printf("pointer address %p\n",hoge_p);
  hoge_p++;
  printf("pointer address %p\n",hoge_p);
  hoge_p++;
  printf("pointer address %p\n",hoge_p);
  

  printf("\nlesson2 pointer and array \n\n");
  
  hoge[0] =0.0;
  hoge[1] =1.0;
  hoge[2] =2.0;
  hoge[3] =3.0;
  hoge[4] =4.0;
  hoge[5] =5.0;
  hoge[6] =6.0;
  hoge[7] =7.0;
  hoge[8] =8.0;
  hoge[9] =9.0;
  
  hoge_p=&hoge[0];
  printf("pointer address %f\n",*hoge_p);
  hoge_p=hoge_p+1;
  printf("pointer address %f\n",*hoge_p);
  hoge_p=hoge_p+1;
  printf("pointer address %f\n",*hoge_p);
  hoge_p=hoge_p+1;
  printf("pointer address %f\n",*hoge_p);
  hoge_p=hoge_p+1;
  printf("pointer address %f\n",*hoge_p);
  hoge_p=hoge_p+1;
  printf("pointer address %f\n",*hoge_p);
  hoge_p=hoge_p-2;
  printf("pointer address %f\n",*hoge_p);
  
  //Same meaning
  printf("\n<= is same as => \n\n");

  hoge_p=hoge;
  printf("pointer address %f\n",*hoge_p);
  hoge_p=hoge_p+1;
  printf("pointer address %f\n",*hoge_p);
  hoge_p=hoge_p+1;
  printf("pointer address %f\n",*hoge_p);
  hoge_p=hoge_p+1;
  printf("pointer address %f\n",*hoge_p);
  hoge_p=hoge_p+1;
  printf("pointer address %f\n",*hoge_p);
  hoge_p=hoge_p+1;
  printf("pointer address %f\n",*hoge_p);
  hoge_p=hoge_p-2;
  printf("pointer address %f\n",*hoge_p);
  
  //Same meaning
  printf("\n<= is same as => \n\n");

  int i;
  hoge_p=hoge;
  for(i=0; i<10; i++){
    printf("pointer address %f\n",*(hoge_p+i) );  
  }


  //Same meaning
  printf("\n<= is same as => \n\n");

  hoge_p=hoge;
  for(i=0; i<10; i++){
    printf("pointer address %f\n",hoge_p[i] );  
  }


  //Same meaning
  printf("\n<= is same as => \n\n");

  hoge_p=hoge;
  for(i=0; i<10; i++){
    printf("pointer address %f\n",i[hoge_p] );  
  }

  
  
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
int main(){

  int size1;
  int size2;
  printf("Input array size1 ,size2\n");
  scanf("%d%d\n",&size1,&size2);  
  int array[size1][size2];

  int i;
  

  int a=1;
  test(&a);
}