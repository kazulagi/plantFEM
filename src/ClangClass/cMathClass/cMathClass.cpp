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