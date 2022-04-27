/*
 * C function that takes a double array, and cubes each element.
 */
#include <stddef.h>
#include <stdio.h>

void real_cube(double x[],size_t n)
{
  size_t i;
  for (i=0; i<n; i++) x[i] = x[i]*x[i]*x[i];
}

void c_dot_product(double x[],double y[],size_t n,double ret[])
{
  size_t i;
  ret[0] = 0.0;
  for (i=0; i<n; i++){
    ret[0] = ret[0] + x[i]*y[i];
  } ;
}