/*
 * C function that takes a float array, and cubes each element.
 */
#include <stddef.h>
#include <stdio.h>
#include <omp.h>

void c_dot_product(double x[],double y[],size_t n,double dp[])
{
    int i;
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



void crs_spmv_real32(float val[], long int row_ptr[], int col_idx[],float old_vector[], 
    float new_vector[], size_t n,size_t col_size)
{
    size_t row;
    size_t col;

    #pragma omp parallel for private(col) 
    for (row=0; row<n; row++){
        for (col=row_ptr[row]; col<row_ptr[row+1];col++  ){
            new_vector[row] = new_vector[row] + val[col-1]*old_vector[col_idx[col-1]-1 ];
        };
    };
}


void crs_spmv_real64(double val[], long int row_ptr[], int col_idx[],double old_vector[], 
    double new_vector[], size_t n,size_t col_size)
{
    size_t row;
    size_t col;

    #pragma omp parallel for private(col) 
    for (row=0; row<n; row++){
        for (col=row_ptr[row]; col<row_ptr[row+1];col++  ){
            new_vector[row] = new_vector[row] + val[col-1]*old_vector[col_idx[col-1]-1 ];
        };
    };
}


/* matrix cos*/

void c_sparse_matrix_cos(int row_ptr[],int col_idx[], double val[],double x[],size_t n,size_t n_col,double ret[])
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
