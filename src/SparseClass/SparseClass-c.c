/*
 * C function that takes a float array, and cubes each element.
 */
#include <stddef.h>
#include <stdio.h>

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