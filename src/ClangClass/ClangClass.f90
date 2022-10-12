module ClangClass
    Use Iso_C_Binding
    implicit none
    
    !
    ! Demonstrates passing a real array to C.
    !

    interface
      subroutine cube(ia,n) Bind(C,Name='real_cube')
        Import
        Integer(C_size_t),Value :: n
        Real(C_double),Intent(InOut) :: ia(n)
      end subroutine
    end interface


    interface
      subroutine c_dot_product(x,y,n,ret) Bind(C,Name='c_dot_product')
        Import
        Integer(c_size_t),Value :: n
        Real(c_double),Intent(in)    :: x(1:n)
        Real(c_double),Intent(in)    :: y(1:n)
        Real(c_double)  :: ret(1)

      end subroutine
    end interface



    interface
      subroutine c_opencl_dot_product() Bind(C)
        
      end subroutine
    end interface


    interface
      subroutine c_opencl_matmul_crs(n,n_col_idx,col_idx,row_ptr,val,vec,ret_vec) Bind(C)
        Import
        Integer(c_size_t),Value :: n,n_col_idx
        integer(c_int),Intent(in)    :: col_idx(1:n_col_idx),row_ptr(1:n+1)
        Real(c_double),Intent(in)    :: val(1:n_col_idx),vec(1:n)
        Real(c_double),Intent(inout) :: ret_vec(1:n)
        
      end subroutine
    end interface

end module ClangClass