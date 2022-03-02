program main
    use FEMSolverClass
    implicit none

    integer(int32),allocatable :: CRS_col(:),CRS_rowptr(:)
    real(real64),allocatable :: CRS_val(:)


    CRS_val = dble([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0])
    CRS_col = [1,2,2,3,3,4,3,4,5,4,5]
    CRS_rowptr = [1,3,5,7,10,12]
    call reduce_crs_matrix(CRS_val,CRS_col,CRS_rowptr,remove_IDs=[4])
    print *, CRS_val
    print *, CRS_col
    print *, CRS_rowptr
    
end program main