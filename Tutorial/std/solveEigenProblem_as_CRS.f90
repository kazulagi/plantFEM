program main
    use SparseClass
    implicit none
    real(real64),allocatable :: Eigen_vectors(:,:),eigen_values(:)
    type(CRS_) :: CRS

    !LAPACK
    CRS%val = [1.0d0, 2.0d0, 3.0d0, 2.0d0, 1.0d0, 1.0d0, 3.0d0, 1.0d0, 8.0d0]
    CRS%row_ptr=[1,4,7,10]
    CRS%col_idx = [1,2,3,1,2,3,1,2,3]

    ! Lanczos method
    call CRS%eig(Eigen_vectors=eigen_vectors,eigen_values=eigen_values)
    call print(eigen_values)
    call print(eigen_vectors)
    
    ! same as
    ! https://www.emathhelp.net/ja/calculators/linear-algebra/eigenvalue-and-eigenvector-calculator/

end program main