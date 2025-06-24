program bcrs_bicgstab_test
    use SparseClass
    use LinearSolverClass
    implicit none
    
    type(BCRS_) :: bcrs
    type(COO_)  :: coo
    type(CRS_)  :: crs
    real(real64),allocatable :: x(:),b(:),a(:,:)

    call coo%poisson(3)
    crs = coo%to_CRS()
    call bcrs%set([1,1],crs )
    call bcrs%set([2,2],crs )
    call bcrs%set([3,3],crs )

    b = [1.0d0,0.0d0,-2.0d0] // [1.0d0,0.0d0,-2.0d0] // [1.0d0,0.0d0,-2.0d0]
    call bcrs%bicgstab(x=x,b=b,itrmax=1000,er=dble(1.0e-14),relative_er=dble(1.0e-18),debug=true)

    print *, norm(bcrs%matmul(x)-b)
    print *, "x="
    call print(x)
    print *, "A="
    call print(bcrs%to_dense())
    print *, "b="
    call print(b)

    
    call gauss_jordan_pv(bcrs%to_dense(), x, b, size(x))
    print *, "x="
    call print(x)

end program bcrs_bicgstab_test