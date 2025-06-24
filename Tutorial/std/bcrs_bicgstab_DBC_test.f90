program bcrs_bicgstab_test
    use SparseClass
    use LinearSolverClass
    implicit none
    
    type(BCRS_) :: bcrs
    type(COO_)  :: coo
    type(CRS_)  :: crs
    real(real64),allocatable :: x(:),b(:),a(:,:)
    real(real64) :: dbc_val

    dbc_val = 10.0d0
    call coo%poisson(3)
    crs = coo%to_CRS()
    call bcrs%set([1,1],crs )
    !call bcrs%set([2,2],crs )
    !call bcrs%set([3,3],crs )

    b = [1.0d0,0.0d0,-2.0d0] ! // [1.0d0,0.0d0,-2.0d0] // [1.0d0,0.0d0,-2.0d0]
    call bcrs%fix(idx=[1],val=[dbc_val],RHS=b)
    call bcrs%bicgstab(x=x,b=b,itrmax=1000,er=dble(1.0e-14),relative_er=dble(1.0e-18),debug=true)

    print *, "|e|",norm(bcrs%matmul(x)-b)

    print *, "x="
    call print(x)
    
    ! x_1 = 10
    ! -4 x_2 +   x_3 = 0 - 10
    !    x_2 - 2 x_3 = -2 

    !    x_2  = -2 + 2 x_3
    ! -4 (-2 + 2*x_3) +   x_3 = 0 - 10
    !   - 7*x_3  = - 18
    !   x_3  =  18/7 = 
    !   x_2  =  2*18/7 -2 = 36/7 -14/7 = 22/7

    a = bcrs%to_dense()
    print *, "A="
    call print(a)
    
    b = [dbc_val, b(2) - a(2,1)*dbc_val, b(3) - a(3,1)*dbc_val]
    a(1,:) = 0.0d0
    a(:,1) = 0.0d0
    a(1,1) = 1.0d0
    x(:)=0.0d0
    print *, "A="
    call print(a)
    print *, "b="
    call print(b)

    
    call gauss_jordan_pv_real64(a0=a, x=x, b=b, n=size(x))
    print *, "x="
    call print(x)
    print *, "|e|",norm(bcrs%matmul(x)-b)

end program bcrs_bicgstab_test