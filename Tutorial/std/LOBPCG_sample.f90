program main
    use SparseClass
    implicit none
    
    

    type(COO_) :: A_COO, B_COO
    type(CRS_) :: A_CRS, B_CRS
    real(real64),allocatable :: X(:), lambda,&
        X_all(:,:),lambda_all(:),&
        X_all_lapack(:,:),lambda_all_lapack(:)
    real(real64) :: err
    integer(int32) :: n

    n = 3000

    ! >> generate problem
    ! A X = \lambda B X
    ! as COO-format
    call A_COO%init(n)
    do i_i=1,n
        call A_COO%add(i_i,i_i-2, 0.30d0)
        call A_COO%add(i_i,i_i-1, 0.50d0)
        call A_COO%add(i_i,i_i, i_i*1.0d0)
        call A_COO%add(i_i,i_i+1, 0.50d0)
        call A_COO%add(i_i,i_i+2, 0.30d0)
    enddo
    call A_COO%add(1,3, 1.0d0)
    call A_COO%add(3,1, 1.0d0)
    call A_COO%add(4,2, 1.0d0)
    call A_COO%add(2,4, 1.0d0)

    call B_COO%init(n)
    do i_i=1,n
        call B_COO%add(i_i,i_i-1, 0.50d0)
        call B_COO%add(i_i,i_i, i_i*1.0d0)
        call B_COO%add(i_i,i_i+1, 0.50d0)
    enddo
    ! << generate matrix as COO-format
    A_CRS = A_COO%to_CRS()
    B_CRS = B_COO%to_CRS()

!>>> To execute single vector mode, please checkout these comment-outs
!    call LOBPCG_single(A=A_CRS,B=B_CRS,X=X, lambda=lambda,alpha=dble(2.0e-3),tol=dble(1.0e-8) )
!    lambda_all = lambda*eyes(1)
!    X_all = zeros(n,1)
!    X_all(:,1) = X
!    X_all(:,1)=X_all(:,1)/norm(X_all(:,1))
!<<<
    
!>>> To execute multi-vector mode, please checkout these comment-outs
    call LOBPCG(A=A_CRS,B=B_CRS,X=X_all, lambda=lambda_all,m=3,MAX_ITR=100000,TOL=dble(1.0e-8),debug=true)
    print *, "eigen_value"
    print *, lambda_all
!<<<
    
    call LAPACK_EIG(A=A_CRS%to_Dense(),B=B_CRS%to_Dense(),x=X_all_lapack,lambda=lambda_all_lapack )
    print *, "eigen_value"
    print *, lambda_all_lapack(1:3)

    !!COMPARE!!
    print *, ">>>> test code executed >>>>> "
    print *, "Check Error on Eigen value", abs(lambda_all(1)-lambda_all_lapack(1) )
    err =  abs(lambda_all(1)-lambda_all_lapack(1) )
    if( err < 1.0e-14 )then
        print *, "[EXCELLENT] PERFECT ACCURACY"
    elseif( err < 1.0e-7 )then
        print *, "[GOOD] FAIR"
    else
        print *, "[WARNING] TOO LARGE ERROR"
    endif
    print *, ">>>> test code executed >>>>> "

contains



end program main
