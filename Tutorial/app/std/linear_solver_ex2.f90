program main
    use LinearSolverClass
    use IOClass
    implicit none

    ! Create Ax=B and solve x = A^(-1) B
    !integer(int32), parameter :: N = 4 
    integer(int32), parameter :: NN=10000! rank 
    real(real64)  :: t1,t2 ! time measure
    !real(real64) :: A(1:N,1:N), B(1:N), X(1:N) ! A, x and B
    !real(real64) :: Val(1:10) 
    !integer(int32) :: index_i(1:10),index_j(1:10)
    real(real64)  ,allocatable :: Val_c(:) 
    integer(int32),allocatable :: index_i_c(:),index_j_c(:)
    integer(int32) :: i,m,k,j
    real(real64) :: Amat(NN,NN),bvec(NN),xvec(NN)
    type(LinearSolver_) :: solver ! linear solver instance.
    type(IO_) :: f
    type(Random_) ::rdm

    call rdm%init()
    !call f%open("Amat.txt")
    do i=1,size(Amat,1)
        Amat(i,i)=rdm%random()+10.0d0
    enddo
    do i=1,size(Amat,1)-1
        Amat(i,i+1)=Amat(i,i+1)+rdm%random()+1.0d0
        Amat(i+1,i)=Amat(i+1,i)+rdm%random()+2.0d0
    enddo
    do i=1,NN
        Bvec(i)=rdm%random()+2.0d0
    enddo
    !call f%close()
    xvec(:)=0.0d0
    m=0
    do i=1,size(Amat,1)
        do j=1,size(Amat,1)
            if(Amat(i,j)/=0.0d0 )then
                m=m+1         
            endif
        enddo
    enddo
    allocate(val_c(m) )
    allocate(index_i_c(m) )
    allocate(index_j_c(m) )

    m=0
    do i=1,size(Amat,1)
        do j=1,size(Amat,1)
            if(Amat(i,j)/=0.0d0 )then
                m=m+1
                val_c(m)=Amat(i,j)
                index_i_c(m) = i
                index_j_c(m) = j
            endif
        enddo
    enddo
!    ! creating A, x, and B
!    A(1:4,1) = (/  2.0d0, -1.0d0,  0.0d0, 0.0d0 /)
!    A(1:4,2) = (/ -1.0d0,  2.0d0, -2.0d0, 0.0d0 /)
!    A(1:4,3) = (/  0.0d0, -1.0d0,  3.0d0, 2.0d0 /)
!    A(1:4,4) = (/  0.0d0,  0.0d0,  2.0d0, 5.0d0 /)
!
!!    X(1:4)   = (/  0.0d0,  0.0d0,  0.0d0, 0.0d0 /)
!
!    B(1:4)   = (/  1.0d0,  2.0d0,  3.0d0, 4.0d0 /)
!
!    ! CRS-format
!    Val(1:10) = (/  2.0d0, -1.0d0,  -1.0d0,  2.0d0, -2.0d0, -1.0d0, -3.0d0, &
!        2.0d0, 2.0d0,  5.0d0/)
!    index_i(1:10) = (/ 1, 1, 2, 2, 2, 3, 3, 3, 4, 4/)
!    index_j(1:10) = (/ 1, 2, 1, 2, 3, 2, 3, 4, 3, 4/)
    ! import Ax=B into the linear solver instance.
    call solver%import(a = Amat, x = Xvec, b = Bvec)

    ! get a cpu-time
    call cpu_time(t1)  
    ! solve Ax=B by Gauss-Jordan
    call solver%solve(Solver="GaussJordan")
    ! get a cpu-time
    call cpu_time(t2)  
    ! show result
    ! X is stored in solver%X(:) (solver.x[])
    print *, "Solved by GaussJordan","/",t2-t1," sec."

    ! Similarly ...

!    X(1:4)   = (/  0.0d0,  0.0d0,  0.0d0, 0.0d0 /)
    call solver%import(a = Amat, x = Xvec, b = Bvec, val=val_c, index_i=index_i_c, index_j=index_j_c)
    call cpu_time(t1)  
    call solver%solve(Solver="GaussSeidel")
    call cpu_time(t2)  
    !print *, "Solved by GaussSeidel",solver%X(:),"/",t2-t1," sec."
    print *, "Solved by GaussSeidel","/",t2-t1," sec."
    
!    X(1:4)   = (/  0.0d0,  0.0d0,  0.0d0, 0.0d0 /)
    call solver%import(a = Amat, x = Xvec, b = Bvec, val=val_c, index_i=index_i_c, index_j=index_j_c)
    call cpu_time(t1)  
    call solver%solve(Solver="BiCGSTAB",CRS=.true.)
    call cpu_time(t2)  
    !print *, "Solved by BiCGSTAB(CRS)",solver%X(:),"/",t2-t1," sec."
    print *, "Solved by BiCGSTAB(CRS)","/",t2-t1," sec."

!    X(1:4)   = (/  0.0d0,  0.0d0,  0.0d0, 0.0d0 /)
    call solver%import(a = Amat, x = Xvec, b = Bvec, val=val_c, index_i=index_i_c, index_j=index_j_c)
    call cpu_time(t1)  
    call solver%solve(Solver="BiCGSTAB",CRS=.false.)
    call cpu_time(t2)  
    !print *, "Solved by BiCGSTAB   ",solver%X(:),"/",t2-t1," sec."
    print *, "Solved by BiCGSTAB   ","/",t2-t1," sec."
!    X(1:4)   = (/  0.0d0,  0.0d0,  0.0d0, 0.0d0 /)
    call solver%import(a = Amat, x = Xvec, b = Bvec)
    call cpu_time(t1)  
    call solver%solve(Solver="GPBiCG")
    call cpu_time(t2)  
    !print *, "Solved by GPBiCG     ",solver%X(:),"/",t2-t1," sec."
    print *, "Solved by GPBiCG     ","/",t2-t1," sec."
    
    ! Under construction!
    !X(1:3)   = (/  0.0d0,  0.0d0,  0.0d0 /)
    !call solver%import(a = A, x = X, b = B)
    !call cpu_time(t1)  
    !call solver%solve(Solver="GPBiCG",preconditioning=.true.)
    !call cpu_time(t2)  
    !print *, "Solved by pre-GPBiCG ",solver%X(:),"/",t2-t1," sec."
end program