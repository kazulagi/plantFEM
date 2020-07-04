program main
    use LinearSolverClass
    implicit none

    ! Create Ax=B and solve x = A^(-1) B
    integer(int32), parameter :: N = 4 ! rank = 3
    real(real64)  :: t1,t2 ! time measure
    real(real64) :: A(1:N,1:N), B(1:N), X(1:N) ! A, x and B
    real(real64) :: Val(1:10) 
    integer(int32) :: index_i(1:10),index_j(1:10)
    type(LinearSolver_) :: solver ! linear solver instance.

    ! creating A, x, and B
    A(1,1:4) = (/  2.0d0, -1.0d0,  0.0d0, 0.0d0 /)
    A(2,1:4) = (/ -1.0d0,  2.0d0, -1.0d0, 0.0d0 /)
    A(3,1:4) = (/  0.0d0,  2.0d0,  3.0d0, 1.0d0 /)
    A(4,1:4) = (/  0.0d0,  0.0d0,  1.0d0, 2.0d0 /)

    X(1:4)   = (/  0.0d0,  0.0d0,  0.0d0, 0.0d0 /)

    B(1:4)   = (/  1.0d0,  2.0d0,  3.0d0, 4.0d0 /)

    ! CRS-format
    Val(1:10) = (/  2.0d0, -1.0d0,  -1.0d0,  2.0d0, -1.0d0, 2.0d0, 3.0d0, &
        1.0d0, 1.0d0,  2.0d0/)
    index_i(1:10) = (/ 1, 1, 2, 2, 2, 3, 3, 3, 4, 4/)
    index_j(1:10) = (/ 1, 2, 1, 2, 3, 2, 3, 4, 3, 4/)

    ! import Ax=B into the linear solver instance.
    call solver%import(a = A, x = X, b = B)

    ! get a cpu-time
    call cpu_time(t1)  
    ! solve Ax=B by Gauss-Jordan
    call solver%solve(Solver="GaussJordan")
    ! get a cpu-time
    call cpu_time(t2)  
    ! show result
    ! X is stored in solver%X(:) (solver.x[])
    print *, "Solved by GaussJordan",solver%X(:),"/",t2-t1," sec."

    ! Similarly ...

    X(1:4)   = (/  0.0d0,  0.0d0,  0.0d0, 0.0d0 /)
    call solver%import(a = A, x = X, b = B, val=val, index_i=index_i, index_j=index_j)
    call cpu_time(t1)  
    call solver%solve(Solver="GaussSeidel")
    call cpu_time(t2)  
    print *, "Solved by GaussSeidel",solver%X(:),"/",t2-t1," sec."
    
    X(1:4)   = (/  0.0d0,  0.0d0,  0.0d0, 0.0d0 /)
    call solver%import(a = A, x = X, b = B, val=val, index_i=index_i, index_j=index_j)
    call cpu_time(t1)  
    call solver%solve(Solver="BiCGSTAB",CRS=.true.)
    call cpu_time(t2)  
    print *, "Solved by BiCGSTAB(CRS)",solver%X(:),"/",t2-t1," sec."

    X(1:4)   = (/  0.0d0,  0.0d0,  0.0d0, 0.0d0 /)
    call solver%import(a = A, x = X, b = B, val=val, index_i=index_i, index_j=index_j)
    call cpu_time(t1)  
    call solver%solve(Solver="BiCGSTAB",CRS=.false.)
    call cpu_time(t2)  
    print *, "Solved by BiCGSTAB   ",solver%X(:),"/",t2-t1," sec."

    X(1:4)   = (/  0.0d0,  0.0d0,  0.0d0, 0.0d0 /)
    call solver%import(a = A, x = X, b = B)
    call cpu_time(t1)  
    call solver%solve(Solver="GPBiCG")
    call cpu_time(t2)  
    print *, "Solved by GPBiCG     ",solver%X(:),"/",t2-t1," sec."
    
    ! Importance Index 9 / 10 : [********* ]
    
end program