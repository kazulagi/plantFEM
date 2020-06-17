program main
    use LinearSolverClass
    implicit none

    ! Create Ax=B and solve x = A^(-1) B
    integer(int32), parameter :: N = 3 ! rank = 3
    real(real64)  :: t1,t2 ! time measure
    real(8) :: A(1:N,1:N), B(1:N), X(1:N) ! A, x and B
    type(LinearSolver_) :: solver ! linear solver instance.

    ! creating A, x, and B
    A(1:3,1) = (/  2.0d0, -1.0d0,  0.0d0 /)
    A(1:3,2) = (/ -1.0d0,  2.0d0, -1.0d0 /)
    A(1:3,3) = (/  0.0d0, -1.0d0,  3.0d0 /)

    X(1:3)   = (/  0.0d0,  0.0d0,  0.0d0 /)

    B(1:3)   = (/  1.0d0,  2.0d0,  3.0d0 /)

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

    X(1:3)   = (/  0.0d0,  0.0d0,  0.0d0 /)
    call solver%import(a = A, x = X, b = B)
    call cpu_time(t1)  
    call solver%solve(Solver="GaussSeidel")
    call cpu_time(t2)  
    print *, "Solved by GaussSeidel",solver%X(:),"/",t2-t1," sec."
    
    X(1:3)   = (/  0.0d0,  0.0d0,  0.0d0 /)
    call solver%import(a = A, x = X, b = B)
    call cpu_time(t1)  
    call solver%solve(Solver="BiCGSTAB")
    call cpu_time(t2)  
    print *, "Solved by BiCGSTAB   ",solver%X(:),"/",t2-t1," sec."

    X(1:3)   = (/  0.0d0,  0.0d0,  0.0d0 /)
    call solver%import(a = A, x = X, b = B)
    call cpu_time(t1)  
    call solver%solve(Solver="GPBiCG")
    call cpu_time(t2)  
    print *, "Solved by GPBiCG     ",solver%X(:),"/",t2-t1," sec."
    
    ! Under construction!
    !X(1:3)   = (/  0.0d0,  0.0d0,  0.0d0 /)
    !call solver%import(a = A, x = X, b = B)
    !call cpu_time(t1)  
    !call solver%solve(Solver="GPBiCG",preconditioning=.true.)
    !call cpu_time(t2)  
    !print *, "Solved by pre-GPBiCG ",solver%X(:),"/",t2-t1," sec."
end program