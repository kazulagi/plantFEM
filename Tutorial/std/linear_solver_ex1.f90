program main
    use LinearSolverClass
    implicit none

    integer(int32), parameter :: N = 3
    real(real64)  :: t1,t2
    real(8) :: A(1:N,1:N), B(1:N), X(1:N)
    type(LinearSolver_) :: solver


    A(1:3,1) = (/  2.0d0, -1.0d0,  0.0d0 /)
    A(1:3,2) = (/ -1.0d0,  0.10d0, -12.0d0 /)
    A(1:3,3) = (/  0.0d0, -1.0d0,  0.10d0 /)

    B(1:3)   = (/  1.0d0,  2.0d0,  3.0d0 /)

    X(1:3)   = (/  0.0d0,  0.0d0,  0.0d0 /)
    call solver%import(a = A, x = X, b = B)
    call cpu_time(t1)  
    call solver%solve(Solver="GaussJordan")
    call cpu_time(t2)  
    print *, "Solved by GaussJordan",solver%X(:),"/",t2-t1," sec."

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
    
    X(1:3)   = (/  0.0d0,  0.0d0,  0.0d0 /)
    call solver%import(a = A, x = X, b = B)
    call cpu_time(t1)  
    call solver%solve(Solver="GPBiCG",preconditioning=.true.)
    call cpu_time(t2)  
    print *, "Solved by pre-GPBiCG ",solver%X(:),"/",t2-t1," sec."
end program