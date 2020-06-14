program main
    use LinearSolverClass
    implicit none

    integer, parameter :: N = 3
    integer :: i
    real(8) :: A(1:N,1:N), B(1:N), X(1:N)
    type(LinearSolver_) :: solver
    A(1:3,1) = (/  2.0d0, -1.0d0,  0.0d0 /)
    A(1:3,2) = (/ -1.0d0,  2.0d0, -1.0d0 /)
    A(1:3,3) = (/  0.0d0, -1.0d0,  1.0d0 /)

    B(1:3)   = (/  1.0d0,  2.0d0,  3.0d0 /)

    X(1:3)   = (/  0.0d0,  0.0d0,  0.0d0 /)
    call solver%import(a = A, x = X, b = B)
    call solver%solve(Solver="GaussJordan")
    print *, "Solved by GaussJordan",solver%X(:)

      
    X(1:3)   = (/  0.0d0,  0.0d0,  0.0d0 /)
    call solver%import(a = A, x = X, b = B)
    call solver%solve(Solver="GaussSeidel")
    print *, "Solved by GaussSeidel",solver%X(:)
    
    X(1:3)   = (/  0.0d0,  0.0d0,  0.0d0 /)
    call solver%import(a = A, x = X, b = B)
    call solver%solve(Solver="BiCGSTAB")
    print *, "Solved by BiCGSTAB     ",solver%X(:)

    ! Experimental
    !X(1:3)   = (/  0.0d0,  0.0d0,  0.0d0 /)
    !call solver%import(a = A, x = X, b = B)
    !call solver%solve(Solver="GPBiCG")
    !print *, "Solved by GPBiCG     ",solver%X(:)
    
end program