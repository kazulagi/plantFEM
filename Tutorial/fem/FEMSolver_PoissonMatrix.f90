program main
    use MPIClass
    use SparseClass
    use FEMSolverClass
    implicit none

    type(COO_) :: A_COO
    type(CRS_) :: A_CRS
    real(real64),allocatable :: x(:),RHS(:)

    type(FEMSolver_) :: solver
    type(MPI_) :: mpid
    integer(int32),parameter :: n = 100
    integer(int32) :: i

    call mpid%start()

    call A_COO%init(num_row=n)
    
    ! Poisson Matrix
    do i=1,A_COO%DOF()
        call A_COO%update(row=i, col=i-1, val=-1.0d0)
        call A_COO%update(row=i, col=i+0, val= 2.0d0)
        call A_COO%update(row=i, col=i+1, val=-1.0d0)
    enddo
    do i=1,A_COO%DOF()
        call A_COO%update(row=i, col=1, val=0.0d0)
        call A_COO%update(row=1, col=i, val=0.0d0)
    enddo
    call A_COO%update(row=1, col=1, val=1.0d0)

    A_CRS = A_COO%to_CRS()

    ! Right-hand side vector    
    RHS = zeros(n)
    RHS(1) = 10.0d0
    RHS(2) = 10.0d0
    
    ! solve
    call solver%setCRS(A_CRS)
    call solver%setRHS(RHS)  

    solver%debug=.true.
    solver%relative_er = dble(1.0e-5)
    
    x = solver%solve()
    
    call print(x)

    call mpid%end()
    
end program main