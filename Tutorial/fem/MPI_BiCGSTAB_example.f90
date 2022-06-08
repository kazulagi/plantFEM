program main
    use MPIClass
    use COOClass
    use FEMSolverClass
    implicit none

    type(COO_) :: A_COO,Test_Matrix_COO
    type(CRS_) :: A_CRS,Test_Matrix_CRS
    real(real64),allocatable :: x(:),RHS(:)

    type(FEMSolver_) :: solver
    type(MPI_) ,target:: mpid
    real(real64) :: dbug_val
    integer(int32),parameter :: n = 3
    integer(int32) :: i

    call mpid%start()

    call A_COO%init(num_row=n)
    
    ! for each process, get matrix and RHS
    ! Poisson Matrix
    do i=1,A_COO%DOF()
        call A_COO%add(row=i, col=i-1, val=-1.0d0)
        call A_COO%add(row=i, col=i+0, val= 2.0d0)
        call A_COO%add(row=i, col=i+1, val=-1.0d0)
    enddo

    ! for each process, get matrix and RHS
    ! Right-hand side vector
    if(mpid%myrank==0)then
        RHS = zeros(n)
        RHS(1) = 4.0d0
        RHS(2) = 4.0d0
        do i=1,A_COO%DOF()
            call A_COO%update(row=i, col=1, val=0.0d0)
            call A_COO%update(row=1, col=i, val=0.0d0)
        enddo
        call A_COO%update(row=1, col=1, val=1.0d0)
        
    else
        RHS = zeros(n)
        if(mpid%myrank+1==mpid%petot)then
            RHS(size(RHS) )=1.0d0
        endif
    endif

    call A_COO%update(row=A_COO%DOF(), col=A_COO%DOF(), val=1.0d0)
    call A_COO%update(row=1, col=1, val=1.0d0)
    
    A_CRS = A_COO%to_CRS()

    ! solve
    solver%MPI_target => mpid

    do i=1,mpid%petot
        call solver%MPI_link([i-1,3],[i,1])
    enddo

    call solver%setCRS(A_CRS)
    call solver%setRHS(RHS)
    x = zeros(n)

    solver%debug=.true.
    call print(A_CRS%to_Dense() )

    !i\j 1	2	3	4	5	6	7
    !1	 1	 0	0	0	0	0	0
    !2	 0	 2	-1	0	0	0	0
    !3	 0	 -1	2	-1	0	0	0
    !4	 0	 0	-1	2	-1	0	0
    !5	 0	 0	0	-1	2	-1	0
    !6	 0	 0	0	0	-1	2	-1
    !7	 0	 0	0	0	0	-1	1

    !RHS
    !4
    !4
    !0
    !0
    !0
    !0
    !2

    !x	解
    !x1	4
    !x2	5
    !x3	6
    !x4	7
    !x5	8
    !x6	9
    !x7	10

    call solver%MPI_BiCGSTAB(x)
    
    
    print *, mpid%myrank," : ",x," error_norm:",norm(solver%MPI_matmul(b=x)-rhs)

    call mpid%end()
    
end program main