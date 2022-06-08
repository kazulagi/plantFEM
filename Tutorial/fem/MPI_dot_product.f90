program main
    use FEMSolverClass
    implicit none

    real(real64),allocatable :: RHS(:)

    type(FEMSolver_) :: solver
    type(MPI_) ,target:: mpid
    integer(int32),parameter :: n = 3

    call mpid%start()

    ! solve
    solver%MPI_target => mpid

    ! rank 0 - row 10 <=> rank 1 - row 1
    call solver%MPI_link([0,3],[1,1])
    call solver%MPI_link([1,3],[2,1])
    call solver%MPI_link([2,3],[3,1])

    RHS = eyes(n)
    call print(solver%Link_Table(1)%rank_and_rowID_2)
    print *, solver%MPI_dot_product(RHS,RHS)

    call mpid%end()
    
end program main