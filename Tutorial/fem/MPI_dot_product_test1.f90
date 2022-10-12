program main
    use FEMSolverClass
    implicit none

    real(real64),allocatable :: RHS(:)

    type(FEMSolver_) :: solver
    type(MPI_) ,target:: mpid
    real(real64) :: dbug_val
    integer(int32),parameter :: n = 3
    integer(int32) :: i

    call mpid%start()

    ! solve
    solver%MPI_target => mpid

    ! rank 0 - row 10 <=> rank 1 - row 1
    do i=1,mpid%petot
        call solver%MPI_link([i-1,3],[i,1])
    enddo

    RHS = eyes(n)
    RHS(2) = RHS(2)+mpid%myrank


    dbug_val = 1
    do i = 1, mpid%petot
        dbug_val = dbug_val + i*i
        dbug_val = dbug_val + 1
    enddo

    print *, mpid%myrank,":",RHS
    print *, "this answer : ",solver%MPI_dot_product(RHS,RHS),"true answer :",dbug_val
    


    call mpid%end()
    
end program main