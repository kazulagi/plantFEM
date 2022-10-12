program main
    use FEMSolverClass
    implicit none

    type(COO_) :: Test_Matrix_COO
    type(CRS_) :: Test_Matrix_CRS
    real(real64),allocatable :: RHS(:)

    type(FEMSolver_) :: solver
    type(MPI_) ,target:: mpid
    integer(int32),parameter :: n = 3
    integer(int32) :: i

    call mpid%start()

    solver%MPI_target => mpid

    if(solver%mpi_target%petot /=3)then
        call print("This bench is for -np 3")
        stop
    endif

    ! rank 0 - row 10 <=> rank 1 - row 1
    do i=1,mpid%petot
        call solver%MPI_link([i-1,3],[i,1])
    enddo

    RHS = eyes(n)
    RHS(2) = RHS(2)+mpid%myrank

    call Test_Matrix_COO%init(3)
    call Test_Matrix_COO%update(row=1,col=1,val=dble(mpid%myrank+1))
    call Test_Matrix_COO%update(row=2,col=2,val=dble(mpid%myrank+1))
    call Test_Matrix_COO%update(row=3,col=3,val=dble(mpid%myrank+1))
    
    Test_Matrix_CRS = Test_Matrix_COO%to_CRS()

    print *, mpid%myrank,":",RHS
    print *, mpid%myrank,":","this answer : ",solver%MPI_matmul(Test_Matrix_CRS,RHS)
    !matrix = 
    ![1 , 0, 0, 0, 0, 0, 0]
    ![0 , 1, 0, 0, 0, 0, 0]
    ![0 , 0, 3, 0, 0, 0, 0]
    ![0 , 0, 0, 2, 0, 0, 0]
    ![0 , 0, 0, 0, 5, 0, 0]
    ![0 , 0, 0, 0, 0, 3, 0]
    ![0 , 0, 0, 0, 0, 0, 3]
    !vector = 
    ![1 , 1, 1, 2, 1, 3, 1]
    ! matrix*vector
    ![1 , 1, 3, 4, 5, 9, 3]

    call mpid%end()
    
end program main