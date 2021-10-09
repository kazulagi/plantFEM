program main
    use plantFEM
    implicit none
    integer,parameter :: n = 10000
    type(Random_) :: random
    type(MPI_)    :: MPIdata  
    double precision :: a(n)
    integer :: i

    call MPIdata%start()
    call random%init()
    do i=1,n
        a(i)=random%random()
    enddo
    call quicksort(a)
    call MPIdata%end()

end program