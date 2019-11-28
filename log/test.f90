program main
    use mpi

    implicit none

    integer :: i,j,n
    call mpi_init(i)
    call mpi_comm_size(mpi_comm_world,j ,i)
    call mpi_comm_rank(mpi_comm_world,n,i)
    print *, "j", j,n

end program 