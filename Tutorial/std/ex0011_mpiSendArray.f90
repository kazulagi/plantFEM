program main
    use std
    implicit none

    type(MPI_) :: mpid

    real(real64),allocatable :: vector(:)

    call mpid%start()

    ! create vector
    if(mpid%myrank == 0)then
        allocate(vector(3))
        vector(1)=1
        vector(2)=2
        vector(3)=3
    endif

    print *, "Is vector allocated? :: ",allocated(vector)

    call mpid%Bcast(From=0, val=vector)

    print *, "Is vector allocated? :: ",allocated(vector)

    call mpid%end()
end program main