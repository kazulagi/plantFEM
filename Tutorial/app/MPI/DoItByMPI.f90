program main
    use std ! use standard libary of plantfem
    implicit none
    ! start
    type(MPI_) :: mpid
    integer(int32) :: fh ! file handle (id)

    ! start MPI
    call mpid%start()
    ! >>>>>>>> do parallel

    ! create sequential file
    call mpid%createFileName("../result","res_")
    ! mpid%name has one of the sequential file name.
    print *, trim(mpid%Name)

    ! open the file
    open(newunit=fh,file=trim(mpid%Name))
    ! write hello world.
    write(fh,*) "hello, world!"
    ! close file
    close(fh)

    ! <<<<<<<< end do parallel
    call mpid%end()
end program 