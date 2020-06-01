program main
    use MPIClass
    implicit none
    type(MPI_) :: mpid
    call mpid%start()
    call mpid%createFileName("../result","res_")
    print *, trim(mpid%Name)
    open(10,file=trim(mpid%Name))
    write(10,*) "hello, world!"
    close(10)
    call mpid%end()
end program 