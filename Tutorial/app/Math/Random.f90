program main
    use std
    implicit none
    type(Random_) :: r
    type(IO_) :: f
    integer(int32) :: i

    call f%open("./","data",".txt")
    call r%init()
    do i=1,2000
        write(f%fh,*) r%random()
    enddo
    call f%close()
    
end program 