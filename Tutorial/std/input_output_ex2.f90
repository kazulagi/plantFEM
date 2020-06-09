program main
    use std
    implicit none
    type(IO_) :: f
    integer(int32) :: i

    do i=1,1000
        call f%open("./","hello"//trim(str(i)),".txt")
        call f%write(str(i))
        call f%close()
    enddo
    
end program 