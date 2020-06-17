program main
    use std ! standard package of SiCroF library
    implicit none

    type(IO_) :: f ! file-IO instance
    integer(int32) :: i, num ! int i and int num
    
    ! for i=1; i<11;i++
    do i=1,10
        !    f.open(filepath, filename, extention)
        !    str(int) => string
        call f%open("./","hello"//trim(str(i)),".txt")
        
        ! This
        call f%write(str(i))
        ! and this
        write(f%fh,*) str(i)
        ! are same 

        call f%close()

        call f%open("./","hello"//trim(str(i)),".txt")        
        ! read a line
        read(f%fh,*) num
        ! print(num)
        print *, num

        call f%close()


    enddo
    
end program 