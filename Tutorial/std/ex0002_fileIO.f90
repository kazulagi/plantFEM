program main
    use std ! standard package of plantFEM library
    implicit none

    ! This example utilizes Input-Output Class, where
    ! You can handle external files.

    ! How to use:

    ! First, create the instance

    type(IO_) :: f ! file-IO instance
    integer(int32) :: i, num ! int i and int num
    

    ! #1 create, edit and close files.
    ! ----> open file(filepath, filename, extention)
    call f%open("./test.txt",'w')
    ! write something
    call f%write(100.0d0)
    write(f%fh,*) 100.0d0
    ! and close it
    call f%close()



    ! ----> create sequential files (filepath, filename, extention)
    ! it creates
    ! ./hello1.txt
    ! ./hello2.txt
    ! ./hello3.txt
    ! ...
    ! ./hello10.txt

    ! for i=1; i<11;i++
    do i=1,10
        !    f.open(filepath, filename, extention)
        !    str(int) => string
        call f%open("./hello"//str(i)//".txt",'w')
        ! This
        call f%write(i)
        ! and this
        write(f%fh,*) str(i)
        ! are same 
        call f%close()

        call f%open("./hello"//str(i)//".txt",'r')        
        ! read a line
        read(f%fh,*) num
        ! print(num)
        print *, num
        call f%close()
    enddo

    ! Importance Index 7 / 10 : [*******   ]

    
end program 