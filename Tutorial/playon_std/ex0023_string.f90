program main
    use IOClass
    implicit none

    type(IO_) :: f
    type(String_) :: string
    integer(int32) :: i
    
    call f%open("hello.txt")
    call f%write("hello1")
    call f%write("hello2")
    call f%write("hello3")
    call f%write("hello4")
    call f%close()

    ! open file

    call f%open("hello.txt")
    do i=1,10
        string =  f%readline() + string 
        if(f%EOF .eqv. .true.) exit
        call print(string)
    enddo
    call f%close()

end program main
