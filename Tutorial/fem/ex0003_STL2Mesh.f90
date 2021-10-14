program main
    use STLClass

    implicit none
    
    type(STL_) :: stl
    type(IO_) :: f
    character(20) :: char
    integer(int32) :: i,j

    call stl%import("test.stl")

    call f%open("test.txt")
    do i=1,size(stl%facet,1)
        do j=1,size(stl%facet,2)
            write(f%fh,*) stl%facet(i,j,:) 
        enddo
    enddo
    call f%write("pause -1")
    call f%close()

    call f%open("test.gp")
    call f%write("splot 'test.txt' w l")
    call f%close()
    call execute_command_line("gnuplot test.gp -p")

end program