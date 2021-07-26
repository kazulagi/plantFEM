use PlantFEM
implicit none

integer(int32) :: i
real(real64)   :: a_n, a
type(IO_) :: f

! create a sequence
call f%open("sequence.txt","w")
! a_1 = 1.0
a_n=1.0d0
do i=1,100000
    ! a_{n+1} = a_{n} + 1 / a_{n}
    a = a_n + 1.0d0/a_n
    write(f%fh,*) i, a
    a_n = a
enddo
call f%close()

! plot 2-D graph without lines
call f%plot("sequence.txt")

! plot 2-D graph with lines
call f%plot("sequence.txt","with line")

end