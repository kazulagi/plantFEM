use PlantFEM
implicit none

integer(int32) :: i
type(Random_) :: random
type(IO_) :: f

! create a gaussian
call f%open("gaussian.txt","w")
do i=1,10000
    ! white gauss noizse
    write(f%fh,*) i, random%gauss(mu=0.0d0, sigma=0.2d0)
enddo
call f%close()

! plot 2-D graph with lines
call f%plot("gaussian.txt","with line")

end