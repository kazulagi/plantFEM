!use SoilClass
use BoringClass
implicit none

!type(Soil_) :: soil
type(Boring_) :: boring
type(IO_) :: f

!call soil%init(x_num=100,y_num=100,z_num=50)
!call soil%vtk("soil")
call boring%example()
print *, boring%URL
call f%open("N_value.txt","w")
do i_i=1,100
    !write(f%fh,*) -dble(i_i)/10.0d0, boring%getN(depth=-dble(i_i)/10.0d0)
    write(f%fh,*) 60.0d0-dble(i_i)/10.0d0, &
        boring%getN(elevation=60.0d0-dble(i_i)/10.0d0)
enddo
call f%close()
call f%plot(option="with lines")



end 