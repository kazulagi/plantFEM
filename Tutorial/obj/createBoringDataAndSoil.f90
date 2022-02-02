use BoringClass
use SoilClass
implicit none

type(Boring_) :: boring(3)
type(Soil_)   :: soil
type(IO_) :: f
real(real64),allocatable :: Nvalue(:)

call boring(1)%setN(depth=-20.0d0,Nvalue=10.0d0)
call boring(1)%setN(depth=-40.0d0,Nvalue=30.0d0)
call boring(1)%setN(depth=-50.0d0,Nvalue=10.0d0)
call boring(1)%setN(depth=-60.0d0,Nvalue=50.0d0)
call boring(1)%setN(depth=-10.0d0,Nvalue=20.0d0)
call boring(1)%setN(depth=-18.0d0,Nvalue=10.0d0)

call boring(2)%setN(depth=-30.0d0,Nvalue=20.0d0)
call boring(2)%setN(depth=-80.0d0,Nvalue=50.0d0)


call boring(3)%setN(depth=-30.0d0,Nvalue=20.0d0)
call boring(3)%setN(depth=-40.0d0,Nvalue=10.0d0)
call boring(3)%setN(depth=-100.0d0,Nvalue=50.0d0)

boring(1)%x = 0.0d0
boring(1)%y = 0.0d0


boring(2)%x = 400.0d0
boring(2)%y = 300.0d0


boring(3)%x = -300.0d0
boring(3)%y = 300.0d0

call f%open("untitled_boring1.txt")
do i_i=1,100
    write(f%fh,*) boring(1)%getN(depth=-dble(i_i) ), -dble(i_i)
enddo
call f%close()
call f%plot(option="with lines")

call f%open("untitled_boring2.txt")
do i_i=1,100
    write(f%fh,*) boring(2)%getN(depth=-dble(i_i) ), -dble(i_i)
enddo
call f%close()
call f%plot(option="with lines")


call f%open("untitled_boring3.txt")
do i_i=1,100
    write(f%fh,*) boring(3)%getN(depth=-dble(i_i) ), -dble(i_i)
enddo
call f%close()
call f%plot(option="with lines")



call soil%init(x_num=100,y_num=100,z_num=20)
call soil%resize(x=1000.0d0,y=1000.0d0,z=200.0d0)
Nvalue = soil%getNvalue(Borings=boring)
call soil%vtk("soil",scalar=Nvalue)

end 