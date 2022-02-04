!use SoilClass
use BoringClass
use SoilClass
implicit none

!type(Soil_) :: soil
integer(int32) ,parameter :: num_boring_x_y=10
type(Boring_) :: boring(num_boring_x_y*num_boring_x_y)
type(Soil_) :: soil
type(IO_) :: f
type(Random_) :: random
real(real64),allocatable :: Nvalue(:),Vs(:)
integer(int32) :: id

do i_i = 1,num_boring_x_y
    do j_j=1,num_boring_x_y
        id = (i_i-1)*num_boring_x_y + j_j
        call boring(id)%example()
        boring(id)%x = 100.0d0/dble(num_boring_x_y)* dble(i_i)! + random%gauss(mu=0.0d0,sigma=5.0d0)
        boring(id)%y = 100.0d0/dble(num_boring_x_y)* dble(j_j)! + random%gauss(mu=0.0d0,sigma=5.0d0)
        boring(id)%PTest_NValue = boring(id)%PTest_NValue 
        boring(id)%PTest_Depth  = boring(id)%PTest_Depth &
        + ((boring(id)%x)**2+(boring(id)%y)**2)/1000.0d0
    enddo
enddo


! create ground
call soil%init(x_num=100,y_num=100,z_num=10)
call soil%resize(x=100.0d0)
call soil%resize(y=100.0d0)
call soil%resize(z=20.0d0)
call soil%move(x=50.0d0)
call soil%move(y=50.0d0)
!call soil%move(z=-15.0d0)

!stop
print *, boring(1)%url
Nvalue = soil%getNvalue(borings=boring)
Vs     = soil%convertNvalue2Vs(Nvalue=Nvalue,Formula=PF_N2Vs_JAPANROAD_1)
call soil%femdomain%vtk("soil",scalar=Vs)

call f%open("Vs.txt")
call f%write(Vs)
call f%close()

end 