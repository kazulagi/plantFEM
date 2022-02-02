!use SoilClass
use BoringClass
use SoilClass
implicit none

!type(Soil_) :: soil
integer(int32) ,parameter :: num_boring_x_y=3
type(Boring_) :: boring(num_boring_x_y*num_boring_x_y)
type(Soil_) :: soil
type(IO_) :: f
type(Random_) :: random
real(real64),allocatable :: Nvalue(:),Vs(:)
integer(int32) :: id

!call soil%init(x_num=100,y_num=100,z_num=50)
!call soil%vtk("soil")
do i_i = 1,num_boring_x_y
    do j_j=1,num_boring_x_y
        id = (i_i-1)*num_boring_x_y + j_j
        call boring(id)%example()
        boring(id)%x = 100.0d0/dble(num_boring_x_y)* dble(i_i) + random%gauss(mu=0.0d0,sigma=10.0d0)
        boring(id)%y = 100.0d0/dble(num_boring_x_y)* dble(j_j) + random%gauss(mu=0.0d0,sigma=10.0d0)
        boring(id)%PTest_NValue = boring(id)%PTest_NValue
        boring(id)%PTest_Depth  = boring(id)%PTest_Depth + 2.0d0*random%random()
    enddo
enddo

do j_j=1,9
    call f%open("N_value_"+str(j_j)+".txt","w")
    do i_i=1,100
        !write(f%fh,*) -dble(i_i)/10.0d0, boring%getN(depth=-dble(i_i)/10.0d0)
        write(f%fh,*) &
            boring(j_j)%getN(depth=-dble(i_i)/10.0d0),&
            -dble(i_i)/10.0d0
    enddo
    call f%close()
    !call f%plot(option="with lines")
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
Nvalue = soil%getNvalue(borings=boring,min_weight_value=0.10d0)
Vs     = soil%convertNvalue2Vs(Nvalue=Nvalue,Formula=PF_N2Vs_JAPANROAD_1)
call soil%femdomain%vtk("soil",scalar=Vs)

end 