use std
use FEMDomainClass
implicit none

type(IO_) :: f
type(FEMDomain_) :: domain
type(Math_) :: math
character(:),allocatable :: line
real(real64) :: x, y, r ,theta,x_sum,y_sum,center(2),max_r,coord(2), ret
real(real64),allocatable :: r_data(:),theta_data(:),leafSurface(:,:),tx(:),tfx(:)
integer(int32) :: num_ptr, i,id,ids(5),id_n
character(:),allocatable :: filename

!filename = "oak.txt"
!filename = "LeafSurface.txt"
filename = "grape_leaf.txt"
call f%open(filename,"r")
! get brief info
num_ptr = 0

x_sum = 0.0d0
y_sum = 0.0d0

do 
    line = f%readline()
    if(f%EOF) exit
    num_ptr = num_ptr+1
    ! read x-y
    read(line,*) x, y
    x_sum = x_sum + x
    y_sum = y_sum + y
enddo
call f%close()

center(1) = x_sum/dble(num_ptr)
center(2) = y_sum/dble(num_ptr)

r_data = zeros(num_ptr)
theta_data = zeros(num_ptr)
leafSurface = zeros(num_ptr,2)

! get detail
call f%open(filename,"r")
num_ptr=0
do 
    line = f%readline()
    if(f%EOF) exit
    ! read x-y
    read(line,*) x, y
    
    coord(1) = x - center(1)
    coord(2) = y - center(2)
    r = sqrt( dot_product(coord,coord) )
    theta = angles( coord )

    num_ptr = num_ptr + 1
    leafSurface(num_ptr,1) = x
    leafSurface(num_ptr,2) = y

    r_data(num_ptr) = r
    theta_data(num_ptr) = theta 
enddo
max_r = maxval(r_data)
r_data = r_data/max_r
call f%close()




call domain%create("Cylinder3D",x_num=10,y_num=10)
call domain%resize(x=2.0d0)
call domain%resize(y=2.0d0)
call domain%resize(z=0.010d0)

! ####################################
! test interpolate


!tx = [0.0d0, 1.0d0, 2.0d0, 3.0d0]
!tfx = [0.0d0, 2.0d0, 4.0d0, 8.0d0]
!ret = interpolate(x =tx,Fx=tfx,x_value = -0.50d0)
!print *, ret
!stop
! ####################################

! adjust shape
do i=1,domain%nn()
    x = domain%mesh%nodcoord(i,1)
    y = domain%mesh%nodcoord(i,2)
    r = sqrt(x**2 + y**2)
    coord(1:2) = domain%mesh%nodcoord(i,1:2)
    r = norm(coord)
    theta = angles(coord)
    ! find nearest theta
    r = r * interpolate(x=theta_data,Fx=r_data,x_value=theta)
    x = r*x
    y = r*y
    domain%mesh%nodcoord(i,1) = x 
    domain%mesh%nodcoord(i,2) = y 
enddo

! export data
call f%open("theta_r_relation.txt","w")
do i=1,size(r_data)
    call f%write(theta_data(i),r_data(i) )
enddo
call f%close()
call f%plot("theta_r_relation.txt","w l")
call f%plot(filename,"w l")

call domain%vtk("leaf")



end