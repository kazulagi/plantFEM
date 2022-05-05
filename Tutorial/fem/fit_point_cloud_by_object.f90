use FEMDomainClass
implicit none

type(FEMDomain_) :: origin_obj, guess_obj
type(Random_) :: random
real(real64),allocatable :: x(:),y(:),z(:)
type(IO_) :: f

x = random%white(1000,mu=5.0d0, sigma = 1.0d0)
y = random%white(1000,mu=-2.0d0,sigma = 1.0d0)
z = random%white(1000,mu=3.0d0, sigma = 1.0d0)

call f%open("xyz.txt","w")
do i_i=1,size(x)
    write(f%fh,*) x(i_i),y(i_i),z(i_i)
enddo
call f%close()
call f%splot("xyz.txt")

! guess it!
call guess_obj%create("Cube3D",x_num=20,y_num=20,z_num=200)
call guess_obj%vtk("init_obj")
! point cloud
call guess_obj%fit(x=x,y=y,z=z,debug=.true. )
call guess_obj%vtk("guess_obj")

end