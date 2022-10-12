use FEMDomainClass
implicit none

type(FEMDomain_) :: origin_obj, guess_obj
real(real64),allocatable :: x(:),points(:,:)

call origin_obj%create("Cube3D")
x = origin_obj%centerPosition() 
call origin_obj%move(x=x(1),y=x(2),z=x(3) )
call origin_obj%resize(x=2.0d0,y=0.30d0,z=0.50d0)
call origin_obj%rotate(x=degrees(2.0d0),y=degrees(1.0d0),z=degrees(5.0d0))
call origin_obj%vtk("origin_obj")

! guess it!
call guess_obj%create("Cube3D",x_num=20,y_num=20,z_num=200)
! point cloud
call guess_obj%fit(x=origin_obj%x(),y=origin_obj%y(),z=origin_obj%z() )
call guess_obj%vtk("guess_obj")

end