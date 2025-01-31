use RangeClass
use FEMDomainClass
implicit none

type(FEMDomain_) :: domain
real(real64),allocatable :: val(:)

! Set FEMDomain's scalar/vector values by range
! select nodes
! by range
call domain%cube(200,200,200)
call domain%resize(100.0d0,100.0d0,50.0d0)
call domain%move(to="Center")
call domain%move(z=-5.0d0)

! <Sphere>
call domain%setVectorValue(vector=val,dof=1,fillValue=1.0d0,&
    range=to_range(center=[0.0d0,0.0d0,0.0d0],radius=14.0d0))

! <Cylinder>
call domain%setVectorValue(vector=val,dof=1,fillValue=-1.0d0,&
    range=to_range(p1=[-50.0d0,0.0d0,-10.0d0],p2=[50.0d0,0.0d0,-10.0d0],radius=2.40d0))

! <Box>
!call domain%setVectorValue(vector=val,dof=1,fillValue=1.0d0,&
!    range=to_range(x_min=-1.0d0,x_max=4.50d0,y_min=0.0d0,y_max=1.0d0,z_min=-2.0d0,z_max=3.0d0))

! <Cone>
call domain%setVectorValue(vector=val,dof=1,fillValue=1.0d0,&
    range=to_range(p1=[0.0d0,0.0d0,-10.0d0],p2=[0.0d0,0.0d0,10.0d0],radius=[2.40d0,20.00d0]))

call domain%killNodes( getIdx(val,1.0d0) )

call domain%vtk("obj")


end