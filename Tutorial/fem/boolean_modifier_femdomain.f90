use RangeClass
use FEMDomainClass
implicit none

type(FEMDomain_) :: domain
real(real64),allocatable :: val(:)
type(Range_) :: myrange

! Boolean modifier
call domain%cube(100,100,100)
call domain%resize(25.0d0,25.0d0,25.0d0)
call domain%move(to="Center")
call domain%move(z=-5.0d0)


myrange = (to_range(p1=[0.0d0,0.0d0,-10.0d0],p2=[0.0d0,0.0d0,10.0d0],radius=[5.00d0,1.00d0]) &
        .and. to_range(p1=[0.0d0,0.0d0,-100.0d0],p2=[0.0d0,0.0d0,100.0d0],radius=2.40d0) ) &
    .or. to_range(p1=[-100.0d0,0.0d0,-10.0d0],p2=[100.0d0,0.0d0,-8.0d0],radius=2.50d0)

call domain%setVectorValue(vector=val,dof=1,fillValue=1.0d0,&
    range=myrange)

call domain%killNodes( getIdx(val,1.0d0) )

call domain%vtk("obj")


end