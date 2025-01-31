use FEMDomainClass
implicit none

type(FEMDomain_) :: domain
real(real64),allocatable :: t_T(:)

call domain%to_cylinder(10,10,200)
call domain%resize(x=2.0d0,y=2.0d0,z=20.0d0)
call domain%move(to="center")

t_T = domain%TorsionalForce(&
    normal=[0.0d0,0.0d0,1.0d0],&
    center=[0.0d0,0.0d0,0.0d0],&
    range=to_range(z_max=domain%z_min()+1.0d0))

call domain%setVectorValue(&
    vector=t_T,DOF=3,fillValue=0.0d0,&
    range=to_range(&
        p1=[0.0d0,0.0d0,-100.0d0],&
        p2=[0.0d0,0.0d0,100.0d0],&
        radius=0.999d0 &
        ))

call domain%vtk("torsional_force",vector=transpose(reshape(t_T,[3,domain%nn()])))

end