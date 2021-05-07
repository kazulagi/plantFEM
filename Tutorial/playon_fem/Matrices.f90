use FEMDomainClass
implicit None

type(FEMDomain_) :: Cube
type(IO_) :: f
real(real64) :: a(3)=0.0d0 ! acceleration

call cube%create(meshtype="Cube3D")
call cube%resize(x=10.0d0)
call cube%resize(y=10.0d0)
call cube%resize(z=10.0d0)

! Mass-Matrix for an Element
call f%open("MassMatrix.txt",'w')
! ElementID=1, Degree of freedom in a node is 3
! 3 unknowns are defined in a node.
call f%write(cube%MassMatrix(ElementID=1, DOF=3))
call f%close()

! Mass-Vector for an Element
! For example, body force N with 3-D space
call f%open("MassVector.txt",'w')
a(3) = -9.80d0
call f%write(cube%MassVector(ElementID=1, DOF=3, Density=1.0d0,Accel=a) )
call f%close()

end