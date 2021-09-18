use FEMDomainClass
implicit None

type(FEMDomain_) :: Cube
type(IO_) :: f
real(real64) :: a(3)=0.0d0 ! acceleration
real(real64),allocatable :: some_global_vector(:)

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

! Stiffness matrix for an Element
call f%open("StiffnessMatrix.txt",'w')
call f%write(cube%StiffnessMatrix(ElementID=1, E=1000.0d0, v=0.30d0))
call f%close()

! B-matrix (Strain-displacement consistency) for an Element
call f%open("BMatrix.txt",'w')
call f%write(cube%BMatrix(ElementID=1))
call f%close()

! Elastic-Coefficient matrix for an Element
call f%open("DMatrix.txt",'w')
call f%write(cube%DMatrix(E=1000.0d0, v=0.30d0))
call f%close()

! Diffusion matrix for an Element
call f%open("DiffusionMatrix.txt",'w')
call f%write(cube%DiffusionMatrix(ElementID=1,D=1.0d0))
call f%close()

! create sequential vector (0, 1, 2, ...)
some_global_vector = arange(cube%nn())
! Element-wize vector for an Element
! For example, body force N with 3-D space
call f%open("ElementVector.txt",'w')
call f%write(cube%ElementVector(ElementID=1,GlobalVector=some_global_vector,DOF=1))
call f%close()


end