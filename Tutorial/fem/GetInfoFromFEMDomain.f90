use FEMDomainClass
implicit none

type(FEMDomain_),target :: cube
type(IO_) :: f
real(real64),allocatable :: GlobalVector(:),ElementVector(:)

! create Domain
call cube%create(meshtype="Cube3D",x_num=5,y_num=10,z_num=5)
! test code for GlobalVector
ElementVector = arange( cube%nne()*cube%nd() )
call cube%GlobalVector(&
    ElementID=10, &
    ElementVector=ElementVector, &
    GlobalVector=GlobalVector, &
    DOF=cube%nd() )
ElementVector = arange( cube%nne()*cube%nd() )
call cube%GlobalVector(&
    ElementID=22, &
    ElementVector=ElementVector, &
    GlobalVector=GlobalVector, &
    DOF=cube%nd() )
!call print(reshape(ElementVector,cube%nne(),cube%nd() ) ) 
!stop

call print("Element #10 consists of Nodes ID")
call print( cube%connectivity(10) )

call print("Element #22 consists of Nodes ID")
call print( cube%connectivity(22) )

call f%open("GlobalVector.txt")
call f%write(reshape(GlobalVector,cube%nn(),cube%nd() ) )
call f%close()


end