use RandomClass
use FEMDomainClass
implicit none

type(Random_) :: random
type(FEMDomain_) :: domain,element
integer(int32) :: i

domain%mesh%nodcoord =zeros(1,3) 

domain%mesh%nodcoord(1,1) = random%random()
domain%mesh%nodcoord(1,2) = random%random()
domain%mesh%nodcoord(1,3) = random%random()

call domain%mesh%meshing(mode=3)

do i=1,domain%ne()
    element = domain%getElement(i)
    call element%vtk("Element"//str(i) )
enddo

call domain%msh("test")
call domain%vtk("test")

end