use RandomClass
use FEMDomainClass
implicit none

type(Random_) :: random
type(FEMDomain_) :: domain,element
integer(int32) :: i

domain%mesh%nodcoord =zeros(300,3) 

do i=1,300
domain%mesh%nodcoord(i,1) = random%random()
domain%mesh%nodcoord(i,2) = random%random()
domain%mesh%nodcoord(i,3) = random%random()
enddo
call domain%delaunay3D()

do i=1,domain%ne()
    element = domain%getElement(i)
    call element%vtk("Element"//str(i) )
enddo

call domain%msh("test")
call domain%vtk("test")

end