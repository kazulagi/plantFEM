use RandomClass
use FEMDomainClass
implicit none

type(Random_) :: random
type(FEMDomain_) :: domain
integer(int32) :: i

domain%mesh%nodcoord =zeros(100,2) 

domain%mesh%nodcoord(1,:) = [-1.0d0,-1.0d0] 
domain%mesh%nodcoord(2,:) = [ 2.0d0,-1.0d0] 
domain%mesh%nodcoord(3,:) = [ 2.0d0, 2.0d0] 
domain%mesh%nodcoord(4,:) = [-1.0d0, 2.0d0] 

do i=5,domain%nn()
    domain%mesh%nodcoord(i,1) = random%random()
    domain%mesh%nodcoord(i,2) = random%random()
enddo
call domain%meshing()
call domain%mesh%convertTriangleToRectangular()
call domain%mesh%Laplacian()
call domain%mesh%convert2Dto3D()
call domain%msh("test")
call domain%vtk("test")

end