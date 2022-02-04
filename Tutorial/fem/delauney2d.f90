use RandomClass
use FEMDomainClass
implicit none

type(Random_) :: random
type(FEMDomain_) :: domain
integer(int32) :: i

domain%mesh%nodcoord =zeros(10,2) 

! set nodes
domain%mesh%nodcoord(1,:) = [-1.0d0,-1.0d0] 
domain%mesh%nodcoord(2,:) = [ 2.0d0,-1.0d0] 
domain%mesh%nodcoord(3,:) = [ 2.0d0, 2.0d0] 
domain%mesh%nodcoord(4,:) = [-1.0d0, 2.0d0] 
do i=5,domain%nn()
    domain%mesh%nodcoord(i,1) = random%random()
    domain%mesh%nodcoord(i,2) = random%random()
enddo

! mesh Delaunay 2D
call domain%meshing()

! Triangle to Rectangle
call domain%mesh%convertTriangleToRectangular()

! Modify shape
call domain%mesh%Laplacian()

! 2D to 3D
call domain%mesh%convert2Dto3D()

call domain%vtk("test")

end