use MeshClass
use FEMDomainClass
implicit none

type(FEMDomain_) :: cube
type(IO_) :: f
integer(int32),allocatable :: GroupID(:,:)
real(real64),allocatable :: Element_Group_ID(:)
integer(int32) :: i,j

call cube%create("Cube3D",x_num=200,y_num=200,z_num=100)
call cube%resize(x=200.0d0)
call cube%resize(y=200.0d0)
call cube%resize(z=100.0d0)

! Binary-Tree Search for 8-node isoparametric elements
GroupID = cube%mesh%BinaryTreeSearch(old_GroupID=GroupID,min_elem_num=10000)
Element_Group_ID = int(zeros(cube%ne() ) )
do i=1,size(GroupID,1)
    do j=1,size(GroupID,2)
        Element_Group_ID(GroupID(i,j)) = dble(i)
    enddo
enddo

! Number of Group, Number of Elements in a group
print*, size(GroupID,1),size(GroupID,2)

! visualize
call cube%vtk("groups",scalar=Element_Group_ID)

! export
!call f%open("BinaryTree.txt")
!call f%write( GroupID)
!call f%close()



end