use MeshClass
use FEMDomainClass
implicit none

type(FEMDomain_) :: mesh
type(MPI_)  :: mpid
integer(int32) :: master_node_id = 0

call mpid%start()

! If I am a master-node, I create mesh
! Otherwise, I don't have a mesh
if(mpid%myrank == master_node_id)then
    call mesh%create("Cube3D")
endif

! sync for all
call mesh%sync(from = master_node_id, mpid=mpid)

call mesh%move(x=dble(mpid%myrank)*1.20d0 )
call mesh%rotate(x=dble(mpid%myrank)*1.20d0 )


call mesh%vtk("mesh" + str(mpid%myrank) )
call mpid%end()

end 