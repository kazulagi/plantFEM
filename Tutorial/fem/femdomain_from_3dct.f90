program main
    use FEMDomainClass
    implicit none

    type(FEMDomain_) :: mesh
    type(IO_) :: pixelPosition
    real(real64),allocatable :: position(:,:)
    integer(int32) :: num_layer,i,j,itr,node1,node2,node3,node4,count,prev_node1
    integer(int32), allocatable :: elemnod(:,:)
    integer(int32) :: nearest_node_id,node_id,elist(2),tri_excep,tri_excep_last
    integer(int32),allocatable :: checked(:),checked_node(:)
    real(real64) :: x,y,z

    ! read plant pixels
    num_layer = pixelPosition%numLine("pixelPosition.txt")
    allocate(position(num_layer,3))

    call pixelPosition%open("pixelPosition.txt")
    do i=1,num_layer
        read(pixelPosition%fh,*) position(i,3),position(i,1),position(i,2)
    enddo
    call pixelPosition%close()

    ! convert them into 3D-Line FEMesh
    call mesh%create(meshtype="root",coordinate=position)

    call mesh%msh("plant")
end program main
