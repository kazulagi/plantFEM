program main
    use FEMDomainClass
    implicit none

    type(FEMDomain_) :: mesh
    integer(int32) :: nearest_node_id

    ! detect nearest node
    call mesh%create(meshtype="Cube")
    call mesh%resize(x=10.0d0,y=10.0d0,z=10.0d0)
    nearest_node_id = mesh%getNearestNodeID(x=1.0d0,y=1.0d0,z=9.0d0)
    print *, nearest_node_id, mesh%position(nearest_node_id)
    
end program main
