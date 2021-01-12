program main
    use FEMDomainClass

    type(FEMDomain_)::omega
    real(8),allocatable :: nod_coord(:,:)

    call ImportArray(nod_coord,FileName="test.mesh")
    call omega%Mesh%ImportNodCoord(nod_coord)
    call omega%Meshing()

end program main