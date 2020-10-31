program main
    use plantFEM
    implicit none

    type(FEMDomain_) :: domain

    ! Create mesh-object and introduce physical fields
    call domain%create(meshtype="Cube")
    call domain%resize(x=10.0d0, y=2.0d0,z=5.0d0)

    ! add physical fields
    call domain%addLayer(name="Temperature",attribute="Nodal",datastyle="scalar")
    call domain%addLayer(name="Chauchy-Stress",attribute="Gausspoint",datastyle="Tensor")
    call domain%addLayer(name="Displacement",attribute="Nodal",datastyle="Vector",VectorRank=3)
    
    ! show information of physical fields
    call domain%showLayer()
    print *, domain%numoflayer
    print *, trim(domain%physicalfield(1)%name)
    print *, trim(domain%physicalfield(2)%name)
    print *, trim(domain%physicalfield(3)%name)
    
    
end program main

