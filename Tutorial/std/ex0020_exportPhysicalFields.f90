program main
    use plantFEM
    implicit none

    type(FEMDomain_) :: domain
    type(Random_) :: random
    integer(int32):: i

    ! Create mesh-object and introduce physical fields
    call domain%create(meshtype="Cylinder3D")
    call domain%resize(x=10.0d0, y=2.0d0,z=5.0d0)
    

    ! add physical fields
    call domain%addLayer(name="Temperature",attribute="Nodal",datastyle="scalar")
    do i=1,size(domain%physicalfield(1)%scalar)
        domain%physicalfield(1)%scalar(i) = random%random()
    enddo
    call domain%addLayer(name="Velocity",attribute="Nodal",datastyle="vector")
    do i=1,size(domain%physicalfield(1)%scalar)
        domain%physicalfield(2)%vector(i,:) = random%random()
    enddo
    call domain%addLayer(name="Chauchy-Stress",attribute="Gausspoint",datastyle="Tensor")
    call domain%addLayer(name="Water-Pressure",attribute="Elemental",datastyle="Scalar")
    do i=1,size(domain%physicalfield(4)%scalar)
        domain%physicalfield(4)%scalar(i) = random%random()
    enddo
    call domain%addLayer(name="Displacement",attribute="Nodal",datastyle="Vector",VectorRank=3)
    

    ! Export mesh as .msh files
    call domain%msh("Cube2")
    ! Export fields as .msh files
    call domain%physicalfield(1)%msh(name="temp",caption="temperature")
    call domain%physicalfield(2)%msh(name="velocity",caption="velocity")
    call domain%physicalfield(4)%msh(name="Water-Pressure",caption="Water-Pressure")
    
    ! show information of physical fields
    call domain%showLayer()
    print *, domain%numoflayer
    print *, trim(domain%physicalfield(1)%name)
    print *, trim(domain%physicalfield(2)%name)
    print *, trim(domain%physicalfield(3)%name)
    
    
end program main

