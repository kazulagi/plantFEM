program main
    use FEMDomainClass
    implicit none

    type(FEMDomain_) :: domain1, domain2
    integer(int32)::field_id

    call domain1%create(meshtype="Cube")
    call domain2%create(meshtype="Cube")

    call domain1%addlayer(name="waterhead",attribute="node",datastyle="scalar")
    call domain2%addlayer(name="waterhead",attribute="node",datastyle="scalar")
    


    field_id=domain1%getLayerID(name="waterhead")
    domain1%physicalfield(field_id)%scalar(:)=2.0d0

    field_id=domain2%getLayerID(name="waterhead")
    domain2%physicalfield(field_id)%scalar(:)=1.0d0
    
    call domain1%projection("=>",domain=domain2,PhysicalField="waterhead",debug=.true.)

    print *, maxval(domain2%physicalfield(field_id)%scalar(:)),minval(domain2%physicalfield(field_id)%scalar(:))

end program main

