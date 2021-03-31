program main
    use FEMDomainClass
    implicit none

    type(FEMDomain_) :: domain1, domain2, interface1,interface2 ! FEM data-structure

    ! create objects
    call domain1%create(meshtype="rectangular3D",x_num=10,y_num=10,z_num=10,&
    x_len=10.0d0,y_len=10.0d0,z_len=10.0d0)

    call domain2%create(meshtype="rectangular3D",x_num=10,y_num=10,z_num=10,&
    x_len=10.0d0,y_len=10.0d0,z_len=10.0d0)
    call domain2%move(x=9.00d0)
    call domain2%rotate(x=1.00d0,y=1.0d0,z=1.0d0)

    ! create interfaces
    call interface1%create(meshtype="Node-To-Node",master=domain1,slave=domain2)
    call interface2%create(meshtype="Node-To-Node",master=domain2,slave=domain1)



    ! show uuids
    print *, "domain1-uuid : "      ,domain1%uuid
    print *, "domain1-slave : "     ,domain1%link(1)

    print *, "domain2-uuid : "      ,domain2%uuid
    print *, "domain2-slave : "     ,domain2%link(1)
    
    print *, "interface1-uuid : "   ,interface1%uuid
    print *, "interface1-master : " ,interface1%link(1)
    print *, "interface1-slave : "  ,interface1%link(2)

    print *, "interface2-uuid : "   ,interface2%uuid
    print *, "interface2-master : " ,interface2%link(1)
    print *, "interface2-slave : "  ,interface2%link(2)

    ! show mesh
    call domain1%msh("domain1")
    call domain2%msh("domain2")
    call interface1%msh("interface1")
    call interface2%msh("interface2")   
    
end program main 
