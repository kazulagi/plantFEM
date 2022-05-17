program main
    use SeepageFlowClass
    implicit none

    type(SeepageFlow_) :: sim
    type(FEMDomain_) :: soil,plate,box
    real(real64),allocatable :: PorePressure(:),Velocity(:,:)

    call soil%create("Cube3D",x_num=20,y_num=20,z_num=20)
    call soil%resize(x=10.0d0,y=10.0d0,z=10.0d0)
    
    call plate%create("Cube3D",x_num=3,y_num=10,z_num=10)
    call plate%resize(x=0.70d0,y=10.0d0,z=10.0d0)
    call plate%move(x=5.0d0,z=2.00d0)

    call box%create("Cube3D",x_num=3,y_num=3,z_num=3)
    call box%resize(x=5.0d0,y=10.0d0,z=5.0d0)
    call box%move(x=5.50d0,z=5.00d0)
    call box%vtk("box")

    call soil%boolean(object=plate,difference=.True.)
    call soil%boolean(object=box,difference=.True.)
    
    call plate%vtk("plate")
    call box%vtk(  "box")
    call soil%vtk( "cut")    

    call plate%remove()
    call box%remove()

    ! Or you can directly kill nodes
    !call soil%killNodes( soil%select(x_min=5.0d0, x_max=5.50d0, z_min=2.0d0 ) )
    !call soil%killNodes( soil%select(x_min=5.50d0, z_min=5.0d0 ) )


    ! or you can read 
    !call soil%read("your_awesome_mesh.vtk")
    
    ! initialize simulator
    call sim%init(soil,model="Darcy",Permiability=full(soil%nn(),1.0d0 ))


    ! Boundary condition
    call sim%fixPressureBoundary(&
            NodeList = soil%select(x_max=5.0d0,z_min=soil%z_max() ) , &
            pressure = 1.0d0 &
        )
    call sim%fixPressureBoundary(&
        NodeList = soil%select( x_min=5.50d0,z_min=4.0d0 ) ,  &
        pressure = 0.0d0 &
    )

    call sim%fixPressureBoundary(&
        NodeList = soil%select(x_max=2.0d0 ) ,  &
        pressure = 1.0d0 &
    )

    call sim%fixPressureBoundary(&
        NodeList = soil%select(x_min=soil%x_max() ) ,  &
        pressure = 0.0d0 &
    )
    print *, "solve"
    
    PorePressure = sim%getPressure(debug=.true.)
    Velocity     = sim%getVelocity(Pressure=PorePressure,Permiability=1.0d0)
    
    ! Compute pore-pressure
    ! visualization
    ! pressure
    call soil%vtk("result_p",scalar=PorePressure)
    ! velocity
    call soil%vtk("result_v",vector=Velocity)

end program main