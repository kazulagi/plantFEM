program main
    use SeepageFlowClass
    implicit none

    type(SeepageFlow_) :: sim
    type(FEMDomain_) :: soil
    real(real64),allocatable :: PorePressure(:),Velocity(:,:)

    call soil%create("Cube3D",x_num=10,y_num=10,z_num=10)
    call soil%rotate(x=radian(10.0d0) ,z=radian(10.0d0) )
    ! or you can read 
    !call soil%read("your_awesome_mesh.vtk")
    
    ! initialize simulator
    call sim%init(soil,model="Darcy",Permiability=full(soil%nn(),1.0d0 ))

    ! Boundary condition
    call sim%fixPressureBoundary(&
            NodeList = soil%select(x_max=soil%x_min()+ 0.10d0) , &
            pressure = -1.0d0 &
        )
    call sim%fixPressureBoundary(&
        NodeList = soil%select(x_min=soil%x_max() - 0.10d0) ,  &
        pressure = 1.0d0 &
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