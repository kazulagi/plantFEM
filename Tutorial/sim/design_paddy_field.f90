program main
    use SeepageFlowClass
    implicit none

    type(SeepageFlow_) :: solver
    type(FEMDomain_) :: soil
    real(real64),allocatable :: WaterHead(:),Velocity(:,:),Permiability(:),center(:)
    real(real64) :: depth = 0.5000d0
    integer(int32) :: timestep

    call soil%create("Cube3D",x_num=30,y_num=50,z_num=20)
    call soil%resize(x=52.0d0, y=20.0d0, z=2.0d0 )
    call soil%move(z=-soil%zmax() )
    ! or you can read 
    !call soil%read("your_awesome_mesh.vtk")
    Permiability = dble(3.0e-6)/100.0d0*eyes(soil%nn())
    do i_i=1,soil%ne()
        center = soil%centerPosition(i_i)
        if( center(3) > 0.15d0 )then 
            Permiability(i_i) = dble(3.0e-4)/100.0d0
        endif
    enddo
    

    ! initialize solverulator
    ! m/sec
    call solver%init(soil,model="Darcy",Permiability=Permiability)

    ! Boundary condition
    ! pressure :: mH2O
    call solver%fixPressureBoundary(&
            NodeList = soil%select(x_max=soil%x_min(), z_min=-depth) , &
            pressure = -depth &
        )
    call solver%fixPressureBoundary(&
        NodeList = soil%select(x_min=soil%x_max(), z_min=-depth) ,  &
        pressure = -depth &
    )
    call solver%fixPressureBoundary(&
            NodeList = soil%select(y_max=soil%y_min(), z_min=-depth) , &
            pressure = -depth &
        )
    call solver%fixPressureBoundary(&
        NodeList = soil%select(y_min=soil%y_max() , z_min=-depth) ,  &
        pressure = -depth &
    )

    call solver%fixPressureBoundary(&
        NodeList = soil%select(z_max=soil%z_min()) ,  &
        pressure = 0.00d0 &
    )

    print *, "solve"
    
    do i_i=1,soil%nn()
        solver%WaterHead(i_i) = 0.0d0
    enddo

    call soil%vtk("result_p_"+zfill(0,4),scalar=solver%WaterHead)
    do timestep=1,100
        WaterHead = solver%getPressure(debug=.true.,dt=60.0d0*60.0d0, timeIntegral="BackwardEuler")
        call soil%vtk("result_p_"+zfill(timestep,4),scalar=WaterHead)
        solver%WaterHead = WaterHead
    enddo
    Velocity     = solver%getVelocity(Pressure=WaterHead,Permiability=0.0010d0)
    
    ! Compute pore-pressure
    ! visualization
    ! pressure
    call soil%vtk("result_p",scalar=WaterHead)
    ! velocity
    call soil%vtk("result_v",vector=-Velocity)

end program main