
program main
    use LoggerClass
    use SeismicAnalysisClass
    implicit none

    type(SeismicAnalysis_)  :: sim 

    type(FEMDomain_),target :: domains(1)
    real(real64),allocatable :: x_axis(:),y_axis(:),z_axis(:)
    type(Random_) :: random
    logical :: connected(1,1) = .false. 

    real(real64) :: Density, &
        Length, Width, Thickness
    real(real64),allocatable ::YoungModulus(:),PoissonRatio(:)
    real(real64) :: Vs(1:3)
    real(real64),allocatable :: v(:,:)
    

    real(real64) :: dt,t

    ! Data logger
    type(Logger_) :: logger(1:20)
    integer(int32),allocatable :: LoggerNodeID(:)

    ! simple earthquake simulation

    ! Info: 
    Vs(1) = 150.0d0  !  150 m/s
    Vs(2) = 300.0d0  !  300 m/s
    Vs(3) = 3000.0d0 ! 3000 m/s
    density = 17.00d0/9.80d0
    Length    = 5000.00d0 ! 1 km
    Width     = 5000.00d0 ! 1 km
    Thickness = 1000.00d0!  300 m

    ! Mesh-generation process
    x_axis = [0.0d0,Length]
    y_axis = [0.0d0,Width]
    call Refine(x_axis,6)
    call Refine(y_axis,6)

    z_axis = [-100.0d0,0.0d0]
    call Refine(z_axis,4)
    z_axis = [-Thickness] // z_axis
    call Refine(z_axis,3)

    call domains(1)%create("Cube3D",x_axis=x_axis,y_axis=y_axis,z_axis=z_axis)

    call domains(1)%vtk("B1")   
    print *, domains(1)%nn()
    YoungModulus = zeros(domains(1)%ne())
    PoissonRatio = 0.300d0*eyes(domains(1)%ne())

    YoungModulus(:) = (Vs(1)*Vs(1)*density)*2.0d0*(1.0d0+0.300d0)
    
    YoungModulus(domains(1)%getElementList(zmax=-20.0d0) ) = (Vs(2)*Vs(2)*density)*2.0d0*(1.0d0+0.300d0)
    
    call domains(1)%rotate(y=radian(30.0d0) )
    YoungModulus(domains(1)%getElementList(zmax=0.0d0) ) = (Vs(3)*Vs(3)*density)*2.0d0*(1.0d0+0.300d0)
    call domains(1)%rotate(y=radian(-30.0d0) )

    call domains(1)%rotate(x=radian(30.0d0) )
    YoungModulus(domains(1)%getElementList(zmax=0.0d0) ) = (Vs(3)*Vs(3)*density)*2.0d0*(1.0d0+0.300d0)
    call domains(1)%rotate(x=radian(-30.0d0) )

    YoungModulus(domains(1)%getElementList(zmax=-100.0d0) ) = (Vs(3)*Vs(3)*density)*2.0d0*(1.0d0+0.300d0)
    
    call domains(1)%vtk("YoungModulus",scalar=YoungModulus)
    
    ! 20 m/s
    ! 
    ! mesh creation done!
    
    !or you can load mesh from vtk file
    !call domains(1) % read("mesh.vtk")

    ! >> Run solver
    print *, "Initialize >> "
    call sim%init(domains)

    print *, "Material >> "
    call sim%setMaterial(DomainID=1,density=[Density],YoungModulus=YoungModulus,PoissonRatio=PoissonRatio)
    
    print *, "Boundary >> "
    call sim%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
        zmax = domains(1)%zmin() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call sim%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
        xmax = domains(1)%xmin() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call sim%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
        ymax = domains(1)%ymin() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call sim%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
        xmin = domains(1)%xmax() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call sim%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
        ymin = domains(1)%ymax() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])

    
    ! non-damping

    
    ! setup logger
    LoggerNodeID = [9673,9641,9609,9577,9513,9481,9417,9385,9257,9097]
    do j_j=1,size(LoggerNodeID)
        call Logger(j_j)%set(&
            channel_name="Ax_"+str(LoggerNodeID(j_j)),&
            channel_value=sim%A(LoggerNodeID(j_j)*3-2),&
            position=domains(1)%position(LoggerNodeID(j_j) ) )
        call Logger(j_j)%set(&
            channel_name="Ay_"+str(LoggerNodeID(j_j)),&
            channel_value=sim%A(LoggerNodeID(j_j)*3-1) )
        call Logger(j_j)%set(&
            channel_name="Az_"+str(LoggerNodeID(j_j)),&
            channel_value=sim%A(LoggerNodeID(j_j)*3-0) )
        call logger(j_j)%vtk("A"+str(LoggerNodeID(j_j)))
    enddo

    do j_j=1,size(LoggerNodeID)
        call logger(j_j)%start()
    enddo

    print *, "Solve >> "

    sim%modal%solver%debug= .true.
    !sim%modal%solver%er0  = dble(1.0e-15)
    !sim%modal%solver%relative_er  = dble(1.0e-10)
    ! 1 kHz sampling
    dt = 1.0d0/100.0d0
    
    sim%alpha = 0.0d0
    sim%beta = 0.0d0
    t = 0.0d0
    do i_i=1,10000
        
        print *, t

        ! m/s
        if(1 <= i_i .and. i_i<=10)then
            call sim%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
                zmin=-900.0d0,zmax=-800.0d0)&
                ,condition="A",boundaryValue=[0.00d0,random%gauss(mu=0.0d0,sigma=1.0d0),0.030d0])
        elseif(11 <= i_i .and. i_i<=20)then
            call sim%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
                zmin=-900.0d0,zmax=-800.0d0)&
                ,condition="A",boundaryValue=[0.00d0,random%gauss(mu=0.0d0,sigma=1.0d0),-0.030d0])
        else
            call sim%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
                zmin=-900.0d0,zmax=-800.0d0)&
                ,condition="A",boundaryValue=[0.0d0,0.0d0,0.00d0])
        endif
        
        t = t + dt
        call sim%solve(dt=dt,timeIntegral="Nemwark-beta",use_same_matrix=.true.)
        
        v = reshape(sim%v,domains(1)%nn(),domains(1)%nd() )
        
        if(mod(i_i,10) ==0)then
            call domains(1)%vtk("x_step_"+zfill(i_i,4), v(:,1) )
            call domains(1)%vtk("y_step_"+zfill(i_i,4), v(:,2) )
            call domains(1)%vtk("z_step_"+zfill(i_i,4), v(:,3) )
        endif

        do j_j=1,10
            call logger(j_j)%save(t=t)
        enddo

    enddo
    ! destroy
    call sim%remove()
    
    ![Sim.]
    ! [Theor]
    ! Vp = 3000.0d0 * 1.871 = 5612.5 m/s ! ok
    ! Vs = 3000.0d0 
    ! Vp/Vs = 1.871

    ! P-wave
    !> 100.0 m / (44 - 26) ms = 5555.6 m/s [ok!]
    ! S-wave
    !  100.0 m / (132 - 89)ms = 3333.3 m/s [ok!]
    !> 50 m/s
    !Vp/Vs = 1.875                         [ok!]

    ! Varification completed.

end program main
