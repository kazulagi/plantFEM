
program main
    use SeismicAnalysisClass
    implicit none

    type(SeismicAnalysis_)  :: sim 

    type(FEMDomain_),target :: domains(1)
    real(real64),allocatable :: x_axis(:),y_axis(:),z_axis(:)
    logical :: connected(1,1) = .false. 

    real(real64) :: Density, &
        Length, Width, Thickness
    real(real64),allocatable ::YoungModulus(:),PoissonRatio(:)
    real(real64) :: Vs(1:3)
    real(real64),allocatable :: v(:,:)
    
    real(real64) :: dt,t

    ! simple earthquake simulation

    ! Info: 
    Vs(1) = 150.0d0  !  150 m/s
    Vs(2) = 300.0d0  !  300 m/s
    Vs(3) = 3000.0d0 ! 3000 m/s
    density = 17.00d0/9.80d0
    Length    = 1000.00d0 ! 1 km
    Width     = 1000.00d0 ! 1 km
    Thickness = 300.00d0!  300 m

    ! Mesh-generation process
    x_axis = [0.0d0,Length]
    y_axis = [0.0d0,Width]
    call Refine(x_axis,6)
    call Refine(y_axis,6)

    z_axis = [-100.0d0,0.0d0]
    call Refine(z_axis,3)
    z_axis = [-Thickness] // z_axis
    call Refine(z_axis,5)

    call domains(1)%create("Cube3D",x_axis=x_axis,y_axis=y_axis,z_axis=z_axis)

    call domains(1)%vtk("B1")   
    print *, domains(1)%nn()
    YoungModulus = zeros(domains(1)%ne())
    PoissonRatio = 0.300d0*eyes(domains(1)%ne())

    YoungModulus(:) = (Vs(1)*Vs(1)*density)*2.0d0*(1.0d0+0.300d0)
    
    YoungModulus(domains(1)%getElementList(zmax=-20.0d0) ) = (Vs(2)*Vs(2)*density)*2.0d0*(1.0d0+0.300d0)
    
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

    print *, "Solve >> "

    sim%modal%solver%debug= .true.
    !sim%modal%solver%er0  = dble(1.0e-15)
    !sim%modal%solver%relative_er  = dble(1.0e-10)
    ! 1 kHz sampling
    dt = 1.0d0/1000.0d0
    
    ! non-damping

    sim%alpha = 0.0d0
    sim%beta = 0.0d0
    
    t = 0.0d0
    do i_i=1,10000
        
        print *, t

        ! m/s
        if(1 <= i_i .and. i_i<=10)then
            call sim%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
                zmin=-280.0d0,zmax=-240.0d0)&
                ,condition="A",boundaryValue=[0.00d0,0.10d0,0.030d0])
        elseif(11 <= i_i .and. i_i<=20)then
            call sim%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
                zmin=-280.0d0,zmax=-240.0d0)&
                ,condition="A",boundaryValue=[0.00d0,-0.10d0,-0.030d0])
        else
            call sim%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
                zmin=-280.0d0,zmax=-240.0d0)&
                ,condition="A",boundaryValue=[0.0d0,0.0d0,0.00d0])
        endif
        
        t = t + dt
        call sim%solve(dt=dt,timeIntegral="Nemwark-beta",use_same_matrix=.true.)
        
        v = reshape(sim%v,domains(1)%nn(),domains(1)%nd() )
        
        call domains(1)%vtk("x_step_"+zfill(i_i,4), v(:,1) )
        call domains(1)%vtk("y_step_"+zfill(i_i,4), v(:,2) )
        call domains(1)%vtk("z_step_"+zfill(i_i,4), v(:,3) )
        
    enddo
    ! destroy
    call sim%remove()
    
    ![Sim.]
    ! P-wave
    !> 100.0 m / (44 - 26) ms = 5555.6 m/s
    ! S-wave
    !  100.0 m / (156 - 90)ms = 1515.2 m/s 
    !> 50 m/s
    !Vp/Vs = 3.6665 


    ! [Theor]
    ! Vp = 3000.0d0 * 1.871 = 5612.5 m/s ! ok
    ! Vs = 3000.0d0 ?
    ! Vp/Vs = 1.871

    ! ベンチマーク結果:
    ! Vpは一致，Vsは計算の方が2倍遅い．
    ! >> デバッグ
end program main
