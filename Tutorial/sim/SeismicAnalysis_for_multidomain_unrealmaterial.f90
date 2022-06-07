!module WaveClass
!    use RandomClass
!
!    type :: Wave_
!    contains
!        procedure, public :: create => createWave
!    end type
!contains
!
!function createWave(this, Sampling_Hz, )
!end module

program main
    !use WaveClass
    use COOClass
    use SeismicAnalysisClass
    implicit none

    type(SeismicAnalysis_)  :: sim 

    type(FEMDomain_),target :: domains(1)
    !type(Wave_) :: wave
    logical :: connected(1,1) = .false. 

    real(real64) :: YoungModulus,    PoissonRatio,    Density, &
        Length, Width, Thickness

    real(real64),allocatable :: v(:,:)
    
    !real(real64) :: sample_wave(:,:)
    real(real64) :: dt,t,G
    type(Time_) :: time

    type(COO_) :: coo(1:2)
    type(CRS_) :: crs(1:3)
    ! Modal analysis
    ! Info: 
    YoungModulus = 101.000d0*(10.0d0)**3  
    PoissonRatio = 0.350d0
    density = 87.30d0/9.80d0
    Length    = 10.00d0 ! 10 m
    Width     = 10.0d0 !  1 m
    Thickness = 10.00d0!  1 cm

    ! Mesh-generation process
    call domains(1)%create("Cube3D",x_num=70,y_num=70,z_num=70)

    call domains(1)%resize(x=Length,y=Width,z=Thickness)

    call domains(1)%move(x= 0.00d0)

    call domains(1)%vtk("B1")

    G = YoungModulus/2.0d0/(1+PoissonRatio)
    print *, "G=" , G ,"kN/m^2"
    print *, "Vs=" , sqrt(G/Density),"m/s"
    
    call time%sleep(5)

    ! 20 m/s
    ! 
    ! mesh creation done!
    
    !or you can load mesh from vtk file
    !call domains(1) % read("mesh.vtk")

    ! >> Run solver
    print *, "Initialize >> "
    call sim%init(domains)

    print *, "Material >> "
    call sim%setMaterial(DomainID=1,density=[Density],YoungModulus=[YoungModulus],PoissonRatio=[PoissonRatio])
    
    print *, "Boundary >> "
    call sim%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
        xmax = domains(1)%xmin() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1000.00d0])

    print *, "Solve >> "

    sim%modal%solver%debug= .true.
    sim%modal%solver%er0  = dble(1.0e-15)
    sim%modal%solver%relative_er  = dble(1.0e-3)
    ! 1 kHz sampling
    dt = 1.0d0/1000.0d0
    
    ! non-damping

    sim%alpha = 0.0d0
    sim%beta = 0.0d0
    
    t = 0.0d0
    do i_i=1,1000
        
        print *, t

        ! m/s
        if(1 <= i_i .and. i_i<=6)then
            call sim%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
                xmax = domains(1)%xmin() ),condition="A",boundaryValue=[0.0d0,0.0d0,0.10d0])
        elseif(7 <= i_i .and. i_i<=12)then
            call sim%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
                xmax = domains(1)%xmin() ),condition="A",boundaryValue=[0.0d0,0.0d0,-0.10d0])
        else
            call sim%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
                xmax = domains(1)%xmin() ),condition="A",boundaryValue=[0.0d0,0.0d0,0.0d0])
        endif
        
        t = t + dt
        call sim%solve(dt=dt,timeIntegral="Nemwark-beta",use_same_matrix=.true.)
        
        if(mod(i_i,10)==0 )then
            v = reshape(sim%v,domains(1)%nn(),domains(1)%nd() )
            call domains(1)%vtk("x_step_"+zfill(i_i/10,4), v(:,1) )
            call domains(1)%vtk("y_step_"+zfill(i_i/10,4), v(:,2) )
            call domains(1)%vtk("z_step_"+zfill(i_i/10,4), v(:,3) )
        endif
        
    enddo
    ! destroy
    call sim%remove()
    
    ![Sim.]
    ! P-wave
    !> 4.6 m/ 35 ms = 131.42 m/s
    ! S-wave
    ! 2.3 m/  35 ms = 65.7 m/s 
    !> 50 m/s
    !Vp/Vs = 4.6/2.3 = 2.0


    ! [Theor]
    !Vp/Vs = sqrt(2.0*(1-0.35)/(1-0.7))
    ! = 2.08

    ! Vp = (101000/(87.30d0/9.80d0)*(1-0.35)/(1.35)/(0.3) )**0.5
    ! = 134.89 m/s
    ! Vs = (101000/(87.300/9.800)/2/1.35)^0.5
    ! =  64.80  m/s

end program main
