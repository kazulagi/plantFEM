program main
    use ModalAnalysisClass
    implicit none

    type(ModalAnalysis_)    :: sim

    type(FEMDomain_),target :: domains(1)
    logical :: connected(1,1) = .false. 

    real(real64) :: YoungModulus,    PoissonRatio,    Density, &
        Length, Width, Thickness
    
    ! Modal analysis
    ! Info: 
    YoungModulus = 1.000d0*(10.0d0)**7  
    PoissonRatio = 0.40d0
    density = 17.0d0/9.80d0
    Length    = 0.70d0 ! 70 cm
    Width     = 0.05d0 !  5 cm
    Thickness = 0.0050d0!0.5 cm

    ! Mesh-generation process
    call domains(1)%create("Cube3D",x_num=150,y_num=5,z_num=2)

    call domains(1)%resize(x=Length,y=Width,z=Thickness)

    call domains(1)%move(x= 0.00d0)

    call domains(1)%vtk("B1")
    
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
        xmax = domains(1)%xmin() ))

    print *, "Solve >> "
    call sim%solve(penalty=YoungModulus*100.0d0)

    print *, "Export >> "
    call sim%vtk(name="test",num_mode=10,amp=0.0010d0,stress_scale=1000.0d0)
    
    ! destroy
    call sim%remove()

    ! analytical solution
    !(1.875/L)^2*√(ρA/EI)
    print *, "Analytical solution: (1st mode) ",((1.8750d0/Length)**2)&
        *sqrt(YoungModulus*&
        (Width*Thickness**3/12.0d0)/(density*Width*Thickness) )/2.0d0/3.141590d0," Hz"
    
    
end program main
