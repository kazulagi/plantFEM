program main
    use TSFEMClass
    implicit none

    type(FEMDomain_) :: domain
    type(TSFEM_)     :: TSFEM

    !call domain%create("Line1D",x_num=500)
    !call domain%resize(x=50.0d0)
    !call domain%vtk("Line1D")

    call domain%create("Cube3D",x_num=300,y_num=300,z_num=1)
    call domain%resize(x=50.0d0,y=50.0d0,z=0.10d0)
    call domain%vtk("Cube3D")

    call TSFEM%init(femdomain=domain,density=2.50d0,&
        YoungModulus=dble(1.0e+7),PoissonRatio=0.30d0,DOF=3)
    
    !call TSFEM%DirichletBoundary(NodeList=[1],direction="X")
    !call TSFEM%AbsorbingBoundary(NodeList=[1],spring=100000.0d0,damper=1.0d0,direction="X")
    call TSFEM%NeumannBoundary(NodeList=[domain%nn()],Force=dble(1.0e+3),direction="X")
    
    call TSFEM%Time(dt=dble(1.0e-4),t=0.0d0,timestep=0)

    do i_i=1,1200

        if(i_i>50)then
            call TSFEM%NeumannBoundary(NodeList=[domain%nn()],Force=0.0d0,direction="X")
        endif
        
        call TSFEM%update()
        if(mod(i_i,10)==0 )then
            call TSFEM%save(name="result")
        endif
        
    enddo

    call TSFEM%movie(name="result")
end program