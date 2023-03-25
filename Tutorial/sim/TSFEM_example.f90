program main
    use TSFEMClass
    implicit none

    type(FEMDomain_) :: domain
    type(TSFEM_)     :: TSFEM

    call domain%create("Line1D",x_num=500)
    call domain%resize(x=50.0d0)
    call domain%vtk("Line1D")

    call TSFEM%init(femdomain=domain,density=2.50d0,YoungModulus=dble(1.0e+7),DOF=1)
    
    !call TSFEM%DirichletBoundary(NodeList=[1],direction="X")
    !call TSFEM%AbsorbingBoundary(NodeList=[1],spring=100000.0d0,damper=1.0d0,direction="X")
    call TSFEM%NeumannBoundary(NodeList=[domain%nn()],Force=dble(1.0e+3),direction="X")
    
    call TSFEM%Time(dt=dble(1.0e-4),t=0.0d0,timestep=0)

    do i_i=1,1200

        if(i_i>50)then
            call TSFEM%NeumannBoundary(NodeList=[domain%nn()],Force=0.0d0,direction="X")
        endif
        call TSFEM%update()
        call TSFEM%save(name="result")
    enddo

    call TSFEM%movie(name="result")
end program