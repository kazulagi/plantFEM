program main
    use TSFEMClass
    implicit none

    type(FEMDomain_) :: domain
    type(FEMDomain_) :: my_domain
    type(TSFEM_)     :: TSFEM
    type(MPI_)       :: mpid
    integer(int32)   :: n
    integer(int32),allocatable :: LocalNodeList(:),idx(:)

    call mpid%init()



    call domain%create("Cube3D",x_num=100,y_num=100,z_num=1)
    call domain%resize(x=50.0d0,y=50.0d0,z=0.10d0)
    my_domain = domain%divide(mpid)
    n = domain%nn()
    call domain%remove()
    
    call my_domain%vtk("Cube3D"+zfill(mpid%myrank,4) )
    
    call TSFEM%init(femdomain=my_domain,density=2.50d0,&
        YoungModulus=dble(1.0e+7),PoissonRatio=0.30d0,DOF=3,mpid=mpid)
    
            
    !call TSFEM%init(femdomain=my_domain,density=2.50d0,&
    !    YoungModulus=dble(1.0e+7),PoissonRatio=0.30d0,DOF=3)
    
    !call TSFEM%DirichletBoundary(NodeList=[1],direction="X")
    !call TSFEM%AbsorbingBoundary(NodeList=[1],spring=100000.0d0,damper=1.0d0,direction="X")
    call TSFEM%NeumannBoundary(NodeList=[n],Force=dble(1.0e+3),direction="Z")
    


    call TSFEM%Time(dt=dble(1.0e-4),t=0.0d0,timestep=0)

    do i_i=1,1200
        if(i_i>50)then
            call TSFEM%NeumannBoundary(NodeList=[n],Force=0.0d0,direction="Z")
        endif
        
        print *, "step :: ",i_i
        call TSFEM%update()

        if(mod(i_i,10)==0 )then
            call TSFEM%save(name="result_sp"+zfill(mpid%myrank,4))
        endif    
    enddo
    
    !call TSFEM%movie(name="result"+zfill(mpid%myrank,4) )
    call mpid%finalize()

end program
