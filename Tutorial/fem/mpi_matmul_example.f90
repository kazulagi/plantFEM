program main
    use FEMDomainClass
    implicit none

    type(FEMDomain_) :: domain
    type(FEMDomain_) :: my_domain
    type(CRS_) :: Mmatrix
    real(real64),allocatable :: Kv(:),v(:)
    type(MPI_) :: mpid

    call mpid%init()
    
    ! Domain Decomposition for FEM
    call domain%create("Cube3D",x_num=2,y_num=2,z_num=1)
    call domain%vtk("Cube3D")

    my_domain = domain%divide(mpid)
    call domain%remove()
    
    Mmatrix = my_domain%MassMatrix(&
        Density=1.0*ones(my_domain%ne()) ,&
        DOF=3 &
        )

    v = ones(Mmatrix%size() )
    Kv = my_domain%mpi_matmul(Mmatrix,v,mpid)
    
    
        
    if(mpid%petot==1)then
        call print(dble([(i_i,i_i=1,size(Kv))]) .h. Kv(:) )
    else
        do i_i=1,size(my_domain%mpi_global_node_idx)
            print *, (my_domain%mpi_global_node_idx(i_i)-1)*3+1, Kv( (i_i-1)*3+1 )
            print *, (my_domain%mpi_global_node_idx(i_i)-1)*3+2, Kv( (i_i-1)*3+2 )
            print *, (my_domain%mpi_global_node_idx(i_i)-1)*3+3, Kv( (i_i-1)*3+3 )
        enddo
    endif

    call mpid%finalize()

end program main