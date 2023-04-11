program main
    use FEMDomainClass
    implicit none

    type(FEMDomain_) :: domain
    type(FEMDomain_) :: my_domain
    type(CRS_) :: Kmatrix
    real(real64),allocatable :: Kv(:),v(:)
    type(MPI_) :: mpid

    call mpid%init()
    
    ! Domain Decomposition for FEM
    call domain%create("Cube3D",x_num=2,y_num=2,z_num=1)
    call domain%vtk("Cube3D")

    my_domain = domain%divide(mpid)
    call domain%remove()
    do i_i=1,mpid%petot
        if(mpid%myrank==i_i-1)then
            call print(mpid%myrank)
            call print(my_domain%mpi_shared_node_info)
        else

        endif
        call mpid%barrier()
    enddo
    call my_domain%vtk("domain_"+zfill(mpid%myrank,4) )
    
    call mpid%finalize()

end program main