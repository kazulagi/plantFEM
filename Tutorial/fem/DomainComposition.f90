program main
    use FEMDomainClass
    implicit none

    type(FEMDomain_) :: domain
    type(FEMDomain_),allocatable :: domains(:)

    ! Domain Decomposition for FEM
    call domain%create("Cube3D",x_num=100,y_num=100,z_num=100)
    call domain%vtk("Cube3D")

    domains = domain%divide(n=10)

    do i_i=1,10
        call domains(i_i)%vtk("domain_"+zfill(i_i,4) )
    enddo

end program main