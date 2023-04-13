program main
    use FEMDomainClass
    implicit none

    type(FEMDomain_) :: domain,my_domain
    type(FEMDomain_),allocatable :: all_domains(:)
    real(real64),allocatable :: x_axis(:),y_axis(:),z_axis(:)
    type(MPI_) :: mpid

    call mpid%init()
    ! Domain decomposition
    x_axis = linspace([0.0d0*1000.0d0,140*1000.0d0],10) & ! EW:300 km
        // linspace([149.0d0*1000.0d0,151.0d0*1000.0d0],200) & ! EW:300 km
        // linspace([160.0d0*1000.0d0,300*1000.0d0],10)  ! EW:300 km

    y_axis = linspace([0.0d0*1000.0d0,140*1000.0d0],10) & ! EW:300 km
        // linspace([149.0d0*1000.0d0,151.0d0*1000.0d0],200) & ! EW:300 km
        // linspace([160.0d0*1000.0d0,300*1000.0d0],10)  ! EW:300 km
        
    z_axis = linspace([0.0d0,-300.0d0],90) & ! UD:0 ~ -300 m , 
        // linspace([-400.0d0,-1000.0d0],30)   & ! UD: ~ -1000m,
        // linspace([-2000.0d0,-50000.0d0],50) ! UD: ~ -50,000m,
    call domain%create("Cube3D",x_axis=x_axis,y_axis=y_axis,z_axis=z_axis)
    call domain%vtk("Cube3D",num_division=10,remove=.true.) 
    
    
    
    ! Read decomposed element
    call my_domain%read_vtk("Cube3D",myrank=0)
    call print(my_domain%mpi_shared_node_info)
    call mpid%finalize()

contains


    
    
end program