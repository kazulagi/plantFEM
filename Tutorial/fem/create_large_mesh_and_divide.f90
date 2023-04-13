program main
    use FEMDomainClass
    implicit none

    type(FEMDomain_) :: domain
    type(FEMDomain_) :: my_domain

    type,abstract :: mesh_t
    end type

    type,extends(mesh_t) :: cube
        integer(int32) :: length
        integer(int32) :: width
        integer(int32) :: height
    end type


    ! Domain decomposition
    call domain%create("Cube3D",x_num=1000,y_num=1000,z_num=100)
    call domain%resize(x=10000.0d0,y=10000.0d0,z=1000.0d0)
    call domain%vtk("Cube3D")
    call domain%vtk("Cube3D",num_division=10) 
    call domain%remove()

    ! Read decomposed element
    call my_domain%read("Cube3D_"+zfill(0,6)+".vtk")
    call my_domain%read_mpi_property("Cube3D_"+zfill(0,6)+".csv")
contains


    
    
end program