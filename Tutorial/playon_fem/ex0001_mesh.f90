program main
    use fem
    implicit none

    type(FEMDomain_) :: obj1
    type(FEMDomain_) :: obj2
    type(FEMDomain_) :: obj3
    type(FEMDomain_) :: obj4
    type(FEMDomain_) :: obj5


    integer(int32) :: i

    ! create mesh entities

    ! -----> 1D
    call obj1%create(meshtype="Bar1D",x_num=10,x_len=10.0d0)
    
    ! -----> 2D
    call obj2%create(meshtype="rectangular2D",x_num=12,y_num=12,x_len=5.0d0,y_len=50.0d0)
    call obj2%gmsh(Name="obj2",timestep=0)
    
    ! -----> 3D
    call obj3%create(meshtype="Cube",x_num=10,y_num=12,z_num=10,x_len=5.0d0,y_len=50.0d0,z_len=10.0d0)
    call obj3%gmsh(Name="obj3",timestep=0)
    
    call obj4%create(meshtype="Sphere3D",x_num=10,y_num=10,z_num=10,x_len=5.0d0,y_len=50.0d0,z_len=10.0d0)
    call obj4%gmsh(Name="obj4",timestep=0)

    ! -----> move
    do i=1, 20
        call obj4%move(x=1.0d0)
        call obj4%gmsh(Name="obj4_move",timestep=i)
    enddo

    ! -----> move
    do i=1, 20
        call obj4%rotate(x=0.10d0)
        call obj4%gmsh(Name="obj4_rotate",timestep=i)
    enddo
end program main