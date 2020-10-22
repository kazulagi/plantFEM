program main
    use fem
    implicit none

    type(FEMDomain_) :: obj1
    type(FEMDomain_) :: obj2
    type(FEMDomain_) :: obj3
    type(FEMDomain_) :: obj4
    type(FEMDomain_) :: obj5

    ! create FEM domain entities

    ! -----> 1D
    call obj1%create(meshtype="Bar1D",x_num=10,x_len=10.0d0)

    ! -----> 2D
    call obj2%create(meshtype="rectangular2D",x_num=12,y_num=12,x_len=5.0d0,y_len=50.0d0)
    call obj2%gmsh(Name="obj2")
    
    ! -----> 3D
    call obj3%create(meshtype="Cube",x_num=10,y_num=12,z_num=10,x_len=5.0d0,y_len=50.0d0,z_len=10.0d0)
    
    ! export .pos for Gmsh
    call obj3%gmsh(Name="obj3")
    
    call obj4%create(meshtype="Sphere",x_num=10,y_num=10,z_num=10,x_len=5.0d0,y_len=50.0d0,z_len=10.0d0)
    call obj4%resize(x=1.0d0,y=1.0d0,z=1.0d0)    
    ! export .pos for Gmsh
    call obj4%gmsh(Name="obj4")
    
    ! move 1.0 to x direction
    call obj4%move(x=1.0d0)
    
    ! rotate 30.0 degrees around x-axis
    call obj4%rotate(y=radian(30.0d0) )

    ! copy obj4 to obj5
    call obj5%copy(obj4)
    call obj5%resize(x=10.0d0,y=10.0d0,z=10.0d0)
    call obj5%gmsh(Name="obj5")

    ! remove obj5
    call obj5%remove()

    ! Create cylinder mesh
    call obj5%create(meshtype="Cylinder",x_num=10,y_num=10,z_num=10,x_len=5.0d0,y_len=50.0d0,z_len=10.0d0)
    call obj5%resize(x=10.0d0,y=10.0d0,z=10.0d0)
    call obj5%gmsh(Name="obj5_1")

    ! export as JSON file
    call obj5%json("obj5.json",endl=.true.)

end program main