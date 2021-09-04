program main
    use plantFEM
    implicit none

    type(FEMDomain_) :: cylinder, cube, sphere

    call cylinder%create(meshtype="Cylinder3D",x_num=5,y_num=5,z_num=10,x_len=10.0d0,y_len=10.0d0,z_len=10.0d0)
    call cylinder%gmsh(name="cylinder")

    call cube%create(meshtype="Cube",x_num=5,y_num=5,z_num=10,x_len=10.0d0,y_len=10.0d0,z_len=10.0d0)
    call cube%gmsh(name="cube")

    call sphere%create(meshtype="Sphere3D",x_num=5,y_num=5,z_num=10,x_len=10.0d0,y_len=10.0d0,z_len=10.0d0)
    call sphere%gmsh(name="sphere")

end program main