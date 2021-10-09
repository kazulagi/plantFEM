program main
    use plantfem
    implicit none
    type(FEMDomain_) :: sphere

    call sphere%create(Name="sphere",MeshType="Sphere3D",x_num=10,y_num=10,x_len=90.0d0, y_len=80.0d0,&
    thickness=70.0d0,division=10)
    call sphere%gmsh()
    
end program main