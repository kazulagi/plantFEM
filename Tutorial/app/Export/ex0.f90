program main
    use fem
    implicit none

    type(FEMDomain_) :: water

    call water%create(Name="water",MeshType="rectangular3D",x_num=30,y_num=5,x_len=300.0d0, y_len=50.0d0,&
        thickness=50.0d0,division=5)
    call water%display("./","example",".vtk")
    call water%display("./","example",".ply")

end program 