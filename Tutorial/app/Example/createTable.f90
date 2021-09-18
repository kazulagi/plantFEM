program main
    use fem
    implicit none

    type(FEMDomain_),target :: Plane1, leg1, leg2, leg3, leg4
    type(FEMDomain_),target :: sphere

    type(FEMDomain_):: Table(5)
    

    call sphere%create(Name="sphere",MeshType="Sphere3D",x_num=10,y_num=10,x_len=90.0d0, y_len=80.0d0,&
    thickness=70.0d0,division=10)

    ! 天板
    call Plane1%create(meshtype="rectangular3D", x_num=30,y_num=15,x_len=100.0d0, &
    y_len=50.0d0,thickness=3.0d0,division=5)
    call Plane1%move(x=-50.0d0, y=-25.0d0, z=-1.50d0)

    ! 机の脚
    call leg1%create(meshtype="rectangular3D", x_num=10,y_num=10,x_len=3.0d0, &
    y_len=-3.0d0,thickness=40.0d0,division=30)
    call leg2%copy(leg1)
    call leg3%copy(leg1)
    call leg4%copy(leg1)
    call leg1%move(x=-50.0d0,y=-20.0d0)
    call leg2%move(x= 50.0d0,y=-20.0d0)
    call leg3%move(x= 50.0d0,y= 20.0d0)
    call leg4%move(x=-50.0d0,y= 20.0d0)
    call leg1%rotate(y=-0.30d0)
    call leg2%rotate(y= 0.30d0)
    call leg3%rotate(y= 0.30d0)
    call leg4%rotate(y=-0.30d0)
    
    ! 出力
    call Plane1%gmsh(Name="Plane1",tag="Plane1")
    call leg1%gmsh(Name="leg1"    ,tag="leg1")
    call leg2%gmsh(Name="leg2"    ,tag="leg2")
    call leg3%gmsh(Name="leg3"    ,tag="leg3")
    call leg4%gmsh(Name="leg4"    ,tag="leg4")
    call sphere%gmsh(Name="sphere"    ,tag="sphere")

    call Plane1%export(Name=   " Plane1" ,  OptionalFileFormat=".stl")
    call   leg1%export(Name= "   leg1" ,  OptionalFileFormat=".stl")
    call   leg2%export(Name= "   leg2" ,  OptionalFileFormat=".stl")
    call   leg3%export(Name= "   leg3" ,  OptionalFileFormat=".stl")
    call   leg4%export(Name= "   leg4" ,  OptionalFileFormat=".stl")
    call sphere%export(Name="sphere"    ,OptionalFileFormat=".stl")



    Table(1) = leg1
    Table(2) = leg2
    Table(3) = leg3
    Table(4) = leg4
    Table(5) = Plane1

end program