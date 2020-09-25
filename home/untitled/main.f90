
program main
    use plantFEM
    use httpclass
    implicit none

    type(FEMDomain_) :: domain1,domain2,domain3
    type(Leaf_) ::leaf1,leaf2
    type(stem_) ::stem1
    type(root_) ::root1,root2
    type(MPI_) ::mpid
    type(http_) :: api

    call leaf1%init()
    call leaf1%rotate(x=radian(90) )

    call leaf2%init()
    call leaf2%rotate(x=radian(-90) )

    call root1%init()
    call root1%rotate(x=radian(10))
    call root2%init()

    call stem1%init()

    call leaf1%connect("=>",stem1)
    call leaf2%connect("=>",stem1)
    call root1%connect("=>",stem1)
    call root2%connect("=>",root1)


    call root1%gmsh("root1")
    call root2%gmsh("root2")
    call leaf1%gmsh("leaf1")
    call leaf2%gmsh("leaf2")
    call stem1%gmsh("stem")
    return



    !call domain%create(meshtype="Sphere3D",x_num=10,y_num=10,z_num=10,x_len=10.0d0, y_len=20.0d0, z_len=30.0d0)
    call domain1%create(meshtype="Cube",x_num=10,y_num=10,z_num=10,x_len=10.0d0*dble(mpid%myrank+1), y_len=20.0d0, z_len=30.0d0)
    call domain1%move(x=10.0d0)
    

    call domain2%create(meshtype="Cube",x_num=10,y_num=10,z_num=10,x_len=10.0d0, y_len=20.0d0, z_len=30.0d0)
    call domain2%rotate(x=radian(10),y=radian(20))
    
    call domain3%create(meshtype="Sphere3D",x_num=10,y_num=10,z_num=10,x_len=10.0d0, y_len=20.0d0, z_len=30.0d0)
    call domain3%move(x=-10.0d0)
    
    call domain1%gmsh(name="test1"//"_"//trim(str(mpid%myrank)))
    call domain2%gmsh(name="test2"//"_"//trim(str(mpid%myrank)))
    call domain3%gmsh(name="test3"//"_"//trim(str(mpid%myrank)))

    return

    api%url = "https://slack.com/api/chat.postMessage"
    api%token = "xoxp-1126761884548-1120792060899-1377501392422-52768e1ace681295d1bc2d40581a7b8e"
    api%channel = "#general"
    api%body = "べろべろばー！！"

    call api%post()


end program main