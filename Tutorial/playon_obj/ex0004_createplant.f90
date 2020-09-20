
program main
    use plantFEM
    implicit none

    type(Stem_) :: stem1, stem2, stem3, stem4, stem5, stem6
    type(Stem_) :: stem7, stem8, stem9
    type(Root_) :: root1, root2, root3, root4, root5, root6
    type(Leaf_) :: leaf1, leaf2, leaf3, leaf4, leaf5, leaf6


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! CAUTION:
    ! This does not work as it is.
    ! please create "stemconfig2.json"
    ! please create "rootconfig2.json"
    ! ~~~~~~~~~~~~~~~~~~~~~
    ! You can get template by
    ! call stem1%init()
    ! and 
    ! call root1%init()
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! init
    call stem1%init()
    
    call stem2%init("stemconfig2.json")
    call stem2%rotate(x=radian(30),y=radian(30) )

    call stem3%init("stemconfig2.json")

    call stem4%init("stemconfig2.json")
    call stem4%rotate(x=radian(10),y=radian(10) )

    call stem5%init("stemconfig2.json")
    call stem5%rotate(x=radian(-10),y=radian(-20),z=radian(45) )


    call stem6%init("stemconfig2.json")
    call stem6%rotate(x=radian(-10),y=radian(-20),z=radian(45) )

    call stem7%init("stemconfig2.json")
    call stem7%rotate(x=radian(-10),y=radian(-20),z=radian(45) )
    
    call stem8%init("stemconfig2.json")
    call stem8%rotate(x=radian(-10),y=radian(-20),z=radian(45) )

    call stem9%init()
    call stem9%rotate(x=radian(-10),y=radian(-20),z=radian(45) )



    call leaf1%init()
    call leaf1%rotate(x=radian(70),y=radian(70),z=radian(80))
    
    call leaf2%init()
    call leaf2%rotate(x=radian(70),y=radian(70),z=radian(-80) )
    
    call leaf3%init()
    call leaf3%rotate(x=radian(30),y=radian(20),z=radian(0) )


    call leaf4%init()
    call leaf4%rotate(x=radian(70),y=radian(70),z=radian(80))
    
    call leaf5%init()
    call leaf5%rotate(x=radian(70),y=radian(70),z=radian(-80) )
    
    call leaf6%init()
    call leaf6%rotate(x=radian(30),y=radian(20),z=radian(0) )



    call root1%init()
    call root1%rotate(x=radian(30),y=radian(20),z=radian(20) )
    
    call root2%init("rootconfig2.json")
    call root2%rotate(x=radian(30),y=radian(20),z=radian(-30) )

    call root3%init()
    call root3%rotate(x=radian(30),y=radian(20),z=radian(20) )
    
    call root4%init("rootconfig2.json")
    call root4%rotate(x=radian(30),y=radian(20),z=radian(-30) )

    call root5%init()
    call root5%rotate(x=radian(30),y=radian(20),z=radian(20) )
    
    call root6%init("rootconfig2.json")
    call root6%rotate(x=radian(30),y=radian(20),z=radian(-30) )

    ! connect

    call stem1%connect("<=",stem2)
    call stem1%connect("<=",stem3)
    call stem2%connect("<=",stem4)
    call stem3%connect("<=",stem5)
    call stem5%connect("<=",stem6)
    call stem7%connect("<=",stem7)
    call stem5%connect("<=",stem8)
    call stem8%connect("<=",stem9)

    call leaf1%connect("=>",stem2)
    call leaf2%connect("=>",stem2)
    call leaf3%connect("=>",stem4)

    call leaf4%connect("=>",stem8)
    call leaf5%connect("=>",stem8)
    call leaf6%connect("=>",stem9)

    call root1%connect("=>",stem1)
    call root2%connect("=>",root1)
    call root3%connect("=>",root2)
    call root4%connect("=>",root3)
    call root5%connect("=>",root3)
    call root6%connect("=>",root4)

    ! output

    call stem1%gmsh("stem1")
    call stem2%gmsh("stem2")
    call stem3%gmsh("stem3")
    call stem4%gmsh("stem4")
    call stem5%gmsh("stem5")
    call stem6%gmsh("stem6")
    call stem7%gmsh("stem7")
    call stem8%gmsh("stem8")
    call stem9%gmsh("stem9")
    
    call root1%gmsh("root1")
    call root2%gmsh("root2")
    call root3%gmsh("root3")
    call root4%gmsh("root4")
    call root5%gmsh("root5")
    call root6%gmsh("root6")
    
    call leaf1%gmsh("leaf1")
    call leaf2%gmsh("leaf2")
    call leaf3%gmsh("leaf3")
    call leaf4%gmsh("leaf4")
    call leaf5%gmsh("leaf5")
    call leaf6%gmsh("leaf6")
    
end program main