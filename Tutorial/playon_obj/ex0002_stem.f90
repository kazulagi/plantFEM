program main
    use StemClass
    implicit none

    type(Stem_) :: stem1, stem2, stem3,stem4,stem5,stem6
    integer(int32) :: i

    !　まずはcontinuum-structural stemを作る。
    call stem1%init()
    call stem2%init(config="stemconfig.json")
    call stem3%init(config="stemconfig.json")
    call stem4%init(config="stemconfig.json")
    call stem5%init(config="stemconfig.json")
    call stem6%init(config="stemconfig.json")


    print *, stem1%getcoordinate("A")
    print *, stem1%getcoordinate("B")
    call stem1%gmsh("Initial")
    call stem1%rotate(x=radian(30.0d0),y=radian(45.0d0),z=radian(170.0d0) )
    print *, stem1%getcoordinate("A")
    print *, stem1%getcoordinate("B")
    call stem1%gmsh("Rotated")
    
    ! connect stem to stem
    
    call stem2%rotate(x=radian(10.0d0))
    call stem3%rotate(y=radian(10.0d0))
    call stem4%rotate(z=radian(10.0d0))
    call stem5%rotate(x=radian(20.0d0))
    call stem6%rotate(y=radian(40.0d0))
    

    call stem1%connect("<=",stem2)
    call stem2%connect("<=",stem3)
    call stem3%connect("<=",stem4)
    call stem2%connect("<=",stem5)
    call stem4%connect("<=",stem6)
    
    call stem1%gmsh("Stem1")
    call stem2%gmsh("Stem2")
    call stem3%gmsh("Stem3")
    call stem4%gmsh("Stem4")
    call stem5%gmsh("Stem5")
    call stem6%gmsh("Stem6")
 
end program main