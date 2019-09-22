program main
    use GeometryClass

    type(Triangle_) ::  tri1
    type(Point_)    ::  point1,point2,point3
    type(Circle_)   ::  cir1

    call point1%Init(dim=2)
    call point2%Init(dim=2)
    call point3%Init(dim=2)
    
    call point1%set(x=0.0d0,y=0.0d0)
    call point2%set(x=1.0d0,y=8.0d0)
    call point3%set(x=5.0d0,y=2.0d0)

    call tri1%init(dim=2)
    call tri1%setNode(point=point1,order=1)
    call tri1%setNode(point=point2,order=2)
    call tri1%setNode(point=point3,order=3)

    
    call tri1%show("Triangle.txt")
    cir1=tri1%getCircle(type_of_circle="center_of_gravity")
    call cir1%show("center_of_gravity.txt")
    cir1=tri1%getCircle(type_of_circle="circumcenter")
    call cir1%show("circumcenter.txt")
    cir1=tri1%getCircle(type_of_circle="innter_center")
    call cir1%show("innter_center.txt")




end program