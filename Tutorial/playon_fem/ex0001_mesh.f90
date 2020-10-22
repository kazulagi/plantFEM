program main
    use fem
    implicit none

    type(Mesh_) :: cube, sphere, cylinder
    real(real64),allocatable :: coordinate(:)

    ! create new plain mesh
    call cube%create(meshtype="Cube",x_num=10,y_num=10,division=10,x_len=1.0d0,y_len=1.0d0,thickness=1.0d0)
    call sphere%create(meshtype="Sphere",x_num=10,y_num=10,division=10,x_len=1.0d0,y_len=1.0d0,thickness=1.0d0)
    call cylinder%create(meshtype="Cylinder",x_num=10,y_num=10,division=10,x_len=1.0d0,y_len=1.0d0,thickness=1.0d0)

    ! export as json files
    call cube%json(name="cube.json",endl=.true.)
    call sphere%json(name="sphere.json",endl=.true.)
    call cylinder%json(name="cylinder.json",endl=.true.)


    ! get number of nodes
    print *, "Number of nodes (cube) : ", cube%numNodes()
    ! get number of elements
    print *, "Number of elements (cube) : ", cube%numElements()

    ! remove elements and nodes in a range
    call cube%remove(x_min=0.20d0,x_max=2.0d0)
    ! get number of nodes
    print *, "Number of nodes (cube) : ", cube%numNodes()
    ! get number of elements
    print *, "Number of elements (cube) : ", cube%numElements()

    ! get coordinate
    ! (x, y, z) of node, the ID of which is 10
    coordinate = cube%getCoordinate(NodeID=11)
    print *, "x, y, z :",coordinate(:)

    ! x-coordinate (x1, x2, x3, ... xn) of all nodes
    coordinate = cube%getCoordinate(onlyY=.true.)
    print *, "(x1, x2, x3, ... xn) :",coordinate(:)

end program main