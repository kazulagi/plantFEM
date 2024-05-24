program main
    use SoybeanClass
    implicit none
    
    type(Soybean_) :: Soybean
    real(real64),allocatable :: vertices(:), vertexData(:)
    integer(int32),allocatable :: vertexIDs(:)
    type(Time_) :: time

    call Soybean%create(config="Tutorial/obj/soy.json")
    print *, soybean%nn_range("stem",1),soybean%stem(1)%nn()
    
    ! you can get vertices (surface points)
    call time%start()
    call Soybean%getVertices(vertices,vertexIDs) !***NEW***!
    call time%show()

    ! volume value is 1.0
    vertexData = ones(Soybean%nn())
    ! surface value is 0.0
    vertexData(vertexIDs(:)) = 0.0d0

    call Soybean%vtk("Soybean",scalar_field=vertexData,single_file=true)
    
end program main