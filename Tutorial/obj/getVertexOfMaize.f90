program main
    use MaizeClass
    implicit none
    
    type(Maize_) :: maize
    real(real64),allocatable :: vertices(:), vertexData(:)
    integer(int32),allocatable :: vertexIDs(:)
    
    call maize%create(config="Tutorial/obj/maize.json")
    
    ! you can get vertices (surface points)
    call maize%getVertices(vertices,vertexIDs) !***NEW***!
    
    ! volume value is 1.0
    vertexData = ones(maize%nn())
    ! surface value is 0.0
    vertexData(vertexIDs(:)) = 0.0d0

    call maize%vtk("maize",scalar_field=vertexData,single_file=true)
    
end program main