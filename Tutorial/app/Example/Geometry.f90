program main
    use plantfem
    implicit none

    type(Triangle_) :: tri
    integer(int32) :: i
    call tri%import(FileName="rect.txt")
    call tri%GetOuterNormal()
    call tri%show()
    print *, tri%center(:), tri%OuterNormal(:)
    print *, dot_product(tri%NodCoord(1,:)-tri%center(:), tri%OuterNormal(:))
    print *, dot_product(tri%NodCoord(2,:)-tri%center(:), tri%OuterNormal(:))
    print *, dot_product(tri%NodCoord(3,:)-tri%center(:), tri%OuterNormal(:))
    
    open(10,file="test.txt")
    do i=1,size(tri%NodCoord,1)
        write(10,*) tri%NodCoord(i,:)
    enddo
    write(10,*) tri%NodCoord(i,:)
    
    close(10)
    open(20,file="vec.txt")
    write(20,*) tri%center(:),tri%OuterNormal(:)
    close(20)

end program