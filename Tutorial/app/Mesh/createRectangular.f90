program main
    use MeshClass
    implicit none

    type(Mesh_) :: root
    integer(int32) :: i,n,m
    call root%create(meshtype="rectangular2D",x_num=120,y_num=120,x_len=5.0d0,y_len=50.0d0)
    call showArray(root%ElemNod)
    call showArraySize(root%NodCoord)
    call showArraySize(root%ElemNod)
    
    open(10,file="test.d")
    do i=1, size(root%ElemNod,1)
        write(10,*) root%NodCoord(root%ElemNod(i,1),1:2)
        write(10,*) root%NodCoord(root%ElemNod(i,2),1:2)
        write(10,*) root%NodCoord(root%ElemNod(i,3),1:2)
        write(10,*) root%NodCoord(root%ElemNod(i,4),1:2)
        write(10,*) root%NodCoord(root%ElemNod(i,1),1:2)
        write(10,*) " "
    enddo
    close(10)
end program main