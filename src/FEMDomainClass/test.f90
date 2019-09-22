program main
    use MeshOperationClass
    use FEMDomainClass
    implicit none
    
    type(Mesh_)::obj1,obj2,obj3
    integer i,n,m


    ! import
    open(10,file="debug.inp",status="old")
    read(10,*) n,m
    allocate(obj1%ElemNod(n,m) )
    do i=1,n
        read(10,*) obj1%ElemNod(i,1:m)
        print *,   obj1%ElemNod(i,1:m)
    enddo
    allocate(obj1%NodCoord(6,2) )
    obj1%NodCoord(:,:)=0.0d0
    obj1%NodCoord(:,:)=0.0d0
    obj1%NodCoord(1,1)=0.0d0
    obj1%NodCoord(2,1)=1.0d0
    obj1%NodCoord(3,1)=1.0d0
    obj1%NodCoord(4,1)=0.0d0
    obj1%NodCoord(5,1)=2.0d0
    obj1%NodCoord(6,1)=2.0d0
    obj1%NodCoord(1,2)=0.0d0
    obj1%NodCoord(2,2)=0.0d0
    obj1%NodCoord(3,2)=1.0d0
    obj1%NodCoord(4,2)=1.0d0
    obj1%NodCoord(5,2)=0.0d0
    obj1%NodCoord(6,2)=1.0d0

    allocate(obj1%ElemMat(size(obj1%ElemNod,1)))
    obj1%ElemMat=1
    close(10)


    open(10,file="debug.inp",status="old")
    read(10,*) n,m
    allocate(obj2%ElemNod(n,m) )
    do i=1,n
        read(10,*) obj2%ElemNod(i,1:m)
        print *,   obj2%ElemNod(i,1:m)
    enddo
    allocate(obj2%NodCoord(6,2) )
    obj2%NodCoord(:,:)=0.0d0+4.0d0
    obj2%NodCoord(1,1)=0.0d0+4.0d0
    obj2%NodCoord(2,1)=1.0d0+4.0d0
    obj2%NodCoord(3,1)=1.0d0+4.0d0
    obj2%NodCoord(4,1)=0.0d0+4.0d0
    obj2%NodCoord(5,1)=2.0d0+4.0d0
    obj2%NodCoord(6,1)=2.0d0+4.0d0
    obj2%NodCoord(1,2)=0.0d0
    obj2%NodCoord(2,2)=0.0d0
    obj2%NodCoord(3,2)=1.0d0
    obj2%NodCoord(4,2)=1.0d0
    obj2%NodCoord(5,2)=0.0d0
    obj2%NodCoord(6,2)=1.0d0
    allocate(obj2%ElemMat(size(obj2%ElemNod,1)))
    obj2%ElemMat=2
    
    close(10)

    !get facet
    call GetSurface2D(obj1)
    call GetSurface2D(obj2)
    !Merge
    call MergeMesh(obj1,obj2,obj3)



    print *, "obj1"
    do i=1,size(obj1%SubMeshNodFromTo,1)
        print *, "Subdomain",obj1%SubMeshNodFromTo(i,1),&
        "starts from node ID : ",obj1%SubMeshNodFromTo(i,2),&
        "up to node ID : ",obj1%SubMeshNodFromTo(i,3)
    enddo
    
    do i=1,size(obj1%SubMeshNodFromTo,1)
        print *, "Subdomain",obj1%SubMeshElemFromTo(i,1),&
        "starts from Element ID : ",obj1%SubMeshElemFromTo(i,2),&
        "up to Element ID : ",obj1%SubMeshElemFromTo(i,3)
    enddo

    do i=1,size(obj1%SubMeshNodFromTo,1)
        print *, "Subdomain",obj1%SubMeshSurfFromTo(i,1),&
        "starts from Surface node ID : ",obj1%SubMeshSurfFromTo(i,2),&
        "up to Surface node ID : ",obj1%SubMeshSurfFromTo(i,3)
    enddo

    print *, "obj3"
    do i=1,size(obj3%SubMeshNodFromTo,1)
        print *, "Subdomain",obj3%SubMeshNodFromTo(i,1),&
        "starts from node ID : ",obj3%SubMeshNodFromTo(i,2),&
        "up to node ID : ",obj3%SubMeshNodFromTo(i,3)
    enddo
    
    do i=1,size(obj3%SubMeshNodFromTo,1)
        print *, "Subdomain",obj3%SubMeshElemFromTo(i,1),&
        "starts from Element ID : ",obj3%SubMeshElemFromTo(i,2),&
        "up to Element ID : ",obj3%SubMeshElemFromTo(i,3)
    enddo

    do i=1,size(obj3%SubMeshNodFromTo,1)
        print *, "Subdomain",obj3%SubMeshSurfFromTo(i,1),&
        "starts from Surface node ID : ",obj3%SubMeshSurfFromTo(i,2),&
        "up to Surface node ID : ",obj3%SubMeshSurfFromTo(i,3)
    enddo

    !do i=1,size(obj3%ElemNod,1)
    !    print *, obj3%ElemNod(i,:)
    !enddo

    !do i=1,size(obj3%SurfaceLine2D,1)
    !    print *, obj3%SurfaceLine2D(i)
    !enddo

    call DisplayMesh(obj3)

end program main