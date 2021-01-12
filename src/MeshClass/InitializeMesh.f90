
    integer i,j,n1,n2

    if(.not.allocated(obj%NodCoord) )then
        obj%ErrorMsg="Caution :: Initialize >> .not.allocated(obj%NodCoord)"
        print *, obj%ErrorMsg 
        return
    endif
    n1=size(obj%NodCoord,1)
    if(allocated(obj%SubMeshNodFromTo ))then
        deallocate(obj%SubMeshNodFromTo)
    endif
    allocate(obj%SubMeshNodFromTo(1,3) )
    obj%SubMeshNodFromTo(1,1)=1
    obj%SubMeshNodFromTo(1,2)=1
    obj%SubMeshNodFromTo(1,3)=n1
    

    if(.not.allocated(obj%ElemNod) )then
        obj%ErrorMsg="Caution :: Initialize >> .not.allocated(obj%ElemNod)"
        print *, obj%ErrorMsg 
        return
    endif
    n1=size(obj%ElemNod,1)
    if(allocated(obj%SubMeshElemFromTo ))then
        deallocate(obj%SubMeshElemFromTo)
    endif
    allocate(obj%SubMeshElemFromTo(1,3) )
    obj%SubMeshElemFromTo(1,1)=1
    obj%SubMeshElemFromTo(1,2)=1
    obj%SubMeshElemFromTo(1,3)=n1

    call GetFacetElement(obj)
    call GetSurface2D(obj)
    
    if(.not.allocated(obj%SurfaceLine2D) )then
        obj%ErrorMsg="Caution :: Initialize >> .not.allocated(obj%ESurfaceLine2D)"
        print *, obj%ErrorMsg 
        return
    endif
    
    n1=size(obj%SurfaceLine2D,1)
    if(allocated(obj%SubMeshSurfFromTo ))then
        deallocate(obj%SubMeshSurfFromTo)
    endif
    
    allocate(obj%SubMeshSurfFromTo(1,3) )
    obj%SubMeshSurfFromTo(1,1)=1
    obj%SubMeshSurfFromTo(1,2)=1
    obj%SubMeshSurfFromTo(1,3)=n1