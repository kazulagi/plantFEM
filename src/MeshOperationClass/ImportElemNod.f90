
    if(allocated(obj%ElemNod) )then
        deallocate(obj%ElemNod)
    endif
    allocate(obj%ElemNod(size(elem_nod,1),size(elem_nod,2) ) )
    obj%ElemNod(:,:)=elem_nod(:,:)

    
    if(allocated(obj%SubMeshElemFromTo))then
        deallocate(obj%SubMeshElemFromTo)
    endif
    allocate(obj%SubMeshElemFromTo(1,3 ))
    obj%SubMeshElemFromTo(1,1)=1
    obj%SubMeshElemFromTo(1,2)=1
    obj%SubMeshElemFromTo(1,3)=size(elem_nod,1)