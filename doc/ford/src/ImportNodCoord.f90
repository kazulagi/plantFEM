
    if(allocated(obj%NodCoord) )then
        deallocate(obj%NodCoord)
    endif
    allocate(obj%NodCoord(size(nod_coord,1),size(nod_coord,2) ) )
    obj%NodCoord(:,:)=nod_coord(:,:)

    if(allocated(obj%SubMeshNodFromTo))then
        deallocate(obj%SubMeshNodFromTo)
    endif
    allocate(obj%SubMeshNodFromTo(1,3 ))
    obj%SubMeshNodFromTo(1,1)=1
    obj%SubMeshNodFromTo(1,2)=1
    obj%SubMeshNodFromTo(1,3)=size(nod_coord,1)