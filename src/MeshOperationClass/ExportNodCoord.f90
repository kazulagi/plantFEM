if(allocated(nod_coord) )then
    deallocate(nod_coord)
endif
allocate(nod_coord(size(obj%NodCoord,1),size(obj%NodCoord,2) ) )
nod_coord(:,:)=obj%NodCoord(:,:)