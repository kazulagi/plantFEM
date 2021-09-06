if(allocated(elem_nod ))then
    deallocate(elem_nod)
endif
allocate(elem_nod(size(obj%ElemNod,1),size(obj%ElemNod,2) ) )
elem_nod(:,:)=obj%ElemNod(:,:)