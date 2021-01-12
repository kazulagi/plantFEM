
    if(allocated(obj%ElemMat) )then
        deallocate(obj%ElemMat)
    endif
    allocate(obj%ElemMat(size(elem_mat,1) ) )
    obj%ElemMat(:)=elem_mat(:)