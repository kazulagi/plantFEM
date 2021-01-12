
    if(allocated(surface_nod) )then
        deallocate(surface_nod)
    endif
    allocate(surface_nod(size(obj%SurfaceLine2D,1) ) )
    surface_nod(:)=obj%SurfaceLine2D(:)
