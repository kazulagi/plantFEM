if(allocated(obj%gzi) ) then
    deallocate(obj%gzi)
endif
allocate(obj%gzi(obj%NumOfDim) )

if(obj%NumOfDim /= size(obj%GaussPoint,1) )then
    print *, "ERROR::SetGaussPoint",obj%NumOfDim, size(obj%GaussPoint,1)
    obj%ErrorMsg="ERROR::SetGaussPoint"
    obj%ierr=1
else
    obj%gzi(:) = obj%GaussPoint(:,obj%GpID )
    obj%ErrorMsg="Succeed::SetGaussPoint"
    obj%ierr=0
endif