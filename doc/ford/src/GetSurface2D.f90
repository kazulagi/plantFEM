integer :: i,j,k,n
integer :: NumOfElem,NumOfDim,NumNodePerElem
integer :: id_1,id_2
integer,allocatable::buffer(:,:)


NumOfElem = size(obj%ElemNod,1) 
NumOfDim  = size(obj%NodCoord,2)
NumNodePerElem = size(obj%ElemNod,2)

If(NumOfDim /= 2) then
    obj%ErrorMsg = "ERROR::GetFaceElement.f90 >> NumOfDim /= 2"
    return
endif

call GetFacetElement(obj)
stop "sfvjkh"
!initialize
allocate(buffer(size(obj%FacetElemNod,1),size(obj%FacetElemNod,2)) )
buffer(1,:)=obj%FacetElemNod(1,:)

!buffer is arranged by clock-wize
do i=1,size(obj%FacetElemNod,1)-1
    id_2=buffer(i,2)
    do j=1,size(obj%FacetElemNod,1)
        if(id_2==obj%FacetElemNod(j,1) )then
            buffer(i+1,:)=obj%FacetElemNod(j,:)
        else
            cycle
        endif
    enddo
enddo
if(allocated(obj%SurfaceLine2D) ) then
    deallocate(obj%SurfaceLine2D)
endif
allocate(obj%SurfaceLine2D(size(buffer,1) ) )
do i=1,size(buffer,1)
    obj%SurfaceLine2D(size(buffer,1)-i+1)=buffer(i,1)
enddo


    
if(allocated(obj%SubMeshSurfFromTo))then
    deallocate(obj%SubMeshSurfFromTo)
endif
allocate(obj%SubMeshSurfFromTo(1,3 ))
obj%SubMeshSurfFromTo(1,1)=1
obj%SubMeshSurfFromTo(1,2)=1
obj%SubMeshSurfFromTo(1,3)=size(obj%SurfaceLine2D,1)