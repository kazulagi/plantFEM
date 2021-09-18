program main
    use fem
    implicit none

    type(FEMDomain_) :: domain
    type(ShapeFunction_) :: shapefn
    integer(int32) :: i
    
    call domain%create(meshtype="Cube")
    do i=1,size(domain%mesh%ElemNod,1)
        shapefn = domain%getShapeFunction(ElementID=i,GaussPointID=1)
        print *, real(shapefn%ElemCoord(1,:))
    enddo
    
    

end program main
