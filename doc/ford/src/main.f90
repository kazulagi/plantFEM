program main
    use ShapeFunctionClass
    implicit none

    type(FEmesh_)::shapefunc

    integer i,j

    shapefunc%NumOfNode = 8
    shapefunc%NumOfDim  = 3
    shapefunc%NumOfGp   = 8
    do j=1,shapefunc%NumOfGp
        shapefunc%GpID=j

        call GetGaussPoint(shapefunc)
        call SetGaussPoint(shapefunc)
        call GetShapeFunction(shapefunc)
        call GetShapeFuncDer1(shapefunc)
        call GetShapeFuncDer2(shapefunc)

        do i=1,size(shapefunc%dNdgzi,2)
            print *, shapefunc%dNdgzi(:,i)
        enddo
        print *, shapefunc%ErrorMsg
        print *, shapefunc%ierr
    enddo
end program main