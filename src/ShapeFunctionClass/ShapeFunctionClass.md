# ShapeFunctionClass 

### Attributes


    type::FEmesh_
        real(8),allocatable::Nmat(:)
        real(8),allocatable::dNdgzi(:,:)
        real(8),allocatable::dNdgzidgzi(:,:)
        real(8),allocatable::gzi(:)
        real(8),allocatable::GaussPoint(:,:)
        real(8),allocatable::GaussIntegWei(:)

        integer :: NumOfNode
        integer :: NumOfOrder
        integer :: NumOfDim
        integer :: NumOfGp
        integer :: GpID
        integer :: ierr
        
        character(len=60):: ErrorMsg

    end type FEmesh_



ShapeFunctionClass.f90

    - GetGaussPoint.f90     : Import Gauss points.
    - SetGaussPoint.f90     : Set the coordinates of current Gauss points into a buffer. 
    - GetShapeFunction.f90  : Get the value of shape function for the current Gauss point.
    - GetShapeFuncDer1.f90  : Get the 1st derivative of shape function for the current Gauss point.
    - GetShapeFuncDer2.f90  : Get the 1st derivative of shape function for the current Gauss point.




## Logs
2019/01/01  Created

2019/01/01  Debuged by test.f90

