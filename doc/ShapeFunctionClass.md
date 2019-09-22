## Class Name :: ShapeFunctionClass

[README](README.md)>>[ShapeFunctionClass](Document/ShapeFunctionClass.md)

### Instruction:
ShapeFunctionClass is for assembling and computing shape function. Following methods are available.



* SetShapeFuncType
```
subroutine SetShapeFuncType(obj)
    class(ShapeFunction_),intent(inout)::obj

```




* GetAllShapeFunc
```
subroutine GetAllShapeFunc(obj,elem_id,nod_coord,elem_nod,OptionalNumOfNode,OptionalNumOfOrder,&
    OptionalNumOfDim,OptionalNumOfGp,OptionalGpID)
    class(ShapeFunction_),intent(inout)::obj
    integer,optional,intent(in)::elem_nod(:,:),elem_id
    real(8),optional,intent(in)::nod_coord(:,:)
    integer,optional,intent(in)::OptionalNumOfNode,OptionalNumOfOrder,&
    OptionalNumOfDim,OptionalNumOfGp,OptionalGpID

```




* DeallocateShapeFunction
```
subroutine DeallocateShapeFunction(obj)
    class(ShapeFunction_),intent(inout)::obj

```




* GetGaussPoint
```
subroutine GetGaussPoint(obj)
    class(ShapeFunction_),intent(inout)::obj

```




* SetGaussPoint
```
subroutine SetGaussPoint(obj)
    class(ShapeFunction_),intent(inout)::obj
```



* GetShapeFunction
```
subroutine GetShapeFunction(obj)
    class(ShapeFunction_),intent(inout)::obj

```



* GetShapeFuncDer1
```
subroutine GetShapeFuncDer1(obj)
    class(ShapeFunction_),intent(inout)::obj

```



* GetShapeFuncDer2
```
subroutine GetShapeFuncDer1(obj)
    class(ShapeFunction_),intent(inout)::obj

```



* GetElemCoord
```
subroutine GetElemCoord(obj,nod_coord,elem_nod,elem_id)
    class(ShapeFunction_),intent(inout)::obj
    integer,intent(in)::elem_nod(:,:),elem_id
    real(8),intent(in)::nod_coord(:,:)

```



* GetJmat
```
subroutine GetJmat(obj)
    class(ShapeFunction_),intent(inout)::obj

```


### Attribute/DataType
```

    type::ShapeFunction_
        real(8),allocatable::Nmat(:)
        real(8),allocatable::dNdgzi(:,:)
        real(8),allocatable::dNdgzidgzi(:,:)
        real(8),allocatable::gzi(:)
        real(8),allocatable::GaussPoint(:,:)
        real(8),allocatable::GaussIntegWei(:)
        real(8),allocatable::Jmat(:,:),JmatInv(:,:)
        real(8),allocatable::ElemCoord(:,:)

        real(8) :: detJ

        
        integer :: NumOfNode
        integer :: NumOfOrder
        integer :: NumOfDim
        integer :: NumOfGp
        integer :: GpID
        integer :: ierr
        
        character*70::ElemType
        character(len=60):: ErrorMsg

    end type ShapeFunction_
```

### Requirements
- MathClass