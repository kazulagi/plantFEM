## Class Name :: BoundaryConditionClass

[README](README.md)>>[BoundaryConditionClass](Document/BoundaryConditionClass.md)

### Instruction:
BoundaryConditionClass focuses on defining and controlling boundary conditions. It uses MeshOperationClass and ArrayOperationClass
Following methods are defined

### Requirements
- ArrayOperationClass
- MeshOperationClass

### Attribute/Datatype

```
    type::Boundary_
        real(8),allocatable::DBoundVal(:,:)
        real(8),allocatable::NBoundVal(:,:)  
        real(8),allocatable::TBoundVal(:,:)  
        real(8),allocatable::TBoundElemGpVal(:,:,:)
        real(8),allocatable::DBoundValInc(:,:)
        real(8),allocatable::NBoundValInc(:,:)  
        real(8),allocatable::TBoundValInc(:,:)

        integer,allocatable::DBoundNodID(:,:)
        integer,allocatable::NBoundNodID(:,:)
        integer,allocatable::TBoundNodID(:,:)
        integer,allocatable::TBoundElemID(:)

        integer,allocatable::DBoundNum(:)
        integer,allocatable::NBoundNum(:)
        integer,allocatable::TBoundNum(:)
        integer,allocatable::TBoundElemNum(:)


        character*70 :: ErrorMsg
    end type Boundary_
```


* CheckDatatypeBoundary

```
subroutine CheckDatatypeBoundary(obj)
    class(Boundary_),intent(inout)::obj
```

* DeallocateBoundary
```
subroutine DeallocateBoundary(obj)
    class(Boundary_),intent(inout)::obj
```


* InitializeBoundary
```
subroutine InitializeBoundary(obj)
    class(Boundary_),intent(inout)::obj
```


* DeleteOverlapBoundary
```
subroutine DeleteOverlapBoundary(obj)
    class(Boundary_),intent(inout)::obj
```


* ImportDBound
```
ImportDBound(obj,Node_ID,DValue)
    class(Boundary_),intent(inout)::obj
    real(8),intent(in)::DValue(:,:)
    integer,intent(in)::Node_ID(:,:)
```


* MergeDBound
```
subroutine MergeDBound(BCObj1,MeshObj1,BCObj2,MeshObj2,OutBCObj)
    class(Boundary_),intent(in)::BCObj1,BCObj2
    class(Mesh_),intent(in)::MeshObj1,MeshObj2
    class(Boundary_),intent(inout)::OutBCObj
```


* MergeNBound
```
subroutine MergeNBound(BCObj1,MeshObj1,BCObj2,MeshObj2,OutBCObj)
    class(Boundary_),intent(in)::BCObj1,BCObj2
    class(Mesh_),intent(in)::MeshObj1,MeshObj2
    class(Boundary_),intent(inout)::OutBCObj
```


* ImportNBound
```
subroutine ImportNBound(obj,Node_ID,NValue)
    class(Boundary_),intent(inout)::obj
    real(8),intent(in)::NValue(:,:)
    integer,intent(in)::Node_ID(:,:)
```
