## Class Name :: FEMIfaceClass

[README](README.md)>>[FEMIfaceClass](Document/FEMIfaceClass.md)

### Instruction:
FEMIfaceClass focuses on defining and operating interface objects for universal problems for Finite Element Method. Following methods are defined.

* DeallocateFEMIface
```
subroutine DeallocateFEMIface(obj)
    type(FEMIface_),intent(inout)::obj
```


* ImportFEMIface
```
subroutine ImportFEMIface(obj,OptionalFileFormat,OptionalProjectName,FileHandle)
    class(FEMIface_),intent(inout)::obj
    character*4,optional,intent(in)::OptionalFileFormat
    character*70,optional,intent(in)::OptionalProjectName

```

* GetFEMIface
```
subroutine GetFEMIface(obj1,obj2,obj)
    class(FEMDomain_),intent(inout)::obj1,obj2
    class(FEMIface_),intent(inout)::obj

```

* GetNTNelement
```
subroutine GetNTNelement(obj)
    type(FEMIface_),intent(inout)::obj
```

* GetNTSelement
```
subroutine GetNTSelement(obj)
    type(FEMIface_),intent(inout)::obj
```

* GetSTSelement
```
subroutine GetSTSelement(obj)
    type(FEMIface_),intent(inout)::obj
```

* ExportFEMIface
```
subroutine ExportFEMIface(obj,OptionalFileFormat,OptionalProjectName,FileHandle)
    class(FEMIface_),intent(inout)::obj
    character*4,optional,intent(in)::OptionalFileFormat
    character*70,optional,intent(in)::OptionalProjectName

```

### Attributes/DataType

```

    type :: FEMIface_
        type(ShapeFunction_)    :: ShapeFunction1,ShapeFunction2
        type(Mesh_)             :: Mesh1,Mesh2
        type(MaterialProp_)     :: MaterialProp
        type(ControlParameter_) :: ControlPara
        real(8),allocatable     :: NTN_NodCoord(:,:)
        real(8),allocatable     :: NTS_NodCoord(:,:)
        real(8),allocatable     :: STS_NodCoord(:,:)
        real(8),allocatable     :: NTN_Val(:,:)
        real(8),allocatable     :: NTS_Val(:,:)
        real(8),allocatable     :: STS_Val(:,:)
        integer,allocatable     :: NTN_ElemNod(:,:)
        integer,allocatable     :: NTS_ElemNod(:,:)
        integer,allocatable     :: NTS_SegmentID(:,:)
        integer,allocatable     :: STS_ElemNod(:,:)
        integer                 :: DomainID1
        integer                 :: DomainID2
        integer                 :: DomainID3
        character*200           :: FilePathDomain1
        character*200           :: FilePathDomain2
        character*200           :: FilePath

        character*200 :: FileNameDomain1
        character*200 :: FileNameDomain2
        character*200 :: FileName
        character*9   :: Dtype
        character*20  :: SolverType
    end type
```

### Requirements

- MathClass
- ArrayOperationClass
- ShapeFunctionClass
- MeshOperationClass
- MaterialPropClass
- BoundaryConditionClass
- ControlParaeterClass
- FEMDomainClass
