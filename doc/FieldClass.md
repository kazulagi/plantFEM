## Class Name :: FieldClass

[README](README.md)>>[FieldClass](Document/FieldClass.md)

### Instruction:
FieldClass is a group of some FEMDomains and some FEMIfaces, which has all information of the field. Following methods are available. 


* ImportField
```
subroutine ImportField(obj,OptionalDomainListName,OptionalIfaceListName,OptionalProjectName,OptionalFileHandle)
    type(Field_),target,intent(inout)::obj
    character*200,optional,intent(in)::OptionalDomainListName
    character*200,optional,intent(in)::OptionalIfaceListName
    character*200,optional,intent(in)::OptionalProjectName
    integer,optional,intent(in)::OptionalFileHandle

```



* ShiftField
```
subroutine ShiftField(obj,distance,Optionaldirection)
    type(field_),intent(inout)::obj
    integer,optional,intent(in)::Optionaldirection
    real(8),intent(in) :: distance
```



* ExportField
```
subroutine ExportField(obj,OptionalDomainListName,OptionalIfaceListName,&
    OptionalProjectName,OptionalFileHandle)
    type(Field_),target,intent(inout)::obj
    character*200,optional,intent(in)::OptionalDomainListName
    character*200,optional,intent(in)::OptionalIfaceListName
    character*200,optional,intent(in)::OptionalProjectName
    integer,optional,intent(in)::OptionalFileHandle
    
```


### Attributes/DataTypes
```

    type :: FieldObjName_
        character*200  :: FieldObjName
    end type
```
```
    type :: Field_
        type(FEMDomain_),allocatable::FEMDomainArray(:)
        type(FEMIface_) ,allocatable::FEMIfaceArray(:)
        type(FieldObjName_),allocatable::FieldList(:)
        Integer,allocatable::Timestep(:)
        real(8),allocatable::RealTime(:)
        integer :: NumberOfObject,NumberOfIface

        character*200 :: FolderName
        character*200 :: DomainListName
        character*200 :: IfaceListName
    end type
```

### Requirements

- FEMDomainClass
- FEMIfaceClass
- DiffusionEquationClass
- FiniteDeformationClass