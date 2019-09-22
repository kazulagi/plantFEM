## Class Name :: MultiPhysicsClass

[README](README.md)>>[MultiPhysicsClass](Document/MultiPhysicsClass.md)

### Instruction:
MultiPhysicsClass is a class for Multi-Physical problems in which two domains interacts. Following methods are implemented.



* SyncMesh
```
subroutine SyncMesh(obj,Arrow)
    type(MultiPhysics_),intent(inout)::obj
    character*2,optional,intent(in)::Arrow

```


### Attribute/DataType

```
    type :: MultiPhysics_
        type(FEMIface_),pointer :: FEMIface
        type(FEMDomain_),pointer :: FEMDomain1
        type(FEMDomain_),pointer :: FEMDomain2
    end type
```

### Requirements
- FEMIfaceClass
- FEMDomainClass
