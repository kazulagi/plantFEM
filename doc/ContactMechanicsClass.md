## Class Name :: ContactMechanicsClass

[README](README.md)>>[ContactMechanicsClass](Document/ContactMechanicsClass.md)
### Instruction:
ContactMechanicsClass focuses on computing contact phenomena such as collision, separation, stick, slip. Following methods are available.

### Attribute/DataType
```

    type :: ContactMechanics_
        type(FEMIface_),pointer::FEMIface
        real(8),allocatable    ::KcontactEBE(:,:,:)
        real(8),allocatable    ::KcontactGlo(:,:)
        real(8),allocatable    ::FcontactEBE(:,:)
        real(8),allocatable    ::FcontactGlo(:)
        real(8),allocatable    ::DispVecEBE(:,:)
        real(8),allocatable    ::DispVecGlo(:)
        real(8),allocatable    ::NTSvariables(:,:)
    end type
```

### Requirements

- MathClass
- FEMIfaceClass