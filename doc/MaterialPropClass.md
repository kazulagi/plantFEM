## Class Name :: MaterialPropClass

[README](README.md)>>[MaterialPropClass](Document/MaterialPropClass.md)

### Instruction:
MaterialPropClass focuses on keeping material informations. Following method are defined.

* DeallocateMaterialProp
```
subroutine DeallocateMaterialProp(obj)
    class(MaterialProp_),intent(inout)::obj
```

* initializeMaterial
```
subroutine initializeMaterial(obj)
    class(MaterialProp_),intent(inout)::obj

```

* ImportMatPara
```
subroutine ImportMatPara(obj,mat_para)
    class(MaterialProp_),intent(inout)::obj
    real(8),intent(in)::mat_para(:,:)

```

* MergeMaterialProp
```
subroutine MergeMaterialProp(inobj1,inobj2,outobj)
    class(MaterialProp_),intent(in)::inobj1,inobj2
    class(MaterialProp_),intent(out)::outobj

```

* ShowMatPara
```
subroutine ShowMatPara(obj)
    class(MaterialProp_),intent(in)::obj

```

### Attribute/DataType
```

    type :: MaterialProp_
        real(8),allocatable::MatPara(:,:)
        integer :: NumOfMatPara
        integer :: NumOfMaterial        

        character*70 ErrorMsg
    end type MaterialProp_
```