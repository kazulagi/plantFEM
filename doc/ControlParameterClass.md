## Class Name :: ControlParameterClass

[README](README.md)>>[ControlParameterClass](Document/ControlParameterClass.md)

### Instruction:
ControlParameterClass is keeping control parameter for analysis. Followings are methods.



* SetControlPara
```
subroutine SetControlPara(obj,OptionalTol,OptionalItrTol,OptionalTimestep,OptionalSimMode)
    class(ControlParameter_),intent(out)::obj
    real(8),optional,intent(in)::OptionalTol
    integer,optional,intent(in)::OptionalSimMode,OptionalItrTol,OptionalTimestep


```

### Attribute/DataType
```
    type::ControlParameter_
        real(8) :: Tol
        integer :: SimMode
        integer :: ItrTol
        integer :: Timestep
    end type ControlParameter_
```