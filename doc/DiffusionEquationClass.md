## Class Name :: DiffusionEquationClass

[README](README.md)>>[DiffusionEquationClass](Document/DiffusionEquationClass.md)

### Instruction:
DiffusionEquationClass focuses on computing diffusion phenomena such as heat transfer, waterflow, and concentration diffusion. Following methods are available.

* ImportFEMDomainDiff   

```
subroutine ImportFEMDomainDiff(obj,OptionalFileFormat,OptionalProjectName)
    class(FEMDomain_),intent(inout)::obj
    character*4,optional,intent(in)::OptionalFileFormat
    character*70,optional,intent(in)::OptionalProjectName

```


* SolveDiffusionEq
```
subroutine SolveDiffusionEq(obj,Solvertype)
    class(DiffusionEq_),intent(inout)::obj
    character*70,optional,intent(in)::Solvertype
```

* UpdateDiffusionEq
```
subroutine UpdateDiffusionEq(obj)
    class(DiffusionEq_),intent(inout)::obj
```



* SetupDiffusionEq
```
subroutine SetupDiffusionEq(obj)
    class(DiffusionEq_),intent(inout)::obj
```



* GetDiffusionMat
```
subroutine GetDiffusionMat(obj)
    class(DiffusionEq_),intent(inout)::obj
```



* GetFlowvector
```
subroutine GetFlowvector(obj)
    class(DiffusionEq_),intent(inout)::obj
```



* GetUnknownValue
```
subroutine GetUnknownValue(obj)
    class(DiffusionEq_),intent(inout)::obj
```



* GetDivergence
```
subroutine GetDivergence(obj)
    class(DiffusionEq_),intent(inout)::obj
```



* UpdateUnknownValue
```
subroutine  UpdateUnknownValue(obj)
    class(DiffusionEq_),intent(inout)::obj
```



* GetElemDiffusionMatrix
```
subroutine GetElemDiffusionMatrix(obj,diff_coeff,DiffMat)
    class(ShapeFunction_),intent(inout)::obj
    real(8),intent(in)::diff_coeff
    real(8),allocatable,intent(inout)::DiffMat(:,:)
```



* GetElemMassMatrix
```
subroutine GetElemMassMatrix(obj,MassMat)
    class(ShapeFunction_),intent(inout)::obj
    real(8),allocatable,intent(inout)::MassMat(:,:)
```



* DisplayDiffusionEq
```
subroutine DisplayDiffusionEq(obj,OptionalProjectName,DisplayMode,OptionalStep)
    class(DiffusionEq_),intent(inout)::obj
    character*70,optional,intent(in) :: OptionalProjectName,DisplayMode
    integer,optional,intent(in)::OptionalStep
```


### Attribute/DataType


```
    type:: DiffusionEq_
        type(FEMDomain_),pointer ::FEMDomain
        real(8),allocatable ::UnknownValue(:,:)
        real(8),allocatable ::UnknownValueInit(:,:)
        real(8),allocatable ::UnknownValueRate(:,:)
        real(8),allocatable ::DiffusionMat(:,:,:)
        real(8),allocatable ::Divergence(:,:)
        real(8),allocatable ::Flowvector(:,:)
        real(8)             ::dt
        integer             :: step
    end type
```

### Requirements

- FEMDomainClass
- PostProcessingClass
- LinearSolverClass