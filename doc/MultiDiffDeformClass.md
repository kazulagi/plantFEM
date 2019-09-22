## Class Name :: MultiDiffDeformClass

[README](README.md)>>[MultiDiffDeformClass](Document/MultiDiffDeformClass.md)

### Instruction:
MultiDiffDeformClass is about muti-phyisics of Diffusion and Finite Deformation. Following is a method.

* EnforceMassConserv
```
subroutine EnforceMassConserv(difobj,defobj)
    class(DiffusionEq_) ,intent(inout)::difobj
    class(FiniteDeform_),intent(in   )::defobj

```

### Requirement

- DiffusionEquationClass
- FiniteDeformationClass
