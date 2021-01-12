## Class Name :: ConstitutiveModelClass

[README](README.md)>>[ConstitutiveModelClass](Document/ConstitutiveModelClass.md)

### Instruction:
ConstitutiveModelClass focuses on computing stress tensors, and derivatives of stress tensors from strain measures. Following methods are available.


* HyperElasticStress
```
subroutine HyperElasticStress(obj)
    type(ConstModel_),intent(inout)::obj
```

* HyperElasticDer
```
subroutine HyperElasticDer(obj,DerType)
    type(ConstModel_),intent(inout)::obj
    character*70,intent(in)::DerType
```

### Requirements
- MathClass

### Attribute/Datatype

```

    type :: ConstModel_
        real(8),allocatable::sigma(:,:)
        real(8),allocatable::S_IJ(:,:)
        real(8),allocatable::M_IJ(:,:)
        real(8),allocatable::tau(:,:)
        real(8),allocatable::F_iJ(:,:)
        real(8),allocatable::F_T(:,:)
        real(8),allocatable::F_iJ_n(:,:)
        real(8),allocatable::F_inv_iJ(:,:)
        real(8),allocatable::F_T_inv_iJ(:,:)
        real(8),allocatable::Fp_iJ(:,:)
        real(8),allocatable::FT_Ij(:,:)
        real(8),allocatable::b_ij(:,:)
        real(8),allocatable::C_IJ(:,:)
        real(8),allocatable::C_IJ_n(:,:)
        real(8),allocatable::C_IJ_inv(:,:)
        real(8),allocatable::E_IJ(:,:)
        real(8),allocatable::Cp_IJ(:,:)
        real(8),allocatable::Cp_IJ_inv(:,:)
        real(8),allocatable::Cp_IJ_n(:,:)
        real(8),allocatable::Bmat(:,:)


        real(8),allocatable::StressDer(:,:,:,:)

        real(8) :: detF
        real(8) :: lamda
        real(8) :: mu
        real(8) :: K_mod
        real(8) :: G_mod
        
        
        character*70::ModelType
        character*70::Config    
    end type

```
