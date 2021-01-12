## Class Name :: FiniteDeformationClass

[README](README.md)>>[FiniteDeformationClass](Document/FiniteDeformationClass.md)

### Instruction:
FiniteDeformationClass is focusing on deformation phenomena based on such modern deformation theory as Finite Strain Theory. Following methods are defined.

* SolveFiniteDeformNewton
```
subroutine SolveFiniteDeformNewton(obj,OptionItr,Solvertype)
	class(FiniteDeform_),intent(inout)::obj
	integer,optional,intent(in)::OptionItr
    character*70,optional,intent(in)::Solvertype
```


* DevideBCIntoTimestep
```
subroutine DevideBCIntoTimestep(obj)
	class(FiniteDeform_),intent(inout)::obj

```


* UpdateBCInTimestep
```
subroutine UpdateBCInTimestep(obj)
	class(FiniteDeform_),intent(inout)::obj

```


* ImportFEMDomainFiDe
```
subroutine ImportFEMDomainFiDe(obj,OptionalFileFormat,OptionalProjectName)
    class(FEMDomain_),intent(inout)::obj
    character*4,optional,intent(in)::OptionalFileFormat
    character*70,optional,intent(in)::OptionalProjectName

```


* SetupFiniteDeform
```
subroutine SetupFiniteDeform(obj)
    class(FiniteDeform_),intent(inout)::obj

```


* UpdateFiniteDeform
```
subroutine UpdateFiniteDeform(obj)
    class(FiniteDeform_),intent(inout)::obj

```


* UpdateCurrConfig
```
subroutine UpdateCurrConfig(obj)
    class(FiniteDeform_),intent(inout)::obj

```


* UpdateInitConfig
```
subroutine UpdateInitConfig(obj)
    class(FiniteDeform_),intent(inout)::obj

```


* GetDeformStressMatAndVector
```
subroutine GetDeformStressMatAndVector(obj,OptionalStep)
	class(FiniteDeform_),intent(inout)::obj
	type(ConstModel_)	::mdl

```


* GetKmat
```
subroutine GetKmat(obj,mdl,sf,Kmat_e,gvec_e,dim_num,elemnod_num,elem)
	type(FiniteDeform_),intent(in) :: obj
	type(ConstModel_),intent(inout)::mdl
	type(ShapeFunction_),intent(in)::sf
	real(8),intent(inout) :: Kmat_e(:,:),gvec_e(:)

```


* GetGvec
```
subroutine GetGvec(obj,mdl,sf,gvec_e,dim_num,elemnod_num,elem)
	type(FiniteDeform_),intent(in) :: obj
	type(ConstModel_),intent(inout)::mdl
	type(ShapeFunction_),intent(in)::sf
	real(8),intent(inout) :: gvec_e(:)
	integer,intent(in)::dim_num,elemnod_num,elem

```


* K_mat_ICU
```
subroutine K_mat_ICU(Kmat, elem_nod, i, Kemat)
   	integer, intent(in) :: i, elem_nod(:,:)
   	real(8), intent(in) :: Kemat(:,:)
	real(8), intent(inout) :: Kmat(:,:,:)

```


* g_vector_ICU
```
subroutine g_vector_ICU(elem,elem_nod,gvec_e,gvec)

	integer, intent(in) :: elem_nod(:,:),elem
	real(8),intent(in):: gvec_e(:)
	real(8),intent(inout)::gvec(:,:)
	
```

* F_tensor_ICU
```
subroutine F_tensor_ICU(obj,elem,gauss,F_iJ_n,F_iJ)
	class(FiniteDeform_),intent(inout)::obj
	integer, intent(in)::elem,gauss

```

* C_tensor
```
subroutine C_tensor(F,C_IJ,b_ij,itr,dim_num)
	real(8),allocatable::C_IJ(:,:),b_ij(:,:),F_T(:,:)
	real(8),intent(in)::F(:,:)
	integer,intent(in)::itr,dim_num

```

* Cp_tensor
```
subroutine Cp_tensor(elem,gauss,strain_measure,Cp_IJ_n,Cp_IJ,Cp_IJ_inv,dim_num)
	real(8),allocatable:: Cp_iJ_n(:,:),Cp_iJ(:,:),Cp_IJ_inv(:,:)
	real(8),intent(in)::strain_measure(:,:,:)
	integer, intent(in)::elem,gauss,dim_num

```

* M_neo_Hookean
```
subroutine M_neo_Hookean(C_IJ,Cp_IJ,Cp_IJ_inv,M_IJ,Lamda,mu,elem,gauss)
	real(8),intent(in)::C_IJ(:,:),Lamda,mu,Cp_IJ(:,:),Cp_IJ_inv(:,:)
	real(8),allocatable::M_IJ(:,:),G_IJ(:,:),C_Cp_1(:,:)
	integer, intent(in):: elem,gauss
```

* Return_Mapping_MCDP
```
subroutine Return_Mapping_MCDP(dim_num,elem,gauss,C_IJ,Cp_IJ,Cp_IJ_n,Cp_IJ_inv,M_IJ,MatPara,&
   itr_rm,tol,sigma,F_T,F_T_inv,itr,itr_contact,strain_measure,step)
	real(8),intent(in)::C_IJ(:,:),Cp_IJ_n(:,:),F_T(:,:),F_T_inv(:,:),MatPara(:)
	real(8),intent(inout)::Cp_IJ(:,:),sigma(:,:,:),strain_measure(:,:,:)
	real(8),allocatable,intent(inout)::M_IJ(:,:),Cp_IJ_inv(:,:)
	integer, intent (in)::elem,gauss,itr,itr_rm,step,itr_contact,dim_num

```

* Ce_neoHK_current
```
subroutine Ce_neoHK_current(dim_num, elem, gauss,Lame1,Lame2,C_IJ,Cp_IJ,b_ij,M_IJ,Ce_neoHK,F_T,F_T_inv,ij)
     integer, intent(in) :: dim_num,elem, gauss
     real(8), intent(in) :: Lame1,Lame2,C_IJ(:,:),Cp_IJ(:,:),b_ij(:,:),F_T(:,:),F_T_inv(:,:),M_IJ(:,:)
     real(8), allocatable, intent(out) :: Ce_neoHK(:,:)
	 integer,allocatable,intent(out)::ij(:,:)
	 
```

* GetSigmaVec
```
subroutine GetSigmaVec(Sigma,Sigma_ij,dim_num)
     real(8), intent(in) :: Sigma_ij(:,:)
     real(8), allocatable, intent(inout) :: Sigma(:)
	 integer,allocatable::ij(:,:)
	 integer,intent(in)::dim_num

```

* GetDmat
```
subroutine GetDmat(Dmat,c_ijkl,dim_num)
     real(8), intent(in) :: c_ijkl(:,:,:,:)
     real(8), allocatable, intent(inout) :: Dmat(:,:)
	 integer,allocatable::ij(:,:)
	 integer,intent(in)::dim_num

```

* B_mat
```
subroutine B_mat(dim_num,Psymat, Jmat, detJ, Bmat,mm)
     real(8), intent(in) :: Psymat(:,:), Jmat(:,:), detJ ! J�̋t�s��
     real(8), allocatable, intent(inout) :: Bmat(:,:)
	 integer,intent(in)::dim_num

```

* K_mat_e
```
subroutine K_mat_e(j,s, BTmat, Ce_neoHK, Bmat, detJ, Kmat_e,F_iJ)
   integer, intent(in) :: j
   real(8), intent(in) :: BTmat(:,:), Ce_neoHK(:,:), Bmat(:,:), detJ, s(:),F_iJ(:,:)
   real(8), intent(out) :: Kmat_e(:,:)
```

* g_vector_e
```
subroutine g_vector_e(elem,gauss,s, BTmat,sigma, detJ, gvec_e)

    integer, intent(in) :: elem,gauss
    real(8), intent(in) :: BTmat(:,:), sigma(:,:,:), detJ, s(:)
    real(8), intent(inout) :: gvec_e(:)

```

* SolveFiniteDeform
```
subroutine SolveFiniteDeform(obj,OptionItr,Solvertype)
	class(FiniteDeform_),intent(inout)::obj
	integer,optional,intent(in)::OptionItr
    character*70,optional,intent(in)::Solvertype

```

* DisplayDeformStress
```
subroutine DisplayDeformStress(obj,OptionalProjectName,DisplayMode,OptionalStep)
    class(FiniteDeform_),intent(inout)::obj
    character*70,optional,intent(in) :: OptionalProjectName,DisplayMode
    integer,optional,intent(in)::OptionalStep

```

* GetTractionVector
```
subroutine GetTractionVector(obj)
	class(FiniteDeform_),intent(inout)::obj

```

* GetInternalVector
```
subroutine GetInternalVector(obj)
	class(FiniteDeform_),intent(inout)::obj


```

* GetResidualVector
```
subroutine GetResidualVector(obj)
	class(FiniteDeform_),intent(inout)::obj

```

* UpdateStrainMeasure
```
subroutine UpdateStrainMeasure(obj)
	class(FiniteDeform_),intent(inout)::obj

```

* DisplayReactionForce
```
subroutine DisplayReactionForce(obj)
	class(FiniteDeform_),intent(in)::obj

```

### Attribute/DataType
```

type:: FiniteDeform_
	type(FEMDomain_),pointer ::FEMDomain
    real(8),allocatable ::DeformStress(:,:,:)
    real(8),allocatable ::DeformStrain(:,:,:)
    real(8),allocatable ::DeformStressInit(:,:,:)
	real(8),allocatable ::DeformStressMat(:,:,:)
	real(8),allocatable ::DeformStressRHS(:,:)
	real(8),allocatable ::DeformVecEBETot(:,:)
    real(8),allocatable ::DeformVecEBEInc(:,:)
    real(8),allocatable ::DeformVecGloTot(:)
	real(8),allocatable ::DeformVecGloInc(:)
	
	real(8),allocatable ::TractionVecGlo(:)
	real(8),allocatable ::ResidualVecGlo(:)
	real(8),allocatable ::InternalVecGlo(:)
	real(8),allocatable ::VolInitCurrEBE(:,:)
	real(8)             ::dt,error
	
	integer :: itr,Step
end type
```

### Requirements
- MathClass
- LinearSolverClass
- FEMDomainClass
- PostProcessingClass
- ConstitutiveModelClass