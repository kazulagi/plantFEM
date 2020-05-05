module WaterAbsorptionClass
    use fem
    use DiffusionEquationClass
    use FiniteDeformationClass
    implicit none

    type :: WaterAbsorption_
        type(FEMDomain_),pointer:: Water, Tissue
        type(DiffusionEq_)::DiffusionEq
        type(FiniteDeform_)::FiniteDeform
    contains
        procedure, public :: import=> importWaterAbsorption
        procedure, public :: init=> initWaterAbsorption
        procedure, public :: update=> updateWaterAbsorption
        procedure, public :: export=> exportWaterAbsorption
    end type

contains

!#####################################
subroutine importWaterAbsorption(obj,Water,Tissue)
    class(WaterAbsorption_),intent(inout) :: obj
	type(FEMDomain_),target,intent(inout) :: Water,Tissue
    obj%Water => Water
    obj%Tissue => Tissue
end subroutine importWaterAbsorption
!#####################################


!#####################################
subroutine initWaterAbsorption(obj)
    class(WaterAbsorption_),intent(inout) :: obj

end subroutine initWaterAbsorption
!#####################################

!#####################################
subroutine updateWaterAbsorption(obj)
    class(WaterAbsorption_),intent(inout) :: obj
    
end subroutine updateWaterAbsorption
!#####################################


!#####################################
subroutine exportWaterAbsorption(obj,OptionalFileFormat,OptionalProjectName,FileHandle,&
    SolverType,MeshDimension,FileName,Name,regacy,with)
    class(WaterAbsorption_),intent(inout) :: obj
    class(FEMDomain_),optional,intent(inout)::with
    character(*),optional,intent(in)::OptionalFileFormat
    character(*),optional,intent(in)::OptionalProjectName,SolverType,FileName
	character*4::FileFormat
	character(*),optional,intent(in) :: Name
	logical,optional,intent(in) :: regacy
    character*200::ProjectName
	character*200 ::iFileName
	
    integer(int32),allocatable::IntMat(:,:)
    real(real64),allocatable::RealMat(:,:)
    integer(int32),optional,intent(in)::FileHandle,MeshDimension
    integer(int32) :: fh,i,j,k,NumOfDomain,n,m,DimNum,GpNum,nn
    character*70 Msg
    
    call obj%Water%export(OptionalFileFormat,OptionalProjectName,FileHandle,&
    SolverType,MeshDimension,FileName,Name,regacy,with)

    call obj%Tissue%export(OptionalFileFormat,OptionalProjectName,FileHandle,&
    SolverType,MeshDimension,FileName,Name,regacy,with)

end subroutine exportWaterAbsorption
!#####################################

end module WaterAbsorptionClass