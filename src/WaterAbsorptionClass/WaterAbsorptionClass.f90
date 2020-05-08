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
        procedure, public :: run => runWaterAbsorption
        procedure, public :: update=> updateWaterAbsorption
        procedure, public :: initHydraulicField => initHydraulicFieldWA
        procedure, public :: updateHydraulicField => updateHydraulicFieldWA
        procedure, public :: initMechanicalField => initMechanicalFieldWA
        procedure, public :: updateMechanicalField => updateMechanicalFieldWA
        procedure, public :: export=> exportWaterAbsorption
        procedure, public :: display => displayWaterAbsorption
    end type

contains

!#####################################
subroutine importWaterAbsorption(obj,Water,Tissue)
    class(WaterAbsorption_),intent(inout) :: obj
	type(FEMDomain_),target,intent(inout) :: Water,Tissue
    obj%DiffusionEq%FEMDomain => Water
    obj%FiniteDeform%FEMDomain => Tissue

end subroutine importWaterAbsorption
!#####################################

!#####################################
subroutine runWaterAbsorption(obj,timestep)
    class(WaterAbsorption_),intent(inout) :: obj
    integer(int32),intent(in) :: timestep
    integer(int32) :: i

    return
    call obj%init()
    ! Repeat over time
    do i=1,timestep
        call obj%update()
    enddo


end subroutine runWaterAbsorption
!#####################################

!#####################################
subroutine initWaterAbsorption(obj)
    class(WaterAbsorption_),intent(inout) :: obj

    ! implement as like this
    call obj%initHydraulicField()
    call obj%initMechanicalField()
end subroutine initWaterAbsorption
!#####################################



!#####################################
subroutine updateWaterAbsorption(obj)
    class(WaterAbsorption_),intent(inout) :: obj
    call obj%updateHydraulicField()
    call obj%updateMechanicalField()

end subroutine updateWaterAbsorption
!#####################################



!#####################################
subroutine initHydraulicFieldWA(obj)
    class(WaterAbsorption_),intent(inout) :: obj

end subroutine initHydraulicFieldWA
!#####################################


!#####################################
subroutine updateHydraulicFieldWA(obj)
    class(WaterAbsorption_),intent(inout) :: obj

end subroutine updateHydraulicFieldWA
!#####################################




!#####################################
subroutine initMechanicalFieldWA(obj)
    class(WaterAbsorption_),intent(inout) :: obj

end subroutine initMechanicalFieldWA
!#####################################


!#####################################
subroutine updateMechanicalFieldWA(obj)
    class(WaterAbsorption_),intent(inout) :: obj

end subroutine updateMechanicalFieldWA
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
    
    ! bug exsits
    !call obj%Tissue%export(SolverType="FiniteDeform",Name=Name)
    !call obj%Water%export(SolverType="DiffusionEq",Name=Name)

end subroutine exportWaterAbsorption
!#####################################

!#####################################
subroutine displayWaterAbsorption(obj)
    class(WaterAbsorption_),intent(inout) :: obj

    ! implement how to display the results.

end subroutine displayWaterAbsorption
!#####################################
end module WaterAbsorptionClass