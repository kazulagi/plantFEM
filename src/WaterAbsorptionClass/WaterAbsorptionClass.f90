module WaterAbsorptionClass
    use fem
    use DiffusionEquationClass
    use FiniteDeformationClass
    implicit none

    type :: WaterAbsorption_
        type(FEMDomain_),pointer:: Water, Tissue
        type(DiffusionEq_)::DiffusionEq
        type(FiniteDeform_)::FiniteDeform
        integer(int32) :: timestep
        real(real64) :: dt
    contains
        procedure, public :: import=> importWaterAbsorption
        procedure, public :: init=> initWaterAbsorption
        procedure, public :: run => runWaterAbsorption
        procedure, public :: update=> updateWaterAbsorption
        procedure, public :: export=> exportWaterAbsorption
        procedure, public :: display => displayWaterAbsorption
    end type

contains

!#####################################
subroutine importWaterAbsorption(obj,Water,Tissue)
    class(WaterAbsorption_),intent(inout) :: obj
    type(FEMDomain_),target,intent(inout) :: Water,Tissue
    if(associated(obj%DiffusionEq%FEMDomain) )then
        nullify(obj%DiffusionEq%FEMDomain)
    endif

    if(associated(obj%FiniteDeform%FEMDomain) )then
        nullify(obj%FiniteDeform%FEMDomain)
    endif
    obj%DiffusionEq%FEMDomain => Water
    obj%FiniteDeform%FEMDomain => Tissue
    
    Water%SolverType  = "DiffusionEq_"
    Tissue%SolverType = "FiniteDeform_" 

    print *, "[ImportFile]>"


end subroutine importWaterAbsorption
!#####################################

!#####################################
subroutine runWaterAbsorption(obj,timestep,dt,SolverType,onlyInit,Only1st,Display)
    class(WaterAbsorption_),intent(inout) :: obj
    character(*),intent(in) :: SolverType
    integer(int32) :: i
    logical,optional,intent(in) :: onlyInit,Only1st,Display
    integer(int32),intent(in) :: timestep
    real(real64),optional,intent(in) :: dt

        
    obj%dt=input(default=1.0d0,option=dt)
    obj%timestep=timestep
    
    obj%DiffusionEq%dt  = obj%dt
    obj%FiniteDeform%dt = obj%dt

    print *, "[ImportFile]>>>[Initialize]>"
    call obj%init(SolverType,Display=Display)
    print *, "[ImportFile]>>>[Initialize]>>>[1st Step]>"
    if(present(onlyInit) )then
        return
    endif
    ! Repeat over time
    do i=2,timestep
        print *, "Timestep :: ",i,"/",timestep   
        call obj%update(SolverType,Display=Display,step=i)

        if(present(only1st) )then
            return
        endif
    enddo


end subroutine runWaterAbsorption
!#####################################

!#####################################
subroutine initWaterAbsorption(obj,SolverType,Display)
    class(WaterAbsorption_),intent(inout) :: obj
    type(Term_)             :: term
    character(*),intent(in) :: SolverType
    logical,optional,intent(in) :: Display

    call term%init()
    ! ###### Diffusion Part ###################
    call obj%DiffusionEq%Setup()
    call obj%DiffusionEq%Solve(SolverType=SolverType)
    if(present(Display) )then
        if(Display .eqv. .true.)then
            call DisplayDiffusionEq(obj%DiffusionEq,&
            DisplayMode=term%Gmsh,OptionalStep=1)
        endif
    endif

    ! ###### Diffusion Part ###################   
    print *, "[ImportFile]>>>[Initialize]>>>[1st Step]>> water >>"
    
    ! ###### Finite deformation part #############################   

    call obj%FiniteDeform%DivideBC()
    call obj%FiniteDeform%Solve(SolverType=SolverType)  
    if(present(Display) )then
        if(Display .eqv. .true.)then
            call DisplayDeformStress(obj%FiniteDeform,&
                DisplayMode=term%gmsh,OptionalStep=1)   
            call DisplayReactionForce(obj%FiniteDeform)
        endif
    endif
    ! ###### Finite deformation part #############################
    print *, "[ImportFile]>>>[Initialize]>>>[1st Step]>>>>["
    
end subroutine initWaterAbsorption
!#####################################



!#####################################
subroutine updateWaterAbsorption(obj,SolverType,Display,step)
    class(WaterAbsorption_),intent(inout) :: obj
    character(*),intent(in) :: SolverType
    logical,optional,intent(in) :: Display
    integer(int32),optional,intent(in) :: step
    type(Term_)             :: term
    call term%init()
    ! ###### Update Diffusion Field over timesteps ###################
    call obj%DiffusionEq%Update()
    call obj%DiffusionEq%Solve(SolverType=SolverType)
    if(present(Display) )then
        if(Display .eqv. .true.)then
            call DisplayDiffusionEq(obj%DiffusionEq,&
            DisplayMode=term%Gmsh,OptionalStep=step)
        endif
    endif
    ! ###### Update Diffusion Field over timesteps ###################
    return
    ! ###### Update Finite Deformation over timesteps ###################
    call obj%FiniteDeform%UpdateInitConfig()
    call obj%FiniteDeform%UpdateBC()
    call obj%FiniteDeform%Solve(SolverType=SolverType) 
    if(present(Display) )then
        if(Display.eqv. .true.)then
            call DisplayDeformStress(obj%FiniteDeform,&
                DisplayMode=term%gmsh,OptionalStep=step)   
            call DisplayReactionForce(obj%FiniteDeform)
        endif
    endif
    ! ###### Update Finite Deformation over timesteps ###################

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