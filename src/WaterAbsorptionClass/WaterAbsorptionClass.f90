module WaterAbsorptionClass
    use fem
    use DiffusionEquationClass
    use FiniteDeformationClass
    implicit none

    type :: WaterAbsorption_
        type(FEMDomain_),pointer:: Water, Tissue
        type(DiffusionEq_)::DiffusionEq
        type(FiniteDeform_)::FiniteDeform
        type(MaterialProp_),pointer:: a_Psi
        type(MaterialProp_),pointer:: a_P
        type(MaterialProp_),pointer:: theta_eq
        type(MaterialProp_),pointer:: theta_ps
        type(MaterialProp_),pointer:: Psi_eq
        type(MaterialProp_),pointer:: a_E
        type(MaterialProp_),pointer:: a_v
        type(MaterialProp_),pointer:: E_eq
        type(MaterialProp_),pointer:: v_eq
        real(real64),allocatable :: WaterAbsorbingPower(:)
        real(real64),allocatable :: WaterPotential(:)
        real(real64),allocatable :: TurgorPressure(:)
        real(real64),allocatable :: WaterContent(:)
        real(real64),allocatable :: Conductivity(:)
        real(real64),allocatable :: YoungsModulus(:)
        real(real64),allocatable :: PoissonsRatio(:)
        real(real64),allocatable :: Permiability(:)
        ! Parameters(A,B)
        ! A : Element ID 
        ! B: 1=a_Psi, 2=a_P, 3=theta_eq, 4=theta_ps, 5=Psi_eq, 
        ! 6=a_E, 7=a_v, 8=E_eq, 9=v_eq

        real(real64),allocatable :: a_Psi_val(:)
        real(real64),allocatable :: a_P_val(:)
        real(real64),allocatable :: theta_eq_val(:)
        real(real64),allocatable :: theta_ps_val(:)
        real(real64),allocatable :: Psi_eq_val(:)
        real(real64),allocatable :: a_E_val(:)
        real(real64),allocatable :: a_v_val(:)
        real(real64),allocatable :: E_eq_val(:)
        real(real64),allocatable :: v_eq_val(:)

        real(real64),allocatable :: Parameters(:,:)
        integer(int32) :: timestep
        real(real64) :: dt
    contains
        procedure, public :: import=> importWaterAbsorption
        procedure, public :: init=> initWaterAbsorption
        procedure, public :: run => runWaterAbsorption
        procedure, public :: update=> updateWaterAbsorption
        procedure, public :: export=> exportWaterAbsorption
        procedure, public :: display => displayWaterAbsorption
        procedure, public :: bake => bakeWaterAbsorption
    end type
contains
!#####################################
subroutine importWaterAbsorption(obj,Water,Tissue,a_Psi,a_P,theta_eq,theta_ps,&
    Psi_eq,a_E,a_v,E_eq,v_eq)
    class(WaterAbsorption_),intent(inout) :: obj
    type(FEMDomain_),target,optional,intent(inout) :: Water,Tissue

    type(MaterialProp_),target,optional,intent(in):: a_Psi
    type(MaterialProp_),target,optional,intent(in):: a_P
    type(MaterialProp_),target,optional,intent(in):: theta_eq
    type(MaterialProp_),target,optional,intent(in):: theta_ps
    type(MaterialProp_),target,optional,intent(in):: Psi_eq
    type(MaterialProp_),target,optional,intent(in):: a_E
    type(MaterialProp_),target,optional,intent(in):: a_v
    type(MaterialProp_),target,optional,intent(in):: E_eq
    type(MaterialProp_),target,optional,intent(in):: v_eq

    if(associated(obj%DiffusionEq%FEMDomain) )then
        nullify(obj%DiffusionEq%FEMDomain)
    endif

    if(associated(obj%FiniteDeform%FEMDomain) )then
        nullify(obj%FiniteDeform%FEMDomain)
    endif
    if(present(water) )then
        obj%DiffusionEq%FEMDomain => Water
        obj%water => water
        Water%SolverType  = "DiffusionEq_"
    endif
    if(present(tissue))then
        obj%FiniteDeform%FEMDomain => Tissue
        Tissue%SolverType = "FiniteDeform_" 
        obj%Tissue => Tissue
    endif

    
    print *, "[ImportFile]>"

    if(present(a_Psi) )then
        if(associated(obj%a_Psi) )then
            nullify(obj%a_Psi)
        endif
        obj%a_Psi => a_Psi
    endif
    
    if(present(a_P) )then
        if(associated(obj%a_P) )then
            nullify(obj%a_P)
        endif
        obj%a_P => a_P
    endif

    if(present(theta_eq) )then
        if(associated(obj%theta_eq) )then
            nullify(obj%theta_eq)
        endif
        obj%theta_eq => theta_eq
    endif
    if(present(theta_ps) )then
        if(associated(obj%theta_ps) )then
            nullify(obj%theta_ps)
        endif
        obj%theta_ps => theta_ps
    endif
    if(present(Psi_eq) )then
        if(associated(obj%Psi_eq) )then
            nullify(obj%Psi_eq)
        endif
        obj%Psi_eq => Psi_eq
    endif
    if(present(a_E) )then
        if(associated(obj%a_E) )then
            nullify(obj%a_E)
        endif
        obj%a_E => a_E
    endif
    if(present(a_v) )then
        if(associated(obj%a_v) )then
            nullify(obj%a_v)
        endif
        obj%a_v => a_v
    endif

    if(present(E_eq) )then
        if(associated(obj%E_eq) )then
            nullify(obj%E_eq)
        endif
        obj%E_eq => E_eq
    endif

    if(present(v_eq) )then
        if(associated(obj%v_eq) )then
            nullify(obj%v_eq)
        endif
        obj%v_eq => v_eq
    endif


end subroutine importWaterAbsorption
!#####################################

!#####################################
subroutine runWaterAbsorption(obj,timestep,dt,SolverType,onlyInit,Only1st,Display)
    class(WaterAbsorption_),intent(inout) :: obj
    character(*),intent(in) :: SolverType
    integer(int32) :: i,n
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
        !n=1
        !call showArray(obj%FiniteDeform%DeformVecGloTot,&
        !Name="test"//trim(adjustl(fstring(n))//".txt")  ) 
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
    integer(int32) :: n

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
            n=obj%FiniteDeform%step
            !call showArray(obj%FiniteDeform%DeformVecGloTot,&
            !Name="test"//trim(adjustl(fstring(n))//".txt")  ) 
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

    ! ###### update DIffusion parameters ############
    obj%Permiability = 0.0010d0 
    call obj%DiffusionEq%import(Permiability=obj%Permiability)

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

    ! ###### update DIffusion parameters ############
    call obj%FiniteDeform%import(YoungsModulus=obj%YoungsModulus)
    call obj%FiniteDeform%import(PoissonsRatio=obj%PoissonsRatio)

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




!#####################################
subroutine bakeWaterAbsorption(obj)
    class(WaterAbsorption_),intent(inout) :: obj
    integer(int32) :: i,j,k,l,n,m,NumOfMaterial,layer,in_num,NumOfLayer
    real(real64),allocatable :: matPara(:,:),info(:,:)
    integer(int32),allocatable :: key(:)
    type(Rectangle_) :: rect,mrect
    logical :: in_case
    real(real64) :: matparaval,coord(3),x_max(3),x_min(3)

    !call obj%tissue%bake(template="FiniteDeform_")
    !call obj%water%bake(template="DiffusionEq_") 
    
    n=size(obj%water%mesh%ElemNod,1)
    print *, n
    ! initialize all parameters
    if(allocated(obj%WaterAbsorbingPower) )then
        deallocate(obj%WaterAbsorbingPower)
    endif
    allocate(obj%WaterAbsorbingPower(n))
    if(allocated(obj%WaterPotential) )then
        deallocate(obj%WaterPotential)
    endif
    allocate(obj%WaterPotential(n))
    if(allocated(obj%TurgorPressure) )then
        deallocate(obj%TurgorPressure)
    endif
    allocate(obj%TurgorPressure(n))
    if(allocated(obj%WaterContent) )then
        deallocate(obj%WaterContent)
    endif
    allocate(obj%WaterContent(n))
    if(allocated(obj%Conductivity) )then
        deallocate(obj%Conductivity)
    endif
    allocate(obj%Conductivity(n))
    if(allocated(obj%YoungsModulus) )then
        deallocate(obj%YoungsModulus)
    endif
    allocate(obj%YoungsModulus(n))
    if(allocated(obj%PoissonsRatio) )then
        deallocate(obj%PoissonsRatio)
    endif
    allocate(obj%PoissonsRatio(n))

    ! initialize parameters
    obj%WaterAbsorbingPower(:) = 0.0d0 ! kPa
    obj%WaterPotential(:) = 0.0d0 ! water
    obj%TurgorPressure(:) = 0.0d0 ! changes with theta
    obj%WaterContent(:) = 1.0d0 ! theta
    obj%Conductivity(:) = 1.0d0 ! changes with theta
    obj%YoungsModulus(:) = 1.0d0 ! E
    obj%PoissonsRatio(:) = 0.30d0 ! v
    if(.not. associated(obj%a_Psi) )then
        print *, "a_Psi is not associated."
        stop 
    endif
    if(.not. associated(obj%a_P) )then
        print *, "a_P is not associated."
        stop 
    endif
    if(.not. associated(obj%theta_eq) )then
        print *, "theta_eq is not associated."
        stop 
    endif
    if(.not. associated(obj%theta_ps) )then
        print *, "theta_ps is not associated."
        stop 
    endif
    if(.not. associated(obj%Psi_eq) )then
        print *, "Psi_eq is not associated."
        stop 
    endif
    if(.not. associated(obj%a_E) )then
        print *, "a_E is not associated."
        stop 
    endif
    if(.not. associated(obj%a_v) )then
        print *, "a_v is not associated."
        stop 
    endif
    if(.not. associated(obj%E_eq) )then
        print *, "E_eq is not associated."
        stop 
    endif
    if(.not. associated(obj%v_eq) )then
        print *, "v_eq is not associated."
        stop 
    endif
    

    ! a_Psi
    call obj%a_Psi%getValues(mesh=obj%Tissue%Mesh,Values=obj%a_Psi_val)
    call obj%a_P%getValues(mesh=obj%Tissue%Mesh,Values=obj%a_P_val)
    call obj%theta_eq%getValues(mesh=obj%Tissue%Mesh,Values=obj%theta_eq_val)
    call obj%theta_ps%getValues(mesh=obj%Tissue%Mesh,Values=obj%theta_ps_val)
    call obj%Psi_eq%getValues(mesh=obj%Tissue%Mesh,Values=obj%Psi_eq_val)
    call obj%a_E%getValues(mesh=obj%Tissue%Mesh,Values=obj%a_E_val)
    call obj%a_v%getValues(mesh=obj%Tissue%Mesh,Values=obj%a_v_val)
    call obj%E_eq%getValues(mesh=obj%Tissue%Mesh,Values=obj%E_eq_val)
    call obj%v_eq%getValues(mesh=obj%Tissue%Mesh,Values=obj%v_eq_val)
    
    open(10,file="test.txt")
    do i=1,size(obj%a_Psi_val)
        write(10,*) obj%a_Psi_val(i)
    enddo
    close(10)
    
    !call showarray(mat=obj%WaterAbsorbingPower, Name="test.txt")
     
end subroutine bakeWaterAbsorption
!#####################################
end module 