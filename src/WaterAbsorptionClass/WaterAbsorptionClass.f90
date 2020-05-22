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
        real(real64),allocatable :: PorePressure(:)
        ! Parameters(A,B)
        ! A : Element ID 

        real(real64),allocatable :: a_Psi_val(:)
        real(real64),allocatable :: a_P_val(:)
        real(real64),allocatable :: theta_eq_val(:)
        real(real64),allocatable :: Psi_eq_val(:)
        real(real64),allocatable :: a_E_val(:)
        real(real64),allocatable :: a_v_val(:)
        real(real64),allocatable :: E_eq_val(:)
        real(real64),allocatable :: v_eq_val(:)

        real(real64),allocatable :: theta_ps_val(:) ! computed from other variables

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
        procedure, public :: gnuplot => gnuplotWaterAbsorption
        procedure, public :: updatePermiability => updatePermiabilityWA
        procedure, public :: updateStiffness => updateStiffnessWA
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
subroutine runWaterAbsorption(obj,timestep,dt,SolverType,onlyInit,Only1st,Display,nr_tol)
    class(WaterAbsorption_),intent(inout) :: obj
    character(*),intent(in) :: SolverType
    integer(int32) :: i,n
    logical,optional,intent(in) :: onlyInit,Only1st,Display
    real(real64) ,optional,intent(in) :: nr_tol
    integer(int32),intent(in) :: timestep
    real(real64),optional,intent(in) :: dt

        
    obj%dt=input(default=1.0d0,option=dt)
    obj%timestep=timestep
    
    obj%DiffusionEq%dt  = obj%dt
    obj%FiniteDeform%dt = obj%dt

    print *, "[ImportFile]>>>[Initialize]>"
    
    call obj%init(SolverType,Display=Display,nr_tol=nr_tol)
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
subroutine initWaterAbsorption(obj,SolverType,Display,nr_tol)
    class(WaterAbsorption_),intent(inout) :: obj
    type(Term_)             :: term
    character(*),intent(in) :: SolverType
    logical,optional,intent(in) :: Display
    real(real64) ,optional,intent(in) :: nr_tol
    integer(int32) :: n

    call term%init()

    call obj%updatePermiability()
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
    
    ! debug
    ! only diffusion


    ! ###### Finite deformation part #############################   
    call obj%updateStiffness()
    call obj%FiniteDeform%DivideBC()
    call obj%FiniteDeform%Solve(SolverType=SolverType,nr_tol=nr_tol)  
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
    integer(int32) :: i
    call term%init()

    ! ###### update DIffusion parameters ############
     
    call obj%updatePermiability()
    call obj%DiffusionEq%import(Permiability=obj%Permiability)
    do i=1,size(obj%water%mesh%nodCoord,1)
        write(1234,*) obj%water%mesh%nodCoord(i,1),obj%DiffusionEq%dt*dble(step), obj%DiffusionEq%UnknownVec(i)
    enddo
    write(1234,*) "  "
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

    ! only diffusion debug
    
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


subroutine updatePermiabilityWA(obj)
    class(WaterAbsorption_),intent(inout) :: obj
    integer(int32) :: i,j,n,m
    real(real64) :: k_0, a_hat, val(2),x
    n=size(obj%a_Psi_val)

    if(.not. allocated(obj%Permiability) )then
        allocate(obj%Permiability(n) )
    endif
    if(.not. allocated(obj%WaterContent) )then
        allocate(obj%WaterContent(n) )
        obj%WaterContent(n) = 0.0d0
    endif
    if(allocated(obj%DiffusionEq%UnknownValue) )then
        if(size(obj%DiffusionEq%UnknownValue,1)==n)then
            m=size(obj%DiffusionEq%UnknownValue,2)
            do i=1,n
                obj%WaterContent(i) = 0.0d0
                do j=1,m
                    ! get avarage value of wate content theta
                    obj%WaterContent(i) =obj%WaterContent(i)+ obj%DiffusionEq%UnknownValue(i,j)/dble(m)
                enddo
            enddo
        endif
    endif
    ! determine permiability for each element
    do i=1,n
        k_0=obj%Water%MaterialProp%matPara(obj%Water%Mesh%ElemMat(i),1 )
        val(1)=obj%a_Psi_val(i)
        val(2)=obj%a_Psi_val(i) - abs(obj%a_P_val(i))
        ! Hebiside step function
        !if(obj%WaterContent(i) <=obj%theta_ps_val(i) )then
        !    a_hat = val(1)
        !else
        !    a_hat=val(2)
        !endif
        ! use normalization
        x = 2.0d0 * (obj%WaterContent(i) - 0.50d0 )*100.0d0
        a_hat = obj%a_Psi_val(i) - 1.0d0 / (1.0d0 + exp(- x ) ) * abs(obj%a_P_val(i))
        write(220,*) obj%WaterContent(i), a_hat*k_0
        obj%Permiability(i) = k_0 * a_hat
    enddo

    write(100,*) obj%Permiability(:)
end subroutine updatePermiabilityWA
!#####################################


!#####################################
subroutine updateStiffnessWA(obj)
    class(WaterAbsorption_),intent(inout) :: obj
    integer(int32) :: i,j,n
    real(real64) :: a_E, E, E_eq, theta,theta_eq,a_P,theta_ps
    real(real64) :: a_v, v, v_eq,p,val(2)
    n=size(obj%tissue%mesh%elemNod,1)
    if(.not. allocated(obj%YoungsModulus) )then
        allocate(obj%YoungsModulus(n) )
    endif

    if(.not. allocated(obj%PoissonsRatio) )then
        allocate(obj%PoissonsRatio(n) )
    endif

    if(.not. allocated(obj%PorePressure) )then
        allocate(obj%PorePressure(n) )
    endif

    ! update these parameters considering volumetric water content
    do i=1,n
        ! import data for each element
        a_E = obj%a_E_val(i)
        a_v = obj%a_v_val(i)
        E_eq = obj%E_eq_val(i)
        v_eq = obj%v_eq_val(i)
        theta = obj%WaterContent(i)
        theta_eq = obj%theta_eq_val(i)
        theta_ps = obj%theta_ps_val(i)
        a_P = obj%a_P_val(i)

        ! compute parameters
        E = a_E * (theta - theta_eq) + E_eq
        v = a_v * (theta - theta_eq) + v_eq
        val(1)=0.0d0
        val(2)=a_P*(theta - theta_ps)
        p = maxval(val)

        ! export parameters
        obj%YoungsModulus(i)= E
        obj%PoissonsRatio(i)= v
        obj%PorePressure(i) = p
    enddo

    call obj%FiniteDeform%import( obj%YoungsModulus, obj%PoissonsRatio, obj%PorePressure )

end subroutine updateStiffnessWA
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
    call obj%Psi_eq%getValues(mesh=obj%Tissue%Mesh,Values=obj%Psi_eq_val)
    call obj%a_E%getValues(mesh=obj%Tissue%Mesh,Values=obj%a_E_val)
    call obj%a_v%getValues(mesh=obj%Tissue%Mesh,Values=obj%a_v_val)
    call obj%E_eq%getValues(mesh=obj%Tissue%Mesh,Values=obj%E_eq_val)
    call obj%v_eq%getValues(mesh=obj%Tissue%Mesh,Values=obj%v_eq_val)

    ! a_Psi should be negative values
    n=size(obj%theta_eq_val)
    do i=1,n
        obj%a_Psi_val(i) = - abs(obj%a_Psi_val(i)) 
    enddo


    ! a_Psi should be negative values
    n=size(obj%theta_eq_val)
    do i=1,n
        obj%a_E_val(i) = - abs(obj%a_E_val(i)) 
    enddo


    ! a_Psi should be negative values
    n=size(obj%theta_eq_val)
    do i=1,n
        obj%a_v_val(i) = abs(obj%a_v_val(i)) 
    enddo

    n=size(obj%theta_eq_val)
    allocate(obj%theta_ps_val(n) )
    do i=1,n
        obj%theta_ps_val(i)  =  (-  obj%Psi_eq_val(i) + obj%a_P_val(i) *  obj%theta_eq_val(i) )/obj%a_P_val(i) 
    enddo

    
    !call showarray(mat=obj%WaterAbsorbingPower, Name="test.txt")
     
end subroutine bakeWaterAbsorption
!#####################################

!#####################################
subroutine gnuplotWaterAbsorption(obj,mode,ElemID)
    class(WaterAbsorption_),intent(in) :: obj
    character(*),intent(in) :: mode
    integer(int32),optional,intent(in) :: ElemID
    integer(int32) :: i,j,n,m
    real(real64) :: theta,psi(2)

    n=input(default=1,option=ElemID)
    if(mode=="OWCC" .or. mode=="all")then
        open(20,file="OWCC.txt")
        do i=0,100
            theta=dble(i)*obj%theta_eq_val(n)/100.0d0
            write(20,*) theta, (theta-obj%theta_eq_val(n))*obj%a_Psi_val(n) +obj%Psi_eq_val(n)
        enddo
        write(20,*) " "
        do i=0,100
            theta=dble(i)*obj%theta_eq_val(n)/100.0d0
            psi(1)=0.0d0
            psi(2)=obj%a_P_val(n)*(theta-obj%theta_ps_val(n))
            write(20,*) theta, maxval(psi)
        enddo
        write(20,*) " "
        close(20)
    endif

    if(mode=="YoungsModulus" .or. mode=="all")then
        open(20,file="YoungsModulus.txt")
        do i=0,100
            theta=dble(i)*obj%theta_eq_val(n)/100.0d0
            write(20,*) theta, (theta-obj%theta_eq_val(n))*obj%a_E_val(n) +obj%E_eq_val(n)
        enddo
        close(20)
    endif

    if(mode=="PoissonsRatio" .or. mode=="all")then
        open(20,file="PoissonsRatio.txt")
        do i=0,100
            theta=dble(i)*obj%theta_eq_val(n)/100.0d0
            write(20,*) theta, (theta-obj%theta_eq_val(n))*obj%a_v_val(n) +obj%v_eq_val(n)
        enddo
        close(20)
    endif
end subroutine gnuplotWaterAbsorption
!#####################################
end module 