module SimulatorClass
    use, intrinsic :: iso_fortran_env
    !use MPIClass
    use TermClass
    use FEMDomainClass
    use DiffusionEquationClass
    use FiniteDeformationClass
    use MultiPhysicsClass
    use PostProcessingClass
    use ContactMechanicsClass
    use FieldClass
    implicit none

    type :: Simulator_
        type(DiffusionEq_) ,allocatable :: DiffusionEq_Array(:)
        type(FiniteDeform_),allocatable :: FiniteDeform_Array(:)
        type(MultiPhysics_)  ,allocatable :: MultiPhysics_Array(:)
        type(ContactMechanics_),allocatable :: ContactMechanics_Array(:)
    contains
        procedure :: Deploy => DeploySimulator
        procedure :: SetTime => SetSimulatorTime
        procedure :: Run => RunSimulation
        procedure :: Display => DisplaySimulation
    end type

contains

!###################################################
subroutine Simulator(world,OptionalStep,OptionalTime,SolverType)
    class(Field_),target,intent(inout)   :: world
    type(Simulator_),target             :: sim
    !type(MPI_) :: mpidata
    integer(int32),intent(in)  :: OptionalStep
    real(real64),intent(in)  :: OptionalTime
    character(*),optional,intent(in)::SolverType
    integer(int32)                 :: i,Step,ierr
    real(real64)                 :: time

    step=OptionalStep
    time=OptionalTime

    call sim%Deploy(world)
    call sim%SetTime(world,time,step)
    do i=1,step
        call sim%Run(world,i,SolverType=SolverType)
        call sim%Display(world,i)
    enddo

end subroutine
!###################################################




!###################################################
subroutine RunSimulation(sim,field,step,SolverType)
    type(Field_),target,intent(inout)  :: field
    class(Simulator_),intent(inout) :: sim
    integer(int32),intent(in)             :: step
    character(*),Optional,intent(in)::SolverType
    type(Term_)             :: term
    integer(int32) :: i,n,ierr
    
    call InitializeTerm(term)
    n=size(field%FEMDomainArray,1)

    do i=1,n ! update all FEMDomain
        if(trim(field%FEMDomainArray(i)%SolverType)=="DiffusionEq_" )then
            if(step==1)then
                ! ###### Diffusion Part ###################
                call sim%DiffusionEq_Array(i)%Setup()
                call sim%DiffusionEq_Array(i)%Solve(SolverType=SolverType)
                ! ###### Diffusion Part ###################   
            else
                ! ###### Update Diffusion Field over timesteps ###################
                call sim%DiffusionEq_Array(i)%Update()
                call sim%DiffusionEq_Array(i)%Solve(SolverType=SolverType)
                ! ###### Update Diffusion Field over timesteps ###################
            endif


        elseif(trim(field%FEMDomainArray(i)%SolverType)=="FiniteDeform_" )then
            if(step==1)then
                ! ###### Finite deformation part #############################   
                call sim%FiniteDeform_Array(i)%DivideBC()
                call sim%FiniteDeform_Array(i)%Solve(SolverType=SolverType)  
                ! ###### Finite deformation part #############################
            else
                ! ###### Update Finite Deformation over timesteps ###################
                call sim%FiniteDeform_Array(i)%UpdateInitConfig()
                call sim%FiniteDeform_Array(i)%UpdateBC()
                call sim%FiniteDeform_Array(i)%Solve(SolverType=SolverType) 
                ! ###### Update Finite Deformation over timesteps ###################
            endif
        else
            print *, "RunSimulation >> Invalid Solver Name for domain : ",trim(field%FEMDomainArray(i)%SolverType)
            stop
        endif
    enddo

    n=size(field%FEMIfaceArray,1)
    do i=1,n ! update all FEMDomain
        if( trim(field%FEMIfaceArray(i)%SolverType) == "SyncMesh_")then
            ! ###### SyncMesh #############################       
            call sim%MultiPhysics_Array(i)%SyncMesh()
            ! ###### SyncMesh ############################# 
        elseif(trim(field%FEMIfaceArray(i)%SolverType) == "ContactMechanics_")then
            ! ###### Contact Analysis #############################
            call sim%ContactMechanics_Array(i)%Update(WeakCoupling = .true.)
            ! ###### Contact Analysis #############################
        else
            print *, "RunSimulation >> Invalid Solver Name for interface : ",trim(field%FEMIfaceArray(i)%SolverType)
        endif

    enddo
    


end subroutine
!###################################################






!###################################################
subroutine InitializeSimulator(sim,field)
    type(Field_    ),intent(inout) :: field
    type(Simulator_),intent(inout) :: sim

    integer(int32) :: n
    n = size(field%FEMDomainArray,1)
    allocate(sim%DiffusionEq_Array(n) )
    allocate(sim%FiniteDeform_Array(n) )

    n = size(field%FEMIfaceArray,1)
    allocate(sim%MultiPhysics_Array(n))
    allocate(sim%ContactMechanics_Array(n))
    
    
end subroutine
!###################################################



!###################################################
subroutine DeploySimulator(sim,field)
    type(Field_),target,intent(inout)  :: field
    class(Simulator_),intent(inout) :: sim
    integer(int32) :: i,j,n,DomainNum,ierr

    if(.not.allocated(sim%DiffusionEq_Array) ) then
        call InitializeSimulator(sim,field)
    endif

    n=size(field%FEMDomainArray,1)
    DomainNum=n
    do i=1,n
        if(trim(field%FEMDomainArray(i)%SolverType)=="DiffusionEq_" )then
            sim%DiffusionEq_Array(i)%FEMDomain => field%FEMDomainArray(i)
        elseif(trim(field%FEMDomainArray(i)%SolverType)=="FiniteDeform_" )then
            sim%FiniteDeform_Array(i)%FEMDomain => field%FEMDomainArray(i)
        else
            print *, "DeploySolver >> Invalid Solver Name : ",trim(field%FEMDomainArray(i)%SolverType)
            stop
        endif
    enddo

    n=size(field%FEMIfaceArray,1)

    do i=1,n
        if(trim(field%FEMIfaceArray(i) %SolverType)=="SyncMesh_" .or. &
            trim(field%FEMIfaceArray(i)%SolverType)=="syncmesh_" )then
            sim%MultiPhysics_Array(i)%FEMIFace => field%FEMIfaceArray(i)

            do j=1, DomainNum
                if(trim(sim%MultiPhysics_Array(i)%FEMIFace%FileNameDomain1) == &
                    trim(field%FEMDomainArray(j)%FileName)  )then
                    sim%MultiPhysics_Array(i)%FEMDomain1 => field%FEMDomainArray(j)
                    
                endif
                if(trim(sim%MultiPhysics_Array(i)%FEMIFace%FileNameDomain2) == &
                    trim(field%FEMDomainArray(j)%FileName)  )then
                    sim%MultiPhysics_Array(i)%FEMDomain2 => field%FEMDomainArray(j)
                endif
            enddo
        elseif(trim(field%FEMIfaceArray(i) %SolverType)=="ContactMechanics_" .or. &
            trim(field%FEMIfaceArray(i)%SolverType)=="contactmechanics_" )then
            sim%ContactMechanics_Array(i)%FEMIFace => field%FEMIfaceArray(i)
            do j=1, DomainNum
                if(trim(sim%ContactMechanics_Array(i)%FEMIFace%FileNameDomain1) == &
                    trim(field%FEMDomainArray(j)%FileName)  )then
                    sim%ContactMechanics_Array(i)%FEMDomain1 => field%FEMDomainArray(j)
                    
                endif
                if(trim(sim%ContactMechanics_Array(i)%FEMIFace%FileNameDomain2) == &
                    trim(field%FEMDomainArray(j)%FileName)  )then
                    
                    sim%ContactMechanics_Array(i)%FEMDomain2 => field%FEMDomainArray(j)
                endif
            enddo

        
        else
            print *, "DeploySolver >> Invalid Solver Name : ",trim(field%FEMIfaceArray(i) %SolverType)
            stop "debug"
        endif
    enddo
end subroutine
!###################################################






!###################################################
subroutine DisplaySimulation(sim,field,step)

    type(Field_),target,intent(inout)  :: field
    class(Simulator_),intent(inout) :: sim
    integer(int32),intent(in)             :: step
    type(Term_)             :: term
    integer(int32) :: i,n,ierr

    call InitializeTerm(term)
    n=size(field%FEMDomainArray,1)
    do i=1,n
        if(trim(field%FEMDomainArray(i)%SolverType)=="DiffusionEq_" )then
            ! ########## Display Results #####################
            call DisplayDiffusionEq(sim%DiffusionEq_Array(i),DisplayMode=term%gmsh,OptionalStep=step)
            ! ########## Display Results #####################

        elseif(trim(field%FEMDomainArray(i)%SolverType)=="FiniteDeform_" )then
            ! ########## Display Results #####################
            call DisplayDeformStress(sim%FiniteDeform_Array(i),Name=field%FieldList(2)%FieldObjName,&
                DisplayMode=term%gmsh,OptionalStep=step)   
            call DisplayReactionForce(sim%FiniteDeform_Array(i))
            ! ########## Display Results #####################
        else
            print *, "DeploySolver >> Invalid Solver Name : ",trim(field%FEMDomainArray(i)%SolverType)
            stop
        endif
    enddo


end subroutine
!###################################################






!###################################################
subroutine SetSimulatorTime(sim,field,time,step)

    type(Field_),target,intent(inout)  :: field
    class(Simulator_),intent(inout) :: sim
    real(real64),intent(in)  :: time
    integer(int32),intent(in) :: step
    integer(int32) :: i,n,ierr
    
    n=size(field%FEMDomainArray,1)
    do i=1,n
        if(trim(field%FEMDomainArray(i)%SolverType)=="DiffusionEq_" )then
            ! ########## Display Results #####################
            field%FEMDomainArray(i)%ControlPara%Timestep=step
            sim%DiffusionEq_Array(i)%dt=time/dble(field%FEMDomainArray(i)%ControlPara%Timestep)
            ! ########## Display Results #####################

        elseif(trim(field%FEMDomainArray(i)%SolverType)=="FiniteDeform_" )then
            field%FEMDomainArray(i)%ControlPara%Timestep=step
            sim%FiniteDeform_Array(i)%dt=time/dble(field%FEMDomainArray(i)%ControlPara%Timestep)
        else
            print *, "DeploySolver >> Invalid Solver Name : ",trim(field%FEMDomainArray(i)%SolverType)
            stop
        endif
    enddo

end subroutine 
!###################################################






end module