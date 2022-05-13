module SeepageFlowClass
    use FEMDomainClass
    use FEMSolverClass
    implicit none

    type :: SeepageFlow_
        type(FEMDomain_),pointer :: FEMDomain => null()
        type(FEMSolver_) :: solver
        character(:),allocatable :: model
    contains
        procedure,public :: init => initSeepageFlow
        procedure,public :: fixPressureBoundary => fixPressureBoundarySeepageFlow
        procedure,public :: getPressure => getPressureSeepageFlow
        procedure,public :: getVelocity  => getVelocitySeepageFlow
    end type

contains

subroutine initSeepageFlow(this,femdomain,model,Permiability)
    class(SeepageFlow_),intent(inout) :: this
    type(FEMDomain_),target,intent(in) :: femdomain
    real(real64),intent(in) :: Permiability(:)
    character(*),intent(in) :: model

    integer(int32) :: ElementID
    
    if(associated(this%femdomain) )then
        this%femdomain => null()        
    endif

    this%femdomain => femdomain
    this%model = model

    select case(this%model)
        case("Darcy", "darcy")
            ! activate following element-wise values
            call this%solver%init(NumDomain=1)
            call this%solver%setDomain(FEMDomain=femdomain,domainID=1)
            call this%solver%setCRS(DOF=1)
            

            !$OMP parallel 
            !$OMP do
            do ElementID = 1, this%femdomain%ne()
                call this%solver%setMatrix(DomainID=1,ElementID=ElementID,DOF=1,&
                   Matrix=this%femdomain%DiffusionMatrix(ElementID=ElementID,&
                    D=Permiability(ElementID) ) )
                !call this%solver%setVector(DomainID=1,ElementID=ElementID,DOF=1,&
                !    Vector=this%femdomain%FlowVector(&
                !        ElementID=ElementID,&
                !        DOF=this%femdomain%nd() ,&
                !        Density=1.700d0,&
                !        Accel=[0.0d0, 0.0d0, -9.80d0]&
                !        ) & 
                !)
            enddo
            !$OMP end do
            !$OMP end parallel
    
    
        case default
            print *, "initSeepageFlow >> unknown model name ",model
    end select

end subroutine

! ##########################################################################
subroutine fixPressureBoundarySeepageFlow(this,NodeList,Pressure,DomainID)
    class(SeepageFlow_),intent(inout) :: This
    integeR(int32),intent(in) :: NodeList(:)
    integeR(int32),optional,intent(in) :: DomainID
    real(real64),intent(in) :: Pressure
    integeR(int32) :: DomainID_

    DomainID_ = input(default=1, option=DomainID)


    call this%solver%fix(DomainID=DomainID_,IDs=NodeList,FixValue=Pressure)
    
end subroutine
! ##########################################################################


function getPressureSeepageFlow(this,debug) result(Pressure)
    class(SeepageFlow_),intent(inout) :: This
    logical,optional,intent(in) :: debug
    real(real64),allocatable :: Pressure(:)

    if(present(debug) )then
        this%solver%debug = debug
    endif
    Pressure =  this%solver%solve()


end function

! ##########################################################################
function getVelocitySeepageFlow(this,Pressure,Permiability) result(Velocity)
    class(SeepageFlow_),intent(inout) :: This
    real(real64),intent(in) :: Permiability,Pressure(:)
    real(real64),allocatable :: Velocity(:,:)
    integer(int32) :: ElementID


    Velocity = zeros(this%femdomain%ne(),this%femdomain%nd() )
    do ElementID = 1, this%femdomain%ne()
        Velocity(ElementID,:) = this%femdomain%FlowVector(&
            ElementID=ElementID,Pressure=Pressure,Permiability=Permiability)
    enddo
    

end function

! ##########################################################################



end module

