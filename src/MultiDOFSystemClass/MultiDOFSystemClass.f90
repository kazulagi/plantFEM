module multiDOFsystemClass
    use fem
    use SeismicAnalysisClass
    implicit none

    type :: multiDOFsystem_
        type(FEMDomain_),pointer :: femdomain
        type(LinearSolver_) :: solver
        real(real64),allocatable :: m(:)
        real(real64),allocatable :: c(:)
        real(real64),allocatable :: k(:)
        real(real64),allocatable :: f(:)
        real(real64),allocatable :: u(:)
        real(real64),allocatable :: v(:)
        real(real64),allocatable :: a(:)
        real(real64) :: Newmark_beta = 0.25d0
        real(real64) :: Newmark_gamma= 0.50d0
        integer(int32) :: itr=0
    contains
        procedure :: init => initmultiDOFsystem
        procedure :: solve => solvemultiDOFsystem
    end type
contains

! #####################################################################
subroutine initmultiDOFsystem(obj,femdomain) 
    class(multiDOFsystem_),intent(inout) :: obj
    type(FEMDomain_),target :: femdomain
    if(associated(obj%femdomain) ) then
        nullify(obj%femdomain)
    endif

    ! check type.
    ! dimension should be 1
    if(femdomain%mesh%empty() )then
        call print("ERROR :: importDomainmultiDOFsystem >> mesh is empty!")
        stop
    endif
    
    if(femdomain%nd() /=1 )then
        call print("ERROR :: importDomainmultiDOFsystem >> dimension should be 1")
        stop
    endif

    obj%femdomain => femdomain

    obj%m = zeros(femdomain%nn() )
    obj%c = zeros(femdomain%nn() )
    obj%k = zeros(femdomain%nn() )
    obj%f = zeros(femdomain%nn() )
    obj%u = zeros(femdomain%nn() )
    obj%v = zeros(femdomain%nn() )
    obj%a = zeros(femdomain%nn() )
    obj%Newmark_beta = 0.25d0
    obj%Newmark_gamma = 0.500d0

end subroutine
! #####################################################################



! #####################################################################
subroutine solvemultiDOFsystem(obj,dt,FixNodeId,displacement,Solver)
    class(multiDOFsystem_),intent(inout) :: obj
    type(SeismicAnalysis_) :: seismic
    real(real64),intent(in) :: dt
    real(real64),intent(in) :: displacement
    integer(int32),intent(in) :: FixNodeId
    character(*),intent(in) :: Solver

    real(real64),allocatable :: Amat(:,:),bvec(:),u_upd(:),v_upd(:),a_upd(:)
    real(real64),allocatable :: Mmat(:,:)
    real(real64),allocatable :: Cmat(:,:)
    real(real64),allocatable :: Kmat(:,:)



    integer(int32) :: i,j,n
    n = obj%femdomain%nn()

    ! solve 1-step
    Mmat = zeros(n,n)
    Cmat = zeros(n,n)
    Kmat = zeros(n,n)

    ! (1) create matrices and vectors
    i = 1
    Mmat(i,i  ) = obj%m(i)
    
    Kmat(i,i  ) = obj%k(i) + obj%k(i+1)
    Kmat(i,i+1) = - obj%k(i+1)

    Cmat(i,i  ) = obj%c(i) + obj%c(i+1)
    Cmat(i,i+1) = - obj%c(i+1)

    do i=2,n-1
        Mmat(i,i  ) = obj%m(i)
        
        Kmat(i,i-1) = - obj%k(i)
        Kmat(i,i  ) = obj%k(i) + obj%k(i+1)
        Kmat(i,i+1) = - obj%k(i+1)

        Cmat(i,i-1) = - obj%c(i)
        Cmat(i,i  ) = obj%c(i) + obj%c(i+1)
        Cmat(i,i+1) = - obj%c(i+1)
    enddo
    i = n
    Mmat(i,i  ) = obj%m(i)
    
    Kmat(i,i-1) = - obj%k(i)
    Kmat(i,i  ) = obj%k(i) 

    Cmat(i,i-1) = - obj%c(i)
    Cmat(i,i  ) = obj%c(i) 


    ! (2) set matrices and vectors
    
    obj%solver%A = seismic%getNewmarkBetaMatrix(&
        M=Mmat, &
        C=Cmat, &
        K=Kmat, &
        beta=obj%newmark_beta,&
        gamma=obj%newmark_gamma,&
        dt=dt)
    
    
    obj%solver%b = seismic%getNewmarkBetaVector(M=Mmat,&
        C=Cmat,       &
        u_n=obj%u,    &
        v_n=obj%v,    &
        a_n=obj%a,    &
        force=obj%f,  &
        beta=obj%newmark_beta,&
        gamma=obj%newmark_gamma,&
        dt=dt)
    obj%solver%x = zeros(n)

    
    call obj%solver%fix(nodeid=FixNodeId,entryvalue=displacement)
    
    !call print(obj%solver%a)

    call obj%solver%solve(Solver)

    u_upd = obj%solver%x
    
    v_upd = seismic%updateVelocityNewmarkBeta(u=u_upd,u_n=obj%u,v_n=obj%v,a_n=obj%a,&
        gamma=obj%newmark_gamma,beta=obj%newmark_beta,dt=dt)
    a_upd = seismic%updateAccelNewmarkBeta(u=u_upd,u_n=obj%u,v_n=obj%v,a_n=obj%a,&
        gamma=obj%newmark_gamma,beta=obj%newmark_beta,dt=dt)

        !if(obj%itr==10)then
        !    stop
        !endif


!    if(obj%itr==3470)then
!        print *, "u"
!        print *, u_upd
!        print *, "v"
!        print *, v_upd
!        print *, "a"
!        print *, a_upd
!        stop
!    endif

    obj%u = u_upd
    obj%v = v_upd
    obj%a = a_upd

    
end subroutine
! #####################################################################


end module