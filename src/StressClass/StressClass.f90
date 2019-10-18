module StressClass
    use StrainClass
    implicit none

    type :: Stress_
        
        ! hyper
        real(8),allocatable :: sigma(:,:)
        real(8),allocatable :: S(:,:)
        real(8),allocatable :: P(:,:)
        
        ! hypo
        real(8),allocatable :: sigma_dot(:,:)
        real(8),allocatable :: sigma_j(:,:)
        real(8),allocatable :: sigma_o(:,:)
        real(8),allocatable :: sigma_t(:,:)
        real(8),allocatable :: sigma_n(:,:)


        integer :: TheoryID
        
        character*40 :: StrainTheory
        
        ! Please input one of following keyphrases

        ! 1 : Finite_Elasticity
        ! 2 : Finite_ElastoPlasticity
        ! 3 : Infinitesimal_Elasticity
        ! 4 : Infinitesimal_ElastoPlasticity
        ! 5 : Small_strain

    contains
        procedure,public :: init => initStress

    end type
contains


! ###############################
subroutine initStress(obj,StrainTheory)
    class(Stress_),intent(inout) :: obj
    character(*),intent(in) :: StrainTheory
    real(8) :: delta(3,3)

    delta(:,:)=0.0d0
    delta(1,1)=1.0d0
    delta(2,2)=1.0d0
    delta(3,3)=1.0d0

    obj%StrainTheory=StrainTheory

    if(    trim(obj%StrainTheory)=="Finite_Elasticity")then
        obj%theoryID=1

        ! hyper
        allocate(obj%sigma(3,3) )
        allocate(obj%S(3,3) )
        allocate(obj%P(3,3) )

        ! hypo
        allocate(obj%sigma_dot(0,0) )
        allocate(obj%sigma_j(0,0) )
        allocate(obj%sigma_o(0,0) )
        allocate(obj%sigma_t(0,0) )
        allocate(obj%sigma_n(0,0) )

        obj%sigma(:,:)  = 0.0d0
        obj%S(:,:)      = 0.0d0
        obj%P(:,:)      = 0.0d0

    elseif(trim(obj%StrainTheory)=="Finite_ElastoPlasticity")then
        obj%theoryID=2

        ! hyper
        allocate(obj%sigma(3,3) )
        allocate(obj%S(3,3) )
        allocate(obj%P(3,3) )

        ! hypo
        allocate(obj%sigma_dot(0,0) )
        allocate(obj%sigma_j(0,0) )
        allocate(obj%sigma_o(0,0) )
        allocate(obj%sigma_t(0,0) )
        allocate(obj%sigma_n(0,0) )


        obj%sigma(:,:)  = 0.0d0
        obj%S(:,:)      = 0.0d0
        obj%P(:,:)      = 0.0d0
        
    elseif(trim(obj%StrainTheory)=="Infinitesimal_Elasticity")then
        obj%theoryID=3

        ! hyper
        allocate(obj%sigma(3,3) )
        allocate(obj%S(0,0) )
        allocate(obj%P(0,0) )

        ! hypo
        allocate(obj%sigma_dot(3,3) )
        allocate(obj%sigma_j(3,3) )
        allocate(obj%sigma_o(3,3) )
        allocate(obj%sigma_t(3,3) )
        allocate(obj%sigma_n(3,3) )


        obj%sigma(:,:)  = 0.0d0
        obj%sigma_dot(:,:) = 0.0d0
        obj%sigma_j(:,:) = 0.0d0
        obj%sigma_o(:,:) = 0.0d0
        obj%sigma_t(:,:) = 0.0d0
        obj%sigma_n(:,:) = 0.0d0
        
    elseif(trim(obj%StrainTheory)=="Infinitesimal_ElastoPlasticity")then
        obj%theoryID=4

        ! hyper
        allocate(obj%sigma(3,3) )
        allocate(obj%S(0,0) )
        allocate(obj%P(0,0) )

        ! hypo
        allocate(obj%sigma_dot(3,3) )
        allocate(obj%sigma_j(3,3) )
        allocate(obj%sigma_o(3,3) )
        allocate(obj%sigma_t(3,3) )
        allocate(obj%sigma_n(3,3) )

        obj%sigma(:,:)  = 0.0d0
        obj%sigma_dot(:,:) = 0.0d0
        obj%sigma_j(:,:) = 0.0d0
        obj%sigma_o(:,:) = 0.0d0
        obj%sigma_t(:,:) = 0.0d0
        obj%sigma_n(:,:) = 0.0d0

    elseif(trim(obj%StrainTheory)=="Small_strain")then
        obj%theoryID=5

        ! hyper
        allocate(obj%sigma(3,3) )
        allocate(obj%S(0,0) )
        allocate(obj%P(0,0) )

        ! hypo
        allocate(obj%sigma_dot(0,0) )
        allocate(obj%sigma_j(0,0) )
        allocate(obj%sigma_o(0,0) )
        allocate(obj%sigma_t(0,0) )
        allocate(obj%sigma_n(0,0) )

        obj%sigma(:,:)  = 0.0d0

    else
        print *, trim(StrainTheory)
        print *, "Please input one of following keyphrases"
        print *, " ---->"
        print *, "Finite_Elasticity"
        print *, "Finite_ElastoPlasticity"
        print *, "Infinitesimal_Elasticity"
        print *, "Infinitesimal_ElastoPlasticity"
        print *, "Small_strain"
        print *, " <----"
    endif


end subroutine
! ###############################

! ###############################
subroutine getStressRate(obj,Type)
    class(Stress_),intent(inout) :: obj
    class(Strain_),intent(inout) :: strain
    character(*),intent(in) :: Type

    if(trim(Type) == "Jaumann" )then
        obj%sigma_j = obj%sigma_dot + matmul(obj%sigma, strain%w) - matmul(strain%w, obj%sigma) 
    elseif(trim(Type) == "Oldroyd" )then
        obj%sigma_o = obj%sigma_dot + matmul(strain%l, obj%sigma) + matmul(obj%sigma, transpose(strain%l) )  
    elseif(trim(Type) == "Truesdell" )then
        obj%sigma_t = obj%sigma_dot - matmul(strain%l, obj%sigma) &
            - matmul(obj%sigma, transpose(strain%l) ) + trace(strain%l)*obj%sigma
    else
        print *, "ERROR :: getStressRate :: invalid stress rate",trim(Type)
        return
    endif

end subroutine
! ###############################


end module 