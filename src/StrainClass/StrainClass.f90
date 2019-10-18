module StrainClass
    implicit none

    type ::  Strain_
        ! Finite strain theory
        real(8),allocatable ::   F(:,:)
        real(8),allocatable :: F_n(:,:)
        real(8),allocatable ::   C(:,:)
        real(8),allocatable :: C_n(:,:)
        real(8),allocatable ::  Cp(:,:)
        real(8),allocatable ::Cp_n(:,:)

        ! Hypo-elasto-plasticity
        real(8),allocatable :: d(:,:)
        real(8),allocatable ::de(:,:)
        real(8),allocatable ::dp(:,:)
        real(8),allocatable :: l(:,:)
        real(8),allocatable :: w(:,:)
        
        ! small-strain
        real(8),allocatable :: eps(:,:)
        real(8),allocatable :: eps_n(:,:)

        integer :: TheoryID
        
        character*40 :: StrainTheory
    contains    
        procedure,public :: init => InitStrain
    end type

contains

! ###############################
subroutine InitStrain(obj,StrainTheory)
    class(Strain_),intent(inout) :: obj
    character(*),intent(in) :: StrainTheory
    real(8) :: delta(3,3)

    delta(:,:)=0.0d0
    delta(1,1)=1.0d0
    delta(2,2)=1.0d0
    delta(3,3)=1.0d0

    obj%StrainTheory=StrainTheory

    if(    trim(obj%StrainTheory)=="Finite_Elasticity")then
        obj%theoryID=1

        ! Finite strain theory
        allocate(obj%    F(3,3) )
        allocate(obj%  F_n(3,3) )
        allocate(obj%    C(3,3) )
        allocate(obj%  C_n(3,3) )
        allocate(obj%   Cp(0,0) )
        allocate(obj% Cp_n(0,0) )

        ! Hypo-elasto-plasticity
        allocate(obj%  d(0,0) )
        allocate(obj% de(0,0) )
        allocate(obj% dp(0,0) )
        allocate(obj%  l(0,0) )
        allocate(obj%  w(0,0) )
        
        ! small-strain
        allocate(obj%  eps(0,0) )
        allocate(obj%  eps_n(0,0) )

        ! Initialize
        obj%    F(:,:) = delta(:,:)
        obj%  F_n(:,:) = delta(:,:)
        obj%    C(:,:) = delta(:,:)
        obj%  C_n(:,:) = delta(:,:)

    elseif(trim(obj%StrainTheory)=="Finite_ElastoPlasticity")then
        obj%theoryID=2

        ! Finite strain theory
        allocate(obj%    F(3,3) )
        allocate(obj%  F_n(3,3) )
        allocate(obj%    C(3,3) )
        allocate(obj%  C_n(3,3) )
        allocate(obj%   Cp(3,3) )
        allocate(obj% Cp_n(3,3) )

        ! Hypo-elasto-plasticity
        allocate(obj%  d(0,0) )
        allocate(obj% de(0,0) )
        allocate(obj% dp(0,0) )
        allocate(obj%  l(0,0) )
        allocate(obj%  w(0,0) )
        
        ! small-strain
        allocate(obj%  eps(0,0) )
        allocate(obj%  eps_n(0,0) )

        obj%    F(:,:) = delta(:,:)
        obj%  F_n(:,:) = delta(:,:)
        obj%    C(:,:) = delta(:,:)
        obj%  C_n(:,:) = delta(:,:)
        obj%   Cp(:,:) = delta(:,:)
        obj% Cp_n(:,:) = delta(:,:)


    elseif(trim(obj%StrainTheory)=="Infinitesimal_Elasticity")then
        obj%theoryID=3


        ! Finite strain theory
        allocate(obj%    F(0,0) )
        allocate(obj%  F_n(0,0) )
        allocate(obj%    C(0,0) )
        allocate(obj%  C_n(0,0) )
        allocate(obj%   Cp(0,0) )
        allocate(obj% Cp_n(0,0) )

        ! Hypo-elasto-plasticity
        allocate(obj%  d(3,3) )
        allocate(obj% de(0,0) )
        allocate(obj% dp(0,0) )
        allocate(obj%  l(3,3) )
        allocate(obj%  w(3,3) )
        
        ! small-strain
        allocate(obj%  eps(3,3) )
        allocate(obj%  eps_n(3,3) )

        ! initialize
        obj%  d(:,:) =delta(:,:)
        obj%  l(:,:) =delta(:,:)
        obj%  w(:,:) =0.0d0
        obj%  eps(:,:) =delta(:,:)
        obj%  eps_n(:,:) =delta(:,:)

    elseif(trim(obj%StrainTheory)=="Infinitesimal_ElastoPlasticity")then
        obj%theoryID=4


        ! Finite strain theory
        allocate(obj%    F(0,0) )
        allocate(obj%  F_n(0,0) )
        allocate(obj%    C(0,0) )
        allocate(obj%  C_n(0,0) )
        allocate(obj%   Cp(0,0) )
        allocate(obj% Cp_n(0,0) )

        ! Hypo-elasto-plasticity
        allocate(obj%  d(3,3) )
        allocate(obj% de(3,3) )
        allocate(obj% dp(3,3) )
        allocate(obj%  l(3,3) )
        allocate(obj%  w(3,3) )
        
        ! small-strain
        allocate(obj%  eps(3,3) )
        allocate(obj%  eps_n(3,3) )

        ! initialize
        obj%  d(:,:) =delta(:,:)
        obj% de(:,:) =delta(:,:)
        obj% dp(:,:) =delta(:,:)
        obj%  l(:,:) =delta(:,:)
        obj%  w(:,:) =0.0d0
        obj%  eps(:,:) =delta(:,:)
        obj%  eps_n(:,:) =delta(:,:)

    elseif(trim(obj%StrainTheory)=="Small_strain")then
        obj%theoryID=5


        ! Finite strain theory
        allocate(obj%    F(0,0) )
        allocate(obj%  F_n(0,0) )
        allocate(obj%    C(0,0) )
        allocate(obj%  C_n(0,0) )
        allocate(obj%   Cp(0,0) )
        allocate(obj% Cp_n(0,0) )

        ! Hypo-elasto-plasticity
        allocate(obj%  d(0,0) )
        allocate(obj% de(0,0) )
        allocate(obj% dp(0,0) )
        allocate(obj%  l(0,0) )
        allocate(obj%  w(0,0) )
        
        ! small-strain
        allocate(obj%  eps(3,3) )
        allocate(obj%  eps_n(3,3) )

        obj%  eps(:,:) =delta(:,:)
        obj%  eps_n(:,:) =delta(:,:)

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


end module