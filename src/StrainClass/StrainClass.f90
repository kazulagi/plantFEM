module StrainClass
    implicit none

    type ::  Strain_
        ! Finite strain theory
        real(8),allocatable ::   F(:,:)
        real(8),allocatable :: F_n(:,:)
        real(8),allocatable ::   C(:,:)
        real(8),allocatable :: C_n(:,:)
        real(8),allocatable ::   b(:,:)
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
        procedure,public :: import => importStrain
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
        allocate(obj%    b(3,3) )
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
        allocate(obj%    b(3,3) )
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
        allocate(obj%    b(0,0) )
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
        allocate(obj%    b(0,0) )
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
        allocate(obj%    b(0,0) )
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


! ###############################
subroutine importStrain(obj,F,F_n,C,C_n,b,Cp,Cp_n,d,de,dp,l,w,eps,eps_n)
    class(Strain_),intent(inout) :: obj
    real(8),optional,intent(in) :: F(:,:),F_n(:,:),C(:,:),C_n(:,:),b(:,:)
    real(8),optional,intent(in) :: Cp(:,:),Cp_n(:,:),d(:,:),de(:,:)
    real(8),optional,intent(in) :: dp(:,:),l(:,:),w(:,:),eps(:,:),eps_n(:,:)

    if(present(F))then
        if(size(obj%F,1)/= size(F,1) .or. size(obj%F,2) /= size(F,2) )then
            print *, "ERROR :: importStrain :: invalid size "
            return
        else
            obj%F(:,:) = F(:,:)
        endif
    endif
    if(present(F_n))then
        if(size(obj%F_n,1)/= size(F_n,1) .or. size(obj%F_n,2) /= size(F_n,2) )then
            print *, "ERROR :: importStrain :: invalid size "
            return
        else
            obj%F_n(:,:) = F_n(:,:)
        endif
    endif
    if(present(C))then
        if(size(obj%C,1)/= size(C,1) .or. size(obj%C,2) /= size(C,2) )then
            print *, "ERROR :: importStrain :: invalid size "
            return
        else
            obj%C(:,:) = C(:,:)
        endif
    endif
    if(present(C_n))then
        if(size(obj%C_n,1)/= size(C_n,1) .or. size(obj%C_n,2) /= size(C_n,2) )then
            print *, "ERROR :: importStrain :: invalid size "
            return
        else
            obj%C_n(:,:) = C_n(:,:)
        endif
    endif
    if(present(b))then
        if(size(obj%b,1)/= size(b,1) .or. size(obj%b,2) /= size(b,2) )then
            print *, "ERROR :: importStrain :: invalid size "
            return
        else
            obj%b(:,:) = b(:,:)
        endif
    endif
    if(present(Cp))then
        if(size(obj%Cp,1)/= size(Cp,1) .or. size(obj%Cp,2) /= size(Cp,2) )then
            print *, "ERROR :: importStrain :: invalid size "
            return
        else
            obj%Cp(:,:) = Cp(:,:)
        endif
    endif
    if(present(Cp_n))then
        if(size(obj%Cp_n,1)/= size(Cp_n,1) .or. size(obj%Cp_n,2) /= size(Cp_n,2) )then
            print *, "ERROR :: importStrain :: invalid size "
            return
        else
            obj%Cp_n(:,:) = Cp_n(:,:)
        endif
    endif
    if(present(d))then
        if(size(obj%d,1)/= size(d,1) .or. size(obj%d,2) /= size(d,2) )then
            print *, "ERROR :: importStrain :: invalid size "
            return
        else
            obj%d(:,:) = d(:,:)
        endif
    endif
    if(present(de))then
        if(size(obj%de,1)/= size(de,1) .or. size(obj%de,2) /= size(de,2) )then
            print *, "ERROR :: importStrain :: invalid size "
            return
        else
            obj%de(:,:) = de(:,:)
        endif
    endif
    if(present(dp))then
        if(size(obj%dp,1)/= size(dp,1) .or. size(obj%dp,2) /= size(dp,2) )then
            print *, "ERROR :: importStrain :: invalid size "
            return
        else
            obj%dp(:,:) = dp(:,:)
        endif
    endif
    if(present(l))then
        if(size(obj%l,1)/= size(l,1) .or. size(obj%l,2) /= size(l,2) )then
            print *, "ERROR :: importStrain :: invalid size "
            return
        else
            obj%l(:,:) = l(:,:)
        endif
    endif
    if(present(w))then
        if(size(obj%w,1)/= size(w,1) .or. size(obj%w,2) /= size(w,2) )then
            print *, "ERROR :: importStrain :: invalid size "
            return
        else
            obj%w(:,:) = w(:,:)
        endif
    endif
    if(present(eps))then
        if(size(obj%eps,1)/= size(eps,1) .or. size(obj%eps,2) /= size(eps,2) )then
            print *, "ERROR :: importStrain :: invalid size "
            return
        else
            obj%eps(:,:) = eps(:,:)
        endif
    endif
    if(present(eps_n))then
        if(size(obj%eps_n,1)/= size(eps_n,1) .or. size(obj%eps_n,2) /= size(eps_n,2) )then
            print *, "ERROR :: importStrain :: invalid size "
            return
        else
            obj%eps_n(:,:) = eps_n(:,:)
        endif
    endif
 
end subroutine
! ###############################

! ###############################
subroutine getStrain(obj,C,b,d,w)
    class(Strain_),intent(inout)::obj
    logical,optional,intent(in) :: C,b,d,w

    if(present(C) )then
        if(C .eqv. .true.)then
            obj%C(:,:) = matmul(transpose(obj%F),obj%F)
        endif
    endif

    if(present(b) )then
        if(b .eqv. .true.)then
            obj%b(:,:) = matmul(obj%F,transpose(obj%F))
        endif
    endif

    if(present(d) )then
        if(d .eqv. .true.)then
            obj%d(:,:) = 0.50d0*(obj%l(:,:) + transpose(obj%l) )
        endif
    endif

    if(present(w) )then
        if(w .eqv. .true.)then
            obj%w(:,:) = 0.50d0*(obj%l(:,:) - transpose(obj%l) )
        endif
    endif


end subroutine
! ###############################


end module