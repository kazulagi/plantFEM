module StrainClass
    use, intrinsic :: iso_fortran_env
    use MathClass
    use ShapeFunctionClass
    implicit none

    type ::  Strain_
        ! Finite strain theory
        real(real64),allocatable ::   F(:,:)
        real(real64),allocatable :: F_n(:,:)
        real(real64),allocatable ::   C(:,:)
        real(real64),allocatable :: C_n(:,:)
        real(real64),allocatable ::   b(:,:)
        real(real64),allocatable ::  Cp(:,:)
        real(real64),allocatable ::Cp_n(:,:)
        real(real64)             :: detF

        ! Hypo-elasto-plasticity
        real(real64),allocatable :: d(:,:)
        real(real64),allocatable ::de(:,:)
        real(real64),allocatable ::dp(:,:)
        real(real64),allocatable :: l(:,:)
        real(real64),allocatable :: w(:,:)
        
        ! small-strain
        real(real64),allocatable :: eps(:,:)
        real(real64),allocatable :: eps_n(:,:)
        real(real64),allocatable :: eps_p(:,:)
        real(real64),allocatable :: eps_p_n(:,:)
        real(real64),allocatable :: eps_e(:,:)
        real(real64),allocatable :: eps_e_n(:,:)

        integer(int32) :: TheoryID
        
        character*40 :: StrainTheory
        
        ! Please input one of following keyphrases

        ! Finite_Elasticity
        ! Finite_ElastoPlasticity
        ! Infinitesimal_Elasticity
        ! Infinitesimal_ElastoPlasticity
        ! Small_strain
        
    contains    
        procedure,public :: init => InitStrain
        procedure,public :: import => importStrain
        procedure,public :: get => getStrain
        procedure,public :: getAll => getAllStrain
        procedure,public :: delete => deleteStrain
    end type

contains

! ###############################
subroutine InitStrain(obj,StrainTheory)
    class(Strain_),intent(inout) :: obj
    character(*),intent(in) :: StrainTheory
    real(real64) :: delta(3,3)

    delta(:,:)=0.0d0
    delta(1,1)=1.0d0
    delta(2,2)=1.0d0
    delta(3,3)=1.0d0

    if(allocated(obj%F) )then
        ! delete old obj
        call obj%delete()
    endif

    obj%StrainTheory=StrainTheory

    if(    trim(obj%StrainTheory)=="Finite_Elasticity")then
        obj%theoryID=1

        ! Finite strain theory
        allocate(obj%    F(3,3) )
        allocate(obj%  F_n(3,3) )
        allocate(obj%    C(3,3) )
        allocate(obj%    b(3,3) )
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
        allocate(obj%  eps_e(0,0) )
        allocate(obj%  eps_e_n(0,0) )
        allocate(obj%  eps_p(0,0) )
        allocate(obj%  eps_p_n(0,0) )

        ! Initialize
        obj%    F(:,:) = delta(:,:)
        obj%  F_n(:,:) = delta(:,:)
        obj%    C(:,:) = delta(:,:)
        obj%  C_n(:,:) = delta(:,:)
        obj%   Cp(:,:) = delta(:,:)
        obj% Cp_n(:,:) = delta(:,:)

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
        allocate(obj%  eps_e(0,0) )
        allocate(obj%  eps_e_n(0,0) )
        allocate(obj%  eps_p(0,0) )
        allocate(obj%  eps_p_n(0,0) )

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
        allocate(obj%  eps_e(3,3) )
        allocate(obj%  eps_e_n(3,3) )
        allocate(obj%  eps_p(3,3) )
        allocate(obj%  eps_p_n(3,3) )

        ! initialize
        obj%  d(:,:) =delta(:,:)
        obj%  l(:,:) =delta(:,:)
        obj%  w(:,:) =0.0d0
        obj%  eps(:,:) =delta(:,:)
        obj%  eps_n(:,:) =delta(:,:)
        obj%  eps_e(:,:) =delta(:,:)
        obj%  eps_e_n(:,:) =delta(:,:)
        obj%  eps_p(:,:) =0.0d0
        obj%  eps_p_n(:,:) =0.0d0

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
        allocate(obj%  eps_e(3,3) )
        allocate(obj%  eps_e_n(3,3) )
        allocate(obj%  eps_p(3,3) )
        allocate(obj%  eps_p_n(3,3) )

        ! initialize
        obj%  d(:,:) =delta(:,:)
        obj% de(:,:) =delta(:,:)
        obj% dp(:,:) =delta(:,:)
        obj%  l(:,:) =delta(:,:)
        obj%  w(:,:) =0.0d0
        obj%  eps(:,:) =delta(:,:)
        obj%  eps_n(:,:) =delta(:,:)
        obj%  eps_e(:,:) =delta(:,:)
        obj%  eps_e_n(:,:) =delta(:,:)
        obj%  eps_p(:,:) =0.0d0
        obj%  eps_p_n(:,:) =0.0d0

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
        allocate(obj%  eps_e(3,3) )
        allocate(obj%  eps_e_n(3,3) )
        allocate(obj%  eps_p(3,3) )
        allocate(obj%  eps_p_n(3,3) )

        obj%  eps(:,:) =delta(:,:)
        obj%  eps_n(:,:) =delta(:,:)
        obj%  eps_e(:,:) =delta(:,:)
        obj%  eps_e_n(:,:) =delta(:,:)
        obj%  eps_p(:,:) =0.0d0
        obj%  eps_p_n(:,:) =0.0d0

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
subroutine deleteStrain(obj)
    class(Strain_),intent(inout) :: obj

        ! Finite strain theory
    deallocate(obj%   F )
    deallocate(obj% F_n )
    deallocate(obj%   C )
    deallocate(obj% C_n )
    deallocate(obj%   b )
    deallocate(obj%  Cp )
    deallocate(obj%Cp_n )
    obj%detF = 0.0d0

    ! Hypo-elasto-plasticity
    deallocate(obj% d )
    deallocate(obj%de )
    deallocate(obj%dp )
    deallocate(obj% l )
    deallocate(obj% w )
    
    ! small-strain
    deallocate(obj% eps )
    deallocate(obj% eps_n )
    deallocate(obj%  eps_e )
    deallocate(obj%  eps_e_n )
    deallocate(obj%  eps_p )
    deallocate(obj%  eps_p_n )

    obj%TheoryID = 0
    
    obj%StrainTheory = " "
end subroutine
! ###############################

! ###############################
subroutine importStrain(obj,F,F_n,C,C_n,b,Cp,Cp_n,d,de,dp,l,w,eps,eps_n)
    class(Strain_),intent(inout) :: obj
    real(real64),optional,intent(in) :: F(:,:),F_n(:,:),C(:,:),C_n(:,:),b(:,:)
    real(real64),optional,intent(in) :: Cp(:,:),Cp_n(:,:),d(:,:),de(:,:)
    real(real64),optional,intent(in) :: dp(:,:),l(:,:),w(:,:),eps(:,:),eps_n(:,:)

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
subroutine getallStrain(obj,ShapeFunction)
    class(Strain_),intent(inout) :: obj
    class(ShapeFunction_),intent(in)::ShapeFunction
    real(real64),allocatable :: fmat(:,:) ,Jmat_n(:,:),ddudgzi(:,:),ddudx_n(:,:),ddudx(:,:),&
        dudx(:,:),JmatInv_n(:,:),dudgzi(:,:)


    ! for Finite strain theory
    if(size(obj%F,1) ==3 )then
        allocate(fmat(3,3) ,Jmat_n(3,3),ddudgzi(3,3),ddudx_n(3,3),JmatInv_n(3,3))
        fmat(:,:) = 0.0d0
        fmat(1,1) = 1.0d0
        fmat(2,2) = 1.0d0
        fmat(3,3) = 1.0d0
        Jmat_n(:,:) = matmul(ShapeFunction%dNdgzi,ShapeFunction%ElemCoord_n)
        ddudgzi(:,:) = matmul(ShapeFunction%dNdgzi,ShapeFunction%du)
        call inverse_rank_2(Jmat_n,JmatInv_n)
        ddudx_n(:,:) = matmul(ddudgzi, JmatInv_n  )
        fmat(:,:) =fmat(:,:) + ddudx_n(:,:)
        obj%F = matmul(fmat,obj%F_n)
        call obj%get(C=.true.,b=.true.,detF=.true.)
    endif

    ! for infinitesimal strain theory
    if(size(obj%d,1) == 3 )then
        allocate(ddudx(3,3),ddudgzi(3,3) )
        ddudgzi(:,:) = matmul(ShapeFunction%dNdgzi,ShapeFunction%du)
        ddudx(:,:) = matmul(ddudgzi, ShapeFunction%JmatInv  )
        obj%l(:,:) = ddudx(:,:)
        ! from velocity gradient tensor l, spin tensor w and stretch tensor d is computed.
        call obj%get(d=.true.)
        call obj%get(w=.true.)
        if(size(obj%de,1)==3 .and. size(obj%dp,1)==3 )then
            call obj%get(de=.true.)
        endif
    endif

    ! for small strain theory
    if(size(obj%eps,1 )==3 )then
        if( size(obj%d,1) ==3)then
            ! forward Euler
            obj%eps(:,:) =obj%eps_n(:,:)+obj%d(:,:) 
            ! other integral scheme will be implemented.
        else
            ! small strain
            ! Caution :: du here is seen as u
            allocate(dudgzi(3,3),dudx(3,3) )
            dudgzi(:,:) = matmul(ShapeFunction%dNdgzi,ShapeFunction%du)
            dudx(:,:) = matmul(dudgzi, ShapeFunction%JmatInv  )
            ! Here is the small strain tensor.
            obj%eps(:,:) = 0.50d0*(dudx(:,:) + transpose(dudx)) 
        endif
    endif



end subroutine
! ###############################

! ###############################
subroutine getStrain(obj,C,b,d,w,de,detF)
    class(Strain_),intent(inout)::obj
    logical,optional,intent(in) :: C,b,d,w,detF,de

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

    if(present(de) )then
        if(de .eqv. .true.)then
            obj%de(:,:) = obj%d(:,:) - obj%dp(:,:) 
        endif
    endif

    if(present(w) )then
        if(w .eqv. .true.)then
            obj%w(:,:) = 0.50d0*(obj%l(:,:) - transpose(obj%l) )
        endif
    endif

    if(present(detF) )then
        if(detF .eqv. .true.)then
            obj%detF = det_mat(obj%F,size(obj%F,1) )
        endif
    endif

end subroutine
! ###############################


end module