module PoromechanicsClass
    use fem
    implicit none

    ! This scheme is based on ZIenkiewicz and Pande, 1982

    type:: Poromechanics_
        type(FEMDomain_),pointer :: FEMDomain
    contains
        procedure, public :: run => runPoromechanics
    end type

contains

! ################################################
subroutine runPoromechanics(obj)
    class(Poromechanics_),intent(inout) :: obj


end subroutine
! ################################################


end module PoromechanicsClass