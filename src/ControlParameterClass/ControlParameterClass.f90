module ControlParaeterClass
    implicit none
    type::ControlParameter_
        real(8) :: Tol
        integer :: SimMode
        integer :: ItrTol
        integer :: Timestep
    contains
        procedure,public :: SetControlPara => SetControlPara
    end type ControlParameter_

contains

!######### Import Control Parameters #########
subroutine SetControlPara(obj,OptionalTol,OptionalItrTol,OptionalTimestep,OptionalSimMode)
    class(ControlParameter_),intent(out)::obj
    real(8),optional,intent(in)::OptionalTol
    integer,optional,intent(in)::OptionalSimMode,OptionalItrTol,OptionalTimestep

    if(present(OptionalSimMode) )then
        obj%SimMode = OptionalSimMode
    else
        obj%SimMode = 1
    endif

    if(present(OptionalTol) )then
        obj%Tol = OptionalTol
    else
        obj%Tol = 1.0e-15
    endif
    
    if(present(OptionalItrTol) )then
        obj%ItrTol = OptionalItrTol
    else
        obj%ItrTol = 100
    endif

    if(present(OptionalTimestep) )then
        obj%Timestep = OptionalTimestep
    else
        obj%Timestep = 100
    endif
    
end subroutine SetControlPara
!######### Import Control Parameters #########


end module ControlParaeterClass