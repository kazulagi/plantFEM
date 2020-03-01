module ControlParaeterClass
    use, intrinsic :: iso_fortran_env    
    implicit none
    type::ControlParameter_
        real(real64) :: Tol
        integer(int32) :: SimMode
        integer(int32) :: ItrTol
        integer(int32) :: Timestep
    contains
        procedure,public :: init => initControlPara
        procedure,public :: SetControlPara => SetControlPara
    end type ControlParameter_

contains

subroutine initControlPara(obj)
    class(ControlParameter_),intent(inout):: obj

    obj%Tol=1.0e-16
    obj%SimMode=-1
    obj%ItrTol=-1
    obj%Timestep=-1
end subroutine

!######### Import Control Parameters #########
subroutine SetControlPara(obj,OptionalTol,OptionalItrTol,OptionalTimestep,OptionalSimMode)
    class(ControlParameter_),intent(out)::obj
    real(real64),optional,intent(in)::OptionalTol
    integer(int32),optional,intent(in)::OptionalSimMode,OptionalItrTol,OptionalTimestep

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