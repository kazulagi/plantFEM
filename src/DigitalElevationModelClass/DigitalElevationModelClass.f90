module DigitalElevationModelClass
    use std
    implicit none

    type :: DigitalElevationModel_
        real(real64),allocatable :: x(:)
        real(real64),allocatable :: y(:)
        real(real64),allocatable :: z(:)
    contains
        procedure, public :: set => setDigitalElevationModel
        procedure, public :: NumberOfPoint => NumberOfPointDigitalElevationModel
        procedure, public :: nn => NumberOfPointDigitalElevationModel
        !procedure, public :: read => readDigitalElevationModel
    end type
contains

! ##################################################
subroutine setDigitalElevationModel(obj,x,y,z)
    class(DigitalElevationModel_),intent(inout) :: obj
    real(real64),intent(in) :: x(:)
    real(real64),intent(in) :: y(:)
    real(real64),intent(in) :: z(:)

    if(size(x)/=size(y) )then
        print *, "ERROR :: readDigitalElevationModel >> size(x) /= size(y)"
        stop
    endif

    if(size(y)/=size(z) )then
        print *, "ERROR :: readDigitalElevationModel >> size(y) /= size(z)"
        stop
    endif

    if(size(x)/=size(z) )then
        print *, "ERROR :: readDigitalElevationModel >> size(x) /= size(z)"
        stop
    endif

    obj%x = x
    obj%y = y
    obj%z = z

end subroutine
! ##################################################

! ##################################################
function NumberOfPointDigitalElevationModel(obj) result(ret)
    class(DigitalElevationModel_),intent(in) :: obj
    integer(int32) :: ret

    ret = size(obj%x)

end function
! ##################################################

end module