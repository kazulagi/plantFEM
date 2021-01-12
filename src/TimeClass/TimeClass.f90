module TimeClass
    use iso_fortran_env
    implicit none

    type :: time_
        real(real64) ,private:: t1=0.0d0
        real(real64) ,private:: t2=0.0d0
    contains
        procedure,public :: start => starttime
        procedure,public :: show => showtime
        procedure,public :: clear => cleartime
        procedure,public :: reset => cleartime
        procedure,public :: sleep => sleeptime
    end type

contains

! ########################################
subroutine starttime(obj)
    class(time_) ,intent(inout) :: obj

    call cpu_time(obj%t1)

end subroutine
! ########################################


! ########################################
subroutine showtime(obj)
    class(time_) ,intent(inout) :: obj

    call cpu_time(obj%t2)
    print *, obj%t2-obj%t1

end subroutine
! ########################################

! ########################################
subroutine cleartime(obj)
    class(time_) ,intent(inout) :: obj

    obj%t1=0.0d0
    obj%t2=0.0d0

end subroutine
! ########################################



! ########################################
subroutine sleeptime(obj,time)
    class(time_) ,intent(inout) :: obj
    integer(int32) :: time

    call sleep(time)

end subroutine
! ########################################


end module