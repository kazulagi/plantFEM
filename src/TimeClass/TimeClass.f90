module TimeClass
    use iso_fortran_env
    implicit none

    type :: time_
        real(real64) ,private:: t1=0.0d0
        real(real64) ,private:: t2=0.0d0
        character(8) :: date
        character(10):: time
        character(5) :: zone
        integer(int32) :: values(8)
    contains
        procedure,public :: start => starttime
        procedure,public :: show => showtime
        procedure,public :: clear => cleartime
        procedure,public :: reset => cleartime
        procedure,public :: sleep => sleeptime
        procedure,public :: DateAndTime => DateAndTimetime
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

function DateAndTimetime(obj) result(date_time)
    class(time_) ,intent(inout) :: obj
    character(:),allocatable ::   date_time

    call date_and_time(obj%date,obj%time,obj%zone,obj%values)

    date_time = obj%date//obj%time//obj%zone

end function

! ########################################
end module