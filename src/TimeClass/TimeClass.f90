module TimeClass
    use omp_lib
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
        procedure,public :: t => tTime
        procedure,public :: freq => freqTime
    end type

contains

! ########################################
subroutine starttime(obj)
    class(time_) ,intent(inout) :: obj

    !call cpu_time(obj%t1)
    obj%t1 = omp_get_wtime()

end subroutine
! ########################################


! ########################################
subroutine showtime(obj)
    class(time_) ,intent(inout) :: obj

    !call cpu_time(obj%t2)
    obj%t2 = omp_get_wtime()

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

function tTime(obj,t_range,hz,max_sample) result(t_axis)
    class(time_) ,intent(in) :: obj
    real(real64),intent(in) :: t_range(1:2), hz
    real(real64),allocatable :: t_axis(:)
    integer(int32),optional,intent(in) :: max_sample
    real(real64) :: dt
    integer(int32) :: this_num, i
    
    dt = 1.0d0/hz

    
    this_num = int((t_range(2) - t_range(1) )*hz)
    

    if(present(max_sample))then
        if(max_sample <= this_num)then
            this_num = max_sample
        endif
    endif

    allocate(t_axis(this_num))
    t_axis(1) = t_range(1)
    do i=2,this_num
        t_axis(i) = t_axis(i-1) + dt
    enddo

end function

! ############################################################
function freqTime(obj,time_axis) result(freq)
    class(time_) ,intent(in) :: obj
    real(real64),intent(in) :: time_axis(:)
    real(real64),allocatable :: freq(:)
    real(real64) :: dt,tt, Nyquist_frequency,dfreq
    integer(int32) :: n, i
    

    tt = maxval(time_axis) - minval(time_axis) 
    n  = size(time_axis)/2
    dt = tt/dble(n)
    Nyquist_frequency = 1.0d0/dt/2.0d0
    dfreq = Nyquist_frequency/dble(n)
    allocate(freq(n) )
    do i=1,n
        freq(i) = dfreq*i
    enddo
    
    


end function

end module