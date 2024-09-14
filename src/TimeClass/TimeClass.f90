module TimeClass
   use omp_lib
   use iso_fortran_env
   implicit none

   type :: datetime_
      integer(int32) :: year
      integer(int32) :: month
      integer(int32) :: date
      integer(int32) :: hour
      integer(int32) :: minutes
      real(real64)   :: seconds
   contains
      procedure :: show => show_datetime_timeclass
   end type

   interface operator(+)
      module procedure :: add_two_datetime
   end interface

   interface to_datetime
      module procedure :: to_datetime_timeclass
   end interface

   type :: time_
      real(real64), private:: t1 = 0.0d0
      real(real64), private:: t2 = 0.0d0
      character(8) :: date
      character(10):: time
      character(5) :: zone
      integer(int32) :: values(8)
   contains
      procedure, public :: start => starttime
      procedure, public :: show => showtime
      procedure, public :: clear => cleartime
      procedure, public :: reset => cleartime
      procedure, public :: sleep => sleeptime
      procedure, public :: DateAndTime => DateAndTimetime
      procedure, public :: t => tTime
      procedure, public :: freq => freqTime
   end type

contains

! ########################################
   function to_datetime_timeclass(year, month, date, hour, minutes, seconds) result(ret)
      type(datetime_) :: ret
      integer(int32), optional, intent(in)  :: year, month, date, hour, minutes
      real(real64), optional, intent(in)  :: seconds

      if (present(year)) then
         ret%year = year
      else
         ret%year = 0
      end if
      if (present(month)) then
         ret%month = month
      else
         ret%month = 0
      end if
      if (present(date)) then
         ret%date = date
      else
         ret%date = 0
      end if
      if (present(hour)) then
         ret%hour = hour
      else
         ret%hour = 0
      end if
      if (present(minutes)) then
         ret%minutes = minutes
      else
         ret%minutes = 0
      end if
      if (present(seconds)) then
         ret%seconds = seconds
      else
         ret%seconds = 0.0d0
      end if

   end function
! ########################################

   subroutine show_datetime_timeclass(dt)
      class(datetime_), intent(in) :: dt

      print *, dt%year
      print *, dt%month
      print *, dt%date
      print *, dt%hour
      print *, dt%minutes
      print *, dt%seconds

   end subroutine

! ########################################
   function add_two_datetime(dt1, dt2) result(ret)
      type(datetime_), intent(in) :: dt1, dt2
      type(datetime_) :: ret

      ret = dt1
      ret%year = ret%year + dt2%year
      ret%month = ret%month + dt2%month
      ret%date = ret%date + dt2%date

      ret%hour = ret%hour + dt2%hour
      ret%minutes = ret%minutes + dt2%minutes
      ret%seconds = ret%seconds + dt2%seconds

      if (ret%seconds >= 60.0d0) then
         ret%minutes = ret%minutes + floor(ret%seconds/60.d0)
         ret%seconds = ret%seconds - floor(ret%seconds/60.d0)
      end if

      if (ret%minutes >= 60) then
         ret%hour = ret%hour + (ret%minutes - mod(ret%minutes, 60))/60
         ret%minutes = mod(ret%minutes, 60)
      end if

      if (ret%hour >= 24) then
         ret%date = ret%date + (ret%minutes - mod(ret%minutes, 24))/24
         ret%hour = mod(ret%hour, 24)
      end if

      do
         if (ret%month > 12) then
            ret%year = ret%year + 1
            ret%month = ret%month - 12
         end if
         if (ret%date > number_of_date_for_month(year=ret%year, month=ret%month)) then
            ret%date = ret%date - number_of_date_for_month(year=ret%year, month=ret%month)
            ret%month = ret%month + 1
         else
            exit
         end if
      end do
   end function
! ########################################

   function number_of_date_for_month(year, month) result(ret)
      integer(int32), intent(in) :: year, month
      integer(int32) :: ret

      if (month == 1) then
         ret = 31
      elseif (month == 2) then
         if (mod(year, 4) == 0) then
            if (mod(year, 100) == 0) then
               if (mod(year, 400) == 0) then
                  ret = 29
               else
                  ret = 28
               end if
            else
               ret = 29
            end if
         else
            ret = 28
         end if
      elseif (month == 3) then
         ret = 31
      elseif (month == 4) then
         ret = 30
      elseif (month == 5) then
         ret = 31
      elseif (month == 6) then
         ret = 30
      elseif (month == 7) then
         ret = 31
      elseif (month == 8) then
         ret = 31
      elseif (month == 9) then
         ret = 30
      elseif (month == 10) then
         ret = 31
      elseif (month == 11) then
         ret = 30
      elseif (month == 12) then
         ret = 31
      else
         ret = 0
      end if

   end function

! ########################################
   subroutine starttime(obj)
      class(time_), intent(inout) :: obj

      !call cpu_time(obj%t1)
      obj%t1 = omp_get_wtime()

   end subroutine
! ########################################

! ########################################
   subroutine showtime(obj)
      class(time_), intent(inout) :: obj

      !call cpu_time(obj%t2)
      obj%t2 = omp_get_wtime()

      print *, obj%t2 - obj%t1

   end subroutine
! ########################################

! ########################################
   subroutine cleartime(obj)
      class(time_), intent(inout) :: obj

      obj%t1 = 0.0d0
      obj%t2 = 0.0d0

   end subroutine
! ########################################

! ########################################
   subroutine sleeptime(obj, time)
      class(time_), intent(inout) :: obj
      integer(int32) :: time

      call sleep(time)

   end subroutine
! ########################################

   function DateAndTimetime(obj) result(date_time)
      class(time_), intent(inout) :: obj
      character(:), allocatable ::   date_time

      call date_and_time(obj%date, obj%time, obj%zone, obj%values)

      date_time = obj%date//obj%time//obj%zone

   end function

! ########################################

   function tTime(obj, t_range, hz, max_sample) result(t_axis)
      class(time_), intent(in) :: obj
      real(real64), intent(in) :: t_range(1:2), hz
      real(real64), allocatable :: t_axis(:)
      integer(int32), optional, intent(in) :: max_sample
      real(real64) :: dt
      integer(int32) :: this_num, i

      dt = 1.0d0/hz

      this_num = int((t_range(2) - t_range(1))*hz)

      if (present(max_sample)) then
         if (max_sample <= this_num) then
            this_num = max_sample
         end if
      end if

      allocate (t_axis(this_num))
      t_axis(1) = t_range(1)
      do i = 2, this_num
         t_axis(i) = t_axis(i - 1) + dt
      end do

   end function

! ############################################################
   function freqTime(obj, time_axis) result(freq)
      class(time_), intent(in) :: obj
      real(real64), intent(in) :: time_axis(:)
      real(real64), allocatable :: freq(:)
      real(real64) :: dt, tt, Nyquist_frequency, dfreq
      integer(int32) :: n, i

      tt = maxval(time_axis) - minval(time_axis)
      n = size(time_axis)/2
      dt = tt/dble(n)
      Nyquist_frequency = 1.0d0/dt/2.0d0
      dfreq = Nyquist_frequency/dble(n)
      allocate (freq(n))
      do i = 1, n
         freq(i) = dfreq*i
      end do

   end function

end module
