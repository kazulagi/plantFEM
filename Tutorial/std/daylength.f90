use EarthClass
implicit none

type(Earth_) :: Earth
real(real64) :: times(1:2)
call earth%init()

call earth%setTimeZone(name="Tokyo")
earth%MyPosition = [36.380d0, 140.470d0]
earth%Year   = 2026
earth%Month  = 1
earth%Day    = 1
earth%Hour   = 0
earth%minute = 0
earth%second = 0

do i_i=1,365
    earth%day = earth%day + 1
    times = earth%getSunTime()
    print *, earth%Year,earth%Month,earth%Day,times(2)-times(1)
enddo

end