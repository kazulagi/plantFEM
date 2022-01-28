use EarthClass
implicit none

type(Earth_) :: Earth

call earth%init()

call earth%setTimeZone(name="Tokyo")
! real-time position
print *, earth%getSunPosition()



end