use EarthClass
implicit none

type(Earth_) :: Earth

call earth%init()

call earth%setTimeZone(name="Tokyo")
earth%MyPosition = [36.380d0, 140.470d0]
earth%Year   = 2022
earth%Month  = 7
earth%Day    = 1
earth%Hour   = 12
earth%minute = 0
earth%second = 0


print *, earth%getSunPosition()



end