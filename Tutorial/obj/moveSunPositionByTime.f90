use EarthClass
use LightClass
implicit none

type(Earth_)   :: earth
type(Light_)   :: light

call earth%init()
call earth%setTimeZone(name="Tokyo")

call light%init()
call light%setSunLight(earth=earth)
call light%updateSunLight(DateTime=[2022,02,28,12,00,00])

print *, light%angles
print *, earth%year,earth%month,earth%day,earth%hour,earth%minute,earth%second
print *, earth%getSunPosition(dt=720000)
print *, earth%year,earth%month,earth%day,earth%hour,earth%minute,earth%second
print *, earth%getSunPosition(dt=720000)
print *, earth%year,earth%month,earth%day,earth%hour,earth%minute,earth%second

end