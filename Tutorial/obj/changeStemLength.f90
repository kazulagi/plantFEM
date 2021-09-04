use SoybeanClass

implicit none

type(Soybean_) :: soy
real(real64),allocatable :: stemLength(:)
integer(int32) :: i
real(real64) :: a,b

call soy%init(config="Tutorial/obj/realSoybeanConfig.json") 
stemLength = soy%stemLength(StemID=0)
a = 0.01d0
do i=1, size(stemLength)
    stemLength(i) = 0.140d0 - a*dble(i) 
enddo
print *, stemLength

call soy%resize(StemID=0,StemLength=stemlength)

call soy%stl("soy2")

end