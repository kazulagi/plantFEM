use SoybeanClass

implicit none

type(Soybean_) :: soy
real(real64),allocatable :: stemLength(:)
integer(int32),allocatable :: apicalNodes(:)
integer(int32) :: i
real(real64) :: a,b

! Get Full-control of soybean

! initialize
call soy%init(config="Tutorial/obj/realSoybeanConfig.json") 

! get lengths of stem nodes.
stemLength = soy%stemLength(StemID=1)

! change lengths
a = 0.01d0
do i=1, size(stemLength)
    stemLength(i) = 0.100d0 + a*dble(i) 
enddo
print *, stemLength
call soy%resize(StemID=1,StemLength=stemlength)

! search apical
print *, soy%NumberOfBranch()
apicalNodes = soy%findApical()


! ...and enlong them
do i=1, size(apicalNodes)
    call soy%stem( apicalNodes(i) )%grow(length_rate=2.0d0)
    call soy%update()
enddo

! export as stl.
call soy%stl("soy3")

end