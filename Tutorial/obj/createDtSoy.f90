use plantfem

implicit none

type(Soybean_) :: soy
integer(int32) :: i
real(real64)   :: length, coord(3)

! Initialize soybean
call soy%init(config="Tutorial/obj/realSoybeanConfig.json") 

! change node-length by its height
do i=1, size(soy%stem)
    if( soy%stem(i)%femdomain%mesh%empty()) exit
    coord = soy%stem(i)%getCoordinate("A")
    length = coord(3)*0.20d0 + 0.010d0
    print *, length
    call soy%stem(i)%grow(length=length)
enddo

! update geoetry
call soy%update()

! move soy
call soy%move(x=0.0d0)

! export as vtk
call soy%vtk(name="soy3")

end