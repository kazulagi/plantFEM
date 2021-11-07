use SoybeanClass
implicit None

type(Soybean_) :: soy
integer(int32) :: n

call soy%create("Tutorial/obj/realSoybeanConfig_mini.json")
call print(soy%getVolume())
call soy%stl("week0/soy")

! simple growth simulation
! for 4 weeks
! new nodes are generated week by week
do n=1,4
    call soy%grow(dt=7*day("sec"), simple=.true.)
    call print(soy%getVolume())
    call soy%stl("week"//str(n)//"/soy")
enddo

end