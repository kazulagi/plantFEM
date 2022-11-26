program main

use SoybeanClass
implicit none

type(Soybean_) :: soy

integer(int32) ::  time_step
real(real64),allocatable :: Final_Length(:)

call soy%init("Tutorial/obj/mini_soy.json")

! FinalLength:: InterNode
Final_Length = linspace([0.030d0,0.060d0],30)
call soy%setFinalInterNodeLength(Length=Final_Length,StemID=0)

! FinalLength:: Petiole
Final_Length = linspace([0.050d0,0.18d0],20)
call soy%setFinalPetioleLength(Length=Final_Length,StemID=0)

! FinalLength:: Leaf
Final_Length = linspace([0.050d0,0.15d0],20)
call soy%setFinalLeafLength(Length=Final_Length,StemID=0)

! Edit soybean
call soy%vtk("soy_before",single_file=.true.)
call soy%grow(dt = 100.0d0,simple=.true.)

! days
do time_step = 1, 80
    call soy%grow(dt = 1.0d0,simple=.true.,add_apical=.true.)
    call soy%vtk("soy_"+zfill(time_step,4) ,single_file=.true.)
enddo
do time_step = 81,120
    call soy%grow(dt = 1.0d0,simple=.true.,add_apical=.false.)
    call soy%vtk("soy_"+zfill(time_step,4) ,single_file=.true.)
enddo


call soy%vtk("soy_after",single_file=.true.)

end program
