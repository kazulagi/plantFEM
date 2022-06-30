program main

use SoybeanClass
implicit none

type(Soybean_) :: soy(1,1)

integer(int32) ::  time_step,row,col
real(real64),allocatable :: Final_Length(:)


do row=1,size(soy,1)
    do col=1,size(soy,2)

        call soy(row,col)%init("Tutorial/obj/mini_soy.json")
        call soy(row,col)%move(x=(row-1)*0.650d0,y=(col-1)*0.120d0 )
        soy(row,col)%apical_dominance_distance = 0.10d0 

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        ! Growth topology
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        ! FinalLength:: InterNode
        Final_Length = linspace([0.030d0,0.060d0],30)
        call soy(row,col)%setFinalInterNodeLength(Length=Final_Length,StemID=0)
        Final_Length = linspace([0.050d0,0.060d0],30)
        call soy(row,col)%setFinalInterNodeLength(Length=Final_Length,StemID=1)
        Final_Length = linspace([0.050d0,0.100d0],30)
        call soy(row,col)%setFinalInterNodeLength(Length=Final_Length,StemID=2)
        Final_Length = linspace([0.050d0,0.100d0],30)
        call soy(row,col)%setFinalInterNodeLength(Length=Final_Length,StemID=3)

        ! FinalLength:: InterNode
        Final_Length = linspace([0.030d0,0.060d0],30)
        call soy(row,col)%setFinalInterNodeLength(Length=Final_Length,StemID=0)

        ! FinalLength:: InterNode
        Final_Length = linspace([0.030d0,0.060d0],30)
        call soy(row,col)%setFinalInterNodeLength(Length=Final_Length,StemID=1)

        ! FinalLength:: Petiole
        Final_Length = linspace([0.050d0,0.18d0],20)
        call soy(row,col)%setFinalPetioleLength(Length=Final_Length,StemID=0)

        ! FinalLength:: Leaf
        Final_Length = linspace([0.050d0,0.15d0],20)
        call soy(row,col)%setFinalLeafLength(Length=Final_Length,StemID=0)

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        
        ! initial growth 
        call soy(row,col)%grow(dt = 100.0d0,simple=.true.)

        ! days

        do time_step = 1, 80
            call soy(row,col)%grow(dt = 1.0d0,simple=.true.,add_apical=.true.)
            call soy(row,col)%vtk("soy_"+str(row)+"_"+str(col)+"_"+zfill(time_step,4) ,single_file=.true.)
            print *, time_step, soy(row,col)%numStem()
            print *, soy(row,col)%NumberOfBranch()
        enddo
        do time_step = 81,120
            call soy(row,col)%grow(dt = 1.0d0,simple=.true.,add_apical=.false.)
            call soy(row,col)%vtk("soy_"+str(row)+"_"+str(col)+"_"+zfill(time_step,4) ,single_file=.true.)
        enddo

    enddo
enddo

end program
