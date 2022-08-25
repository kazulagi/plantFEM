use RiceClass
implicit none

type(Rice_) :: Rice
real(real64),allocatable :: displacement(:),Modes(:,:), Freq(:)

rice%stem_division(1:3) = [2,2,12]
rice%leaf_division(1:3) = [4,1,10]
rice%panicle_division(1:3) = [1,1,50]
call Rice%create(config="Tutorial/obj/rice.json")
call Rice%remove(root=.true.)
call Rice%vtk("Rice",single_file=.true.)
call Rice%checkMemoryRequirement()

call Rice%setYoungModulus(70000.0d0,stem=.true.)
call Rice%setYoungModulus(10000.0d0,leaf=.true.)
call Rice%setYoungModulus(70000.0d0,Panicle=.true.)

call Rice%setPoissonRatio(0.30d0)
call Rice%setDensity(0.9300d0)
call Rice%setDensity(10.00d0,Panicle=.true.)

Modes = Rice%getEigenMode(ground_level=0.020d0,penalty=1000000.0d0,&
    debug=.true., Frequency=Freq,EbOM_Algorithm="P2P")

call print(Freq(1:10) )
call Rice%vtk("rice_deform",single_file = .true.)
call Rice%export_eig("rice_eig",frequency=Freq,ModeVectors=Modes,stress_type="I1")

end