use MaizeClass
implicit none

type(Maize_) :: maize
real(real64),allocatable :: displacement(:),Modes(:,:), Freq(:)

maize%stem_division(1:3) = [2,2,10]
maize%leaf_division(1:3) = [4,1,10]
maize%ear_division(1:3) = [3,3,8]
maize%panicle_division(1:3) = [2,1,2]
call maize%create(config="Tutorial/obj/maize.json")
call maize%remove(root=.true.)
call maize%vtk("maize",single_file=.true.)
call maize%checkMemoryRequirement()

call maize%setYoungModulus(70000.0d0,stem=.true.)
call maize%setYoungModulus(10000.0d0,leaf=.true.)
call maize%setYoungModulus(70000.0d0,Ear=.true.)
call maize%setYoungModulus(70000.0d0,Panicle=.true.)

call maize%setPoissonRatio(0.30d0)
call maize%setDensity(0.9300d0)

Modes = maize%getEigenMode(ground_level=0.020d0,penalty=1000000.0d0,&
    debug=.true., Frequency=Freq,EbOM_Algorithm="P2P")

! fundamental
displacement = Modes(:,1)

print *, maxval(displacement),minval(displacement)

call maize%deform(displacement = displacement)

call maize%vtk("soy_deform",single_file = .true.)
call print(Freq(1:10) )

end