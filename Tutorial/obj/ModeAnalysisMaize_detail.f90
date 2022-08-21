use ArrayClass
use MaizeClass
implicit none

type(Maize_) :: maize
real(real64),allocatable :: displacement(:),Modes(:,:), Freq(:)
integer(int32),allocatable :: elementList(:,:),stem_elementList(:,:),list(:)



maize%stem_division(1:3) = [2,2,10]
maize%leaf_division(1:3) = [4,1,10]
maize%ear_division(1:3) = [3,3,8]
maize%panicle_division(1:3) = [3,2,3]
call maize%create(config="Tutorial/obj/realMaizeConfig.json")
call maize%remove(root=.true.)
call maize%vtk("maize",single_file=.true.)
call maize%checkMemoryRequirement()


elementList = maize%getElementList(z_min=1.00d0)

call maize%setYoungModulus(20000.0d0,stem=.true.)

! partially change Young modulus @ stem
elementList = maize%getElementList(z_min=0.700d0)
call maize%setYoungModulus( 7000.0d0,stem=.true.,elementList=ElementList)

call maize%setYoungModulus( 1000.0d0,leaf=.true.)

call maize%setYoungModulus(30000.0d0,Ear=.true.)
call maize%setYoungModulus( 5000.0d0,Panicle=.true.)

call maize%setPoissonRatio(0.30d0)
call maize%setDensity(0.9300d0)
call maize%setDensity(0.9800d0,Ear=.true.)

call maize%vtk("maize_E",  single_file=.true.,scalar_field=maize%getYoungModulusField() )
call maize%vtk("maize_v",  single_file=.true.,scalar_field=maize%getPoissonRatioField() )
call maize%vtk("maize_rho",single_file=.true.,scalar_field=maize%getDensityField() )

Modes = maize%getEigenMode(ground_level=0.020d0,penalty=1000000.0d0,&
    debug=.true., Frequency=Freq,EbOM_Algorithm="P2P")


! fundamental
do i_i=1,10
    displacement = Modes(:,i_i)


    call maize%deform(displacement = displacement)

    call maize%vtk("maize_deform"+zfill(i_i,3),single_file = .true.)

    call maize%deform(displacement = - displacement)
enddo

call print(Freq(1:10) )

end