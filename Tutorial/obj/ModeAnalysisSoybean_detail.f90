use soybeanClass
implicit none

type(soybean_) :: soybean
real(real64),allocatable :: displacement(:),Modes(:,:), Freq(:)
integer(int32),allocatable :: elementList(:,:),stem_elementList(:,:)

!soybean%stem_division(1:3) = [2,2,10]
!soybean%leaf_division(1:3) = [4,1,10]
call soybean%create(config="Tutorial/obj/soy.json")
call soybean%remove(root=.true.)
call soybean%vtk("soybean",single_file=.true.)
call soybean%checkMemoryRequirement()


call soybean%setYoungModulus(20000.0d0,stem=.true.)

! partially change Young modulus @ stem
elementList = soybean%getElementList(z_min=0.300d0,x_max=0.03d0,x_min=-0.03d0)
call soybean%setYoungModulus( 7000.0d0,stem=.true., elementList=ElementList)

call soybean%setYoungModulus( 1000.0d0,leaf=.true.)

call soybean%setPoissonRatio(0.30d0)
call soybean%setDensity(0.9300d0)

call soybean%vtk("soybean_E",  single_file=.true.,scalar_field=soybean%getYoungModulusField() )
call soybean%vtk("soybean_v",  single_file=.true.,scalar_field=soybean%getPoissonRatioField() )
call soybean%vtk("soybean_rho",single_file=.true.,scalar_field=soybean%getDensityField() )

Modes = soybean%getEigenMode(ground_level=0.020d0,penalty=1000000.0d0,&
    debug=.true., Frequency=Freq,EbOM_Algorithm="P2P")

! fundamental
displacement = Modes(:,1)

print *, maxval(displacement),minval(displacement)

call soybean%deform(displacement = displacement)

call soybean%vtk("soy_deform",single_file = .true.)
call print(Freq(1:10) )

end