use SoybeanClass
implicit none


type(Soybean_) :: soy
real(real64),allocatable :: displacement(:),a(:),Modes(:,:), Freq(:)

call soy%init("Tutorial/obj/mini_soy.json")
call soy%remove(root=.true.)
call soy%vtk("soy",single_file=.true.)
call soy%checkMemoryRequirement()

call soy%setYoungModulus(100000.0d0,stem=.true.)
call soy%setYoungModulus(10000.0d0,leaf=.true.)
call soy%setPoissonRatio(0.30d0)
call soy%setDensity(1.200d0)

Modes = soy%getEigenMode(ground_level=0.020d0,penalty=10000000.0d0,&
    debug=.true., Frequency=Freq,num_mode=20)

! fundamental
displacement = Modes(:,1)

print *, maxval(displacement),minval(displacement)
displacement = displacement * dble(1.0e+7)

call soy%deform(displacement = displacement)

call soy%vtk("soy_deform",single_file = .true.)

end 