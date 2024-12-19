use SoybeanClass
implicit none

type(Soybean_) :: soy

call soy%init(config="Tutorial/obj/soy_seed.json")
call soy%vtk("soy_seed_v2",single_file=True)

end