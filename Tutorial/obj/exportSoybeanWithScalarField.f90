use SoybeanClass
implicit none

type(soybean_) :: soy
real(real64),allocatable :: scalar_field(:)

call soy%init(config="Tutorial/obj/soy.json")

call soy%vtk("soy",single_file=.true.)
scalar_field = linspace([0.0d0,dble(soy%nn()) ],soy%nn() )
call soy%vtk("soy_scalar",single_file=.true.,scalar_field=scalar_field)

end