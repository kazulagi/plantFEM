use :: SoybeanClass

type(Soybean_) :: soy
type(Random_)  :: random
real(real64),allocatable   :: a_field(:)

call soy%init(config="Tutorial/obj/soy.json")

a_field = soy%getDistanceFromGround()

call soy%vtk("soy",scalar_field=a_field,single_file=true)

end