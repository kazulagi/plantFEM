use :: SoybeanClass

type(Soybean_) :: soy
type(Random_)  :: random
real(real64),allocatable   :: distances(:)
integer(int32)::id_range(2)

call soy%init(config="Tutorial/obj/soy.json")
call soy%update()

distances = soy%getDistanceFromGround()
id_range  = soy%getRangeOfNodeID(root=true)
distances( id_range(1)  :  id_range(2)  ) = 0.0d0
call soy%vtk("soy",scalar_field=distances,single_file=true)

end