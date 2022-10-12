use GrapeClass
implicit none

type(Grape_) :: grape

call grape%create(config = "Tutorial/obj/realGrapeConfig.json")
call grape%vtk("grape")
call grape%stl("grape")

end