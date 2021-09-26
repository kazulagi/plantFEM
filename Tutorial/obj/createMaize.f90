use MaizeClass
implicit none

type(Maize_) :: maize

call maize%create(config="Tutorial/obj/realMaizeConfig.json")
call maize%vtk("maize")

end