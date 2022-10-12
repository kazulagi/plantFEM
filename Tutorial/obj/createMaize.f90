use MaizeClass
implicit none

type(Maize_) :: maize

call maize%create(config="Tutorial/obj/realMaizeConfig.json")
call maize%remove(root=.true.)
call maize%vtk("maize",single_file=.true.)
call maize%checkMemoryRequirement()

end