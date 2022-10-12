use SoybeanClass
implicit none

type(Soybean_) :: soy

! set mesh resolution
soy%stem_division = [2,2,10]
soy%peti_division = [2,2,20]
soy%leaf_division = [5,1,8]
call soy%init("Tutorial/obj/soy.json")
call soy%vtk("soy",single_file=.true.)

! check size
call soy%checkMemoryRequirement()


end