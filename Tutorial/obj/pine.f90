use PineClass
implicit none

type(Pine_) :: pine
type(FEMDomain_) :: cyl

! This command creates pine tree, but only main stem.
call pine%init("Tutorial/obj/pine.json")
call pine%stem(1)%vtk("pine_stem")

end 