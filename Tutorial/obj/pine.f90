use FEMDomainClass
use PineClass
implicit none

type(Pine_) :: pine

! This command creates pine tree, but only main stem.
call pine%init("Tutorial/obj/pine.json")
print *, "created"
call pine%stem(1)%vtk("pine")

end 