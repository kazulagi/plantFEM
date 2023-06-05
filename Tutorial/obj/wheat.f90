use WheatClass
implicit none

type(Wheat_) :: wheat

! under development
call wheat%init("Tutorial/obj/wheat.json")
call wheat%vtk("wheat",single_file=.true.)

end