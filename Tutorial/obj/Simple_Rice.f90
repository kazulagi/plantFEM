use RiceClass

implicit none

type(Rice_) :: rice

call rice%create("Tutorial/obj/rice.json")
call rice%vtk("rice",single_file=.true.)
end