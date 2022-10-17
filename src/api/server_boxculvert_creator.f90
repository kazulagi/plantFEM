use CivilItemClass
implicit none

type(FEMDomain_) :: boxculvert
type(CivilItem_) :: ci
character(256) :: fpath

call get_command_argument(number=1,value=fpath)
boxculvert = ci%BoxCulvert(config="Tutorial/obj/boxculvert.json")
call boxculvert%vtk(trim(adjustl(fpath)))
call boxculvert%stl(trim(adjustl(fpath))+".vtk")

end