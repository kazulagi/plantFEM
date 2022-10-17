use CivilItemClass
implicit none

type(FEMDomain_) :: boxculvert
type(CivilItem_) :: ci
character(256) :: fpath
type(MPI_) :: mpid

call mpid%start()
fpath = "boxculvert.json"
call get_command_argument(number=1,value=fpath)
boxculvert = ci%BoxCulvert(trim(adjustl(fpath)))
call boxculvert%vtk(trim(adjustl(fpath)))
call boxculvert%stl(trim(adjustl(fpath))+".vtk")
call mpid%end()

end