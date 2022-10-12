use CivilItemClass
implicit none

type(FEMDomain_) :: dam
type(CivilItem_) :: ci
character(256) :: fpath

call get_command_argument(number=1,value=fpath)
dam = ci%EarthDam(config=trim(adjustl(fpath)) )
call dam%vtk(trim(adjustl(fpath)))

end