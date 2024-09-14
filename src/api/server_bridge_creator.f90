
use FEMDomainClass
use CivilItemClass
implicit none

type(CivilItem_) :: ci
type(FEMDomain_) :: bridge
character(256) :: fpath

call get_command_argument(number=1, value=fpath)
bridge = ci%RigidFrameViaduct(config=trim(adjustl(fpath)))
call bridge%vtk(trim(fpath))
call bridge%stl(trim(fpath) + ".vtk")

end
