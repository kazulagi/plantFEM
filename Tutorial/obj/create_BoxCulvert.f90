use CivilItemClass
implicit none

type(FEMDomain_) :: boxculvert
type(CivilItem_) :: ci
character(256) :: fpath
type(MPI_) :: mpid

call mpid%start()
fpath = "boxculvert.json"
!call get_command_argument(number=1,value=fpath)
boxculvert = ci%BoxCulvert(&
    width=2000.0d0/1000.0d0,&
    height=1000.0d0/1000.0d0,&
    length=4000.0d0/1000.0d0,&
    top_thickness=100.0d0/1000.0d0,&
    side_thickness=100.0d0/1000.0d0,&
    bottom_thickness=100.0d0/1000.0d0,&
    edge_thickness=70.0d0/1000.0d0,&
    divisions=[5,7,7],&
    cut_angles=[20.0d0,20.0d0])

call boxculvert%vtk(trim(adjustl(fpath)))
!call boxculvert%stl(trim(adjustl(fpath))+".vtk")
call mpid%end()

end