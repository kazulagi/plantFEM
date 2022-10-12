use FEMDomainClass
use CivilItemClass
!implicit none


type(CivilItem_) :: civil
type(FEMDomain_) :: ground, dem
real(real64) :: length(3)
!type(Random_) :: random


! please wget "dem.stl" from plantfem.org/download/ 
! Or you can download from 
! https://maps.gsi.go.jp/#17/35.513500/139.484214/&base=std&ls=std&disp=1&vs=c1g1j0h0k0l0u0t0z0r0s0m0f1
! Base mesh
call ground%create("Cube3D",x_num=100,y_num=100,z_num=30)
call ground%resize(x=1000.0d0,y=1000.0d0,z=300.0d0)
call ground%move(z=-250.0d0)
call ground%vtk("ground_init")

! DEM data
call dem%open("dem.stl")
call dem%rotate(x=radian(90.0d0) )
call dem%killNodes(NodeList=dem%select(z_max=65.0d0) )
!x -> East (992m), y-> North(997m)
length = dem%length()
! modify scale
call dem%resize(x=997.0d0,y=992.0d0,z=length(3)*997.0d0/length(1) )
call dem%move(to="origin")
call dem%vtk("dem_2")

! fit
ground = civil%ground(femdomain=ground,surface_point=dem%points(),debug=.true.) 

! export
call ground%vtk("ground_fin")

end 