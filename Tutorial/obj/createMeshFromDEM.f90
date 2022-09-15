use FEMDomainClass
use CivilItemClass
!implicit none


type(CivilItem_) :: civil
type(FEMDomain_) :: ground, dem
real(real64) :: length(3)
!type(Random_) :: random

call ground%create("Cube3D",x_num=100,y_num=100,z_num=30)
call ground%resize(x=1000.0d0,y=1000.0d0,z=100.0d0)
call ground%move(z=-50.0d0)
call ground%vtk("ground_init")

call dem%open("dem.stl")
call dem%rotate(x=radian(90.0d0) )
call dem%killNodes(NodeList=dem%select(z_max=65.0d0) )
!x -> East (992m), y-> North(997m)

length = dem%length()

call dem%resize(x=997.0d0,y=992.0d0,z=length(3)*997.0d0/length(1) )
call dem%move(to="origin")
call dem%vtk("dem_2")

ground = civil%ground(femdomain=ground,surface_point=dem%points(),debug=.true.) 

call ground%vtk("ground_fin")

end 