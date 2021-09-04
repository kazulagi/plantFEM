use plantfem

implicit none

type(Soybean_) :: soy
type(Soil_) ::soil

call soy%init(config="Tutorial/obj/realSoybeanConfig.json") 
call soy%vtk(name="soy")
call soy%deform(disp=[0.00d0,0.0d0,0.0d0],z_min=10000.00d0) 
call soy%vtk(name="soy_deform")

end