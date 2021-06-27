use plantfem

implicit none

type(Soybean_) :: soy
type(Soil_) ::soil

call soy%init(config="soyconf.json") 
call soy%vtk(name="soy")
call soy%deform(disp=(/0.10d0,0.0d0,0.0d0/),z_min=0.300d0) 
call soy%vtk(name="soy_deform")

end