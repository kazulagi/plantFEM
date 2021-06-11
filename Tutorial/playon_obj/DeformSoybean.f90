use plantfem

implicit none

type(Soybean_) :: soy
type(Soil_) ::soil

call soy%init(config="Tutorial/playon_obj/realSoybeanConfig.json") 
call soy%vtk(name="soy")
call soy%deform(disp=(/1.0d0,0.0d0,0.0d0/),z_min=0.60d0) 

end