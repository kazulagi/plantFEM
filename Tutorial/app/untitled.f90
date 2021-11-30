use plantfem

implicit none

type(Soybean_) :: soy
type(Soil_) ::soil

call soy%init(config="Tutorial/obj/realSoybeanConfig.json") 
call soy%stl(name="soy")
!call soy%json(name="soy")
!call soy%vtk(name="soy")


call soil%create(x_num=3,y_num=3,z_num=1)
call soil%resize(x=3.0d0, y=3.0d0, z=1.0d0)
call soil%msh(name="soil")

end