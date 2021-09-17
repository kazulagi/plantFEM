use MaizeClass
use SoybeanClass
implicit none

type(Maize_) :: maize
type(Soybean_) :: Soybean
type(Stem_) :: stem


call maize%create(config="Tutorial/obj/realMaizeConfig.json")
call maize%vtk("maize")

end