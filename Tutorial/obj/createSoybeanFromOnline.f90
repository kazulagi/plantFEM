use plantfem
implicit none

type(Soybean_) :: soy

call soy%init(config="https://raw.githubusercontent.com/kazulagi/plantFEM/master/Tutorial/obj/realSoybeanConfig.json")

call soy%stl("soy")

end