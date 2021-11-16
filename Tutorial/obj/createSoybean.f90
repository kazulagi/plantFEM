use SoybeanClass
implicit none

type(Soybean_) :: soy

call soy%init(config="Tutorial/obj/realSoybeanConfig.json")
call soy%stl(name="soy")


! measuring volume (m^2)
call print(soy%getVolume())
call print(soy%ne())

end