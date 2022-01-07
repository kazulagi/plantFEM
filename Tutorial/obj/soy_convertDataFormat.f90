use SoybeanClass
implicit none

type(Soybean_) :: soy
real(real64),allocatable :: volume(:)

call soy%init(config="Tutorial/obj/soy.json")

volume = soy%getVolumePerElement()

print *, soy%convertDataFormat(scalar=volume,new_format=PF_SOY_OBJECT_WISE)
print *, sum(soy%convertDataFormat(scalar=volume,new_format=PF_SOY_OBJECT_WISE))

end