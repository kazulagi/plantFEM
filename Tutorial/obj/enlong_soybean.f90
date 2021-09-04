use SoybeanClass

type(Soybean_) :: soy
!real(real64),allocatable :: relation(:,:)

call soy%init(config="soyconf.json")

call soy%stem(1)%grow(length=1.0d0)
call soy%update()

call soy%msh("soy")

end