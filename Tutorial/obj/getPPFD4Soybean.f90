use SoybeanClass
implicit none

type(Soybean_) :: soy
type(Light_)   :: light
real(real64),allocatable :: ppfd(:)

call light%init()
call soy%init(config="Tutorial/obj/soy.json")

! get ppfd value
ppfd = soy%getPPFD(Light=Light,Transparency=0.90d0)

! export
call soy%vtk(name="ppfd_ht",&
    scalar_field=ppfd,&
    single_file=.true.)



end