use plantFEM
implicit none

type(Soybean_) :: soy
type(Light_)   :: light
type(Air_)     :: Air
real(real64),allocatable :: ppfd(:),volume(:),photo(:)

call light%init()
call Air%init()
call soy%init(config="Tutorial/obj/soy.json")

! get ppfd value
ppfd   = soy%getPPFD(Light=Light,Transparency=0.90d0)
photo  = soy%getPhotoSynthesisSpeedPerVolume(&
    Light=Light,Air=Air,dt=60.0d0*60.0d0*8.0d0,ppfd=ppfd) ! μg/m^3/day
volume = soy%getVolumePerElement()

! export
call soy%vtk(name="ppfd_ht",&
    scalar_field=ppfd,&
    single_file=.true.)

call soy%vtk(name="photosynth",&
    scalar_field=photo,&
    single_file=.true.)

print *, "Total C6H12O6 balance (micro-gram/day)",dot_product(photo,volume) 
print *, "Total C6H12O6 balance (gram/day)",dot_product(photo,volume)/1000.0d0/100.0d0

end