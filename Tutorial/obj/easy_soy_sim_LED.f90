use LightClass
use SoybeanClass
implicit none

type(Soybean_) :: soy
type(Light_)   :: light
type(Air_)     :: Air
real(real64),allocatable :: ppfd(:),volume(:),photo(:)
real(real64),allocatable :: spectrum(:,:),R_FR(:)
integer(int32),allocatable :: elemlist(:,:)
real(real64) :: z_min,z_max
type(IO_) :: f

call light%init()
call light%turnOff()
! White LED
call light%addSpectrum(wavelength=450.0d0,peak_radiation=1.0d0,sigma=20.0d0)
call light%addSpectrum(wavelength=570.0d0,peak_radiation=0.20d0,sigma=40.0d0)
call light%addSpectrum(wavelength=580.0d0,peak_radiation=0.20d0,sigma=60.0d0)
call light%addSpectrum(wavelength=680.0d0,peak_radiation=2.20d0,sigma=60.0d0)
! 色を確認
print *,int(light%to_RGB())



call Air%init()
call soy%init(config="Tutorial/obj/soy.json")

! get ppfd value
ppfd   = soy%getPPFD(Light=Light,Transparency=0.90d0)
spectrum = soy%getSpectrum(Light=Light,Transparency=0.90d0)
R_FR     = soy%to_R_FR(spectrum)
photo  = soy%getPhotoSynthesisSpeedPerVolume(&
    Light=Light,Air=Air,dt=60.0d0*60.0d0*8.0d0,ppfd=ppfd) ! μg/m^3/day
volume = soy%getVolumePerElement()

! PPFD (micro-mol/m^2/s)
call soy%vtk(name="ppfd_ht",&
    scalar_field=ppfd,&
    single_file=.true.)
! R/FR ratio (600-700nm/700-800nm)
call soy%vtk(name="R_FR",&
    scalar_field=R_FR,&
    single_file=.true.)
! Photosynthesis speed (micro-gram/m^3)
call soy%vtk(name="photosynth",&
    scalar_field=photo,&
    single_file=.true.)

print *, "Total C6H12O6 balance (micro-gram/day)",dot_product(photo,volume) 
print *, "Total C6H12O6 balance (gram/day)",dot_product(photo,volume)/1000.0d0/100.0d0

do i_i=0,9
    ! average spectrum for each layer (interval: 0.1m)
    call f%open("spectrum"+zfill(i_i,3)+".txt","w")
    z_min = dble(i_i  )/10.0d0
    z_max = dble(i_i+1)/10.0d0
    call f%write(average(transpose(spectrum(soy%getGlobalElementIdx(z_min=z_min,z_max=z_max),:))))
    call f%close()
enddo

end