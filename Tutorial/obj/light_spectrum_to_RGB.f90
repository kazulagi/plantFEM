use lightclass
implicit none

type(Light_) :: light
integer(int32)::rgb(1:3)
character(:),allocatable :: command

call light%init()
call light%turnOff()

! White LED

call light%addSpectrum(wavelength=450.0d0,peak_radiation=3.0d0*1.0d0,sigma=20.0d0)
call light%addSpectrum(wavelength=570.0d0,peak_radiation=3.0d0*0.40d0,sigma=40.0d0)
call light%addSpectrum(wavelength=580.0d0,peak_radiation=3.0d0*0.40d0,sigma=60.0d0)

! 色を確認
print *,int(light%to_RGB())
rgb = int(light%to_RGB())
command="https://convertingcolors.com/rgb-co&
    lor-"+str(rgb(1))+"_"+str(rgb(2))+"_"+str(rgb(3))+&
    ".html?search=RGB("+str(rgb(1))+&
    ",%"+str(rgb(2))+",%"+str(rgb(3))+")"
call print(command)
call system("firefox '"+command+"'")

stop

end