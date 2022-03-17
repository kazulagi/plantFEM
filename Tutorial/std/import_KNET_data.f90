use IOClass
use ArrayClass
implicit none

! K-net data analysis
type(IO_) :: f
character(:),allocatable :: file_path
character(:),allocatable :: Station_Name

real(real64),allocatable :: accel_wave(:,:),hz(:)
complex(complex64),allocatable :: wave(:),spectre(:)
integer(int32) :: frame = 1024*16
real(real64) :: min_hz,max_hz
real(real64) :: dt

Station_Name = "MYG016"

! Please chane this path
file_path = "../Data/Earthquake/20220316/Knet/20220316233600.knt/"+Station_Name+"2203162336.EW"

accel_wave = f%import(format=KNT_ASCII,name=file_path)

call f%open(Station_Name+".txt","w")
call f%write(accel_wave)
call f%close()
call f%plot(option="with lines")

! FFT
wave = accel_wave(:,2)
spectre = fft(wave(1:frame) )

dt = (accel_wave(frame,1) - accel_wave(1,1))/dble(frame)*2
max_hz = 1.0d0/dt
min_hz = 1.0d0/dt/dble(frame)
hz = linspace([min_hz,max_hz],frame/2)
print *, dt, max_hz,min_hz

call f%open(Station_Name+"_FFT.txt","w")
call f%write(hz(1:frame/2),abs(spectre(1:frame/2) ) )
call f%close()
call f%plot(option="with lines; set logscale; replot")


end