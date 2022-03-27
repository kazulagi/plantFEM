use plantfem
implicit none

type(IO_) :: f
character(:),allocatable :: file_path
real(real64),allocatable :: accel_wave(:,:)

file_path = "../Data/Earthquake/20070716/Knet/NIG0190707161013.EW" 

accel_wave = f%import(format=KNT_ASCII,name=file_path)

call f%plot(x=accel_wave(:,1),Fx=accel_wave(:,2),option="with lines" )

end