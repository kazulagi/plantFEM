use ArrayClass
use IOClass
use MathClass
implicit none

complex(complex64),allocatable :: Ft(:),Fw(:),Ft_(:)
complex(complex64),allocatable :: RW(:),FRW(:)
integer(int64) :: datasize
type(IO_) ::f

datasize = 2**10
call print("Data-Size is :: ")
call print(datasize)

Ft = zeros(datasize)
Ft(datasize/2-1:datasize/2+1) = 1.0d0

Fw = FFT(Ft)
Ft_ = IFFT(Fw)

call f%open("Ft.txt")
call f%write( abs(Ft) )
call f%close()
call f%plot(option="with lines")

call f%open("Fw.txt")
call f%write( abs(Fw(:datasize) ) )
call f%close()
call f%plot(option="with lines")

RW = HammingWindow(int(datasize)/2, int(datasize))
Fw = windowing(RW,Fw)
Ft_ = IFFT(Fw)
call f%open("RectangularWindow.txt")
call f%write( abs(Ft_(:datasize) ) )
call f%close()
call f%plot(option="with lines")




end