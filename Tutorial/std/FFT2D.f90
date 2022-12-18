use MathClass
use ArrayClass
use IOClass
implicit none

integer(int32) :: i,j,N,h
real(real64),allocatable :: xy(:,:)
complex(real64),allocatable :: FFT_xy(:,:)
real(real64) :: r
type(IO_) :: f


N = 2**7
xy = zeros(N,N)
h = N/2
r = 3.0d0
do i=1,size(xy,1)
    do j=1,size(xy,2)
        if( sqrt( dble( (i-h) *(i-h)+(j-h)*(j-h)) ) < r )then
            xy(i,j) = 1.0d0
        endif
    enddo
enddo
FFT_xy = FFT(xy)

call f%open("FFT_2D.txt")
do i=1,size(FFT_xy,1)
    do j=1,size(FFT_xy,2)
        call f%write(dble(i),dble(j),abs(FFT_xy(i,j) ) )
    enddo
enddo
call f%close()
call f%splot()


end