use ArrayClass
use IOClass
implicit none

real(real64) :: lx,ly
real(real64),allocatable :: space(:,:),func(:)
type(IO_) :: f

! f(x,y) = 1 if |x|<lx, |y|<ly
! f(x,y) = 0 otherwise

! Visualize F(wx,wy) : 2D Fourier Spectre of f(x,y) 

lx = 1.0d0
ly = 1.0d0

space = linspace(            &
    xrange=[-10.0d0,10.0d0], &
    yrange=[-10.0d0,10.0d0], &
    xnum = 100,              &
    ynum = 100               &
    )

func = zeros(size(space,1) )
func = 4.0d0*sin(lx*space(:,1) )/space(:,1)*sin(ly*space(:,2) )/space(:,2)

call f%open("2DFourierSpectre.txt","w")
call f%write(space(:,1),space(:,2),func(:) )
call f%close()
call f%splot(option="with lines")

end