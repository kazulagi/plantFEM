use ArrayClass
use IOClass

real(real64),allocatable :: x(:), fx(:),dfdx(:),I_f_dx(:)
type(IO_) :: f

x = linspace([0.0d0,20.0d0],10000)
fx = zeros(10000)
fx = sin(x)
dfdx = d_dx(x=x,fx=fx)
I_f_dx = I_dx(x=x,fx=fx)

call f%open("fx.txt")
call f%write(x,fx)
call f%close()
call f%plot()


call f%open("dfdx.txt")
call f%write(x,dfdx)
call f%close()
call f%plot()


call f%open("I_f_dx.txt")
call f%write(x,I_f_dx)
call f%close()
call f%plot()





end