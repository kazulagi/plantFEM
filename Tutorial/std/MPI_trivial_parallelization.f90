use MPIClass

implicit none

type(MPI_) :: mpid
real(real64) :: x, y, z

call mpid%start()
! MPI-distributed parameters (x, y)
call mpid%EP_set_variable(var=x,var_range=[-30.0d0,30.0d0],N=30)
call mpid%EP_set_variable(var=y,var_range=[-30.0d0,30.0d0],N=30)

do
    if( mpid%EP_get_variable())then
        ! compute z for each (x,y)
        z = x**2 + y**2 - 2*y + x
        ! save results
        call mpid%EP_set_result([z])
    else
        exit
    endif
enddo

call mpid%end()


end