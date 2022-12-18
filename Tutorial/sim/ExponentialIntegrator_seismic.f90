program main

use SparseClass
use IOClass
implicit none

type(COO_) :: coo
type(CRS_) :: crs
real(real64),allocatable :: x(:)
complex(real64),allocatable :: u(:),u_p(:),u_m(:)
complex(real64) :: alpha
real(real64) :: M_inv_K,dt,h
integer(int32) :: timestep
type(Math_) :: math
type(IO_) :: f
integer(int32) :: DOF

! [M]{d^2 u/dt^2} + [C]{du/dt} + [K]{u} = {f}
! {d^2 u/dt^2} + [M]^{-1}[C]{du/dt} + [M]^{-1}[K]{u} = [M]^{-1}{f}
! [M]^{-1}[C] = 2*h*([M]^{-1}[K])^{1/2}であるとする．
! [w] = [M]^{-1}[K]とおくと
! u(t) = exp( -h + sqrt(h^2 - 1) )t[w]) a + exp( -h - sqrt(h^2 - 1) )t[w]) b 

! u(t+dt) = exp( -h + sqrt(h^2 - 1) )(t+dt)[w]) a 
!         + exp( -h - sqrt(h^2 - 1) )(t+dt)[w]) b 

! u(t+dt) = exp(( -h + sqrt(h^2 - 1) )(dt)[w])exp( -h + sqrt(h^2 - 1) )(t)[w]) a 
!         + exp(( -h - sqrt(h^2 - 1) )(dt)[w])exp( -h - sqrt(h^2 - 1) )(t)[w]) b 

! u(t)^{+} := exp( -h + sqrt(h^2 - 1) )(t)[w]) a  
! u(t)^{-} := exp( -h - sqrt(h^2 - 1) )(t)[w]) a  

! u(t+dt) = exp( -h + sqrt(h^2 - 1)(dt) ) exp([w])u(t)^{+}
!         + exp( -h - sqrt(h^2 - 1)(dt) ) exp([w])u(t)^{-}


DOF = 2
M_inv_K = 100.0d0

call coo%init(DOF)
do i_i=1,DOF
    call coo%add(i_i,i_i-1,-M_inv_K/i_i)
    call coo%add(i_i,i_i  , M_inv_K/i_i)
    call coo%add(i_i,i_i+1,-M_inv_K/i_i)
enddo
crs = coo%to_crs()


! u(+)
u_p = zeros(DOF)
u_p(1) = 1.0d0 
u_p(2) = 0.50d0 
! u(-) 
u_m = zeros(DOF)
u_m(1) = 1.0d0
u_m(2) = 0.50d0 

u   = zeros(DOF)

! parameters
dt = 1.0d0/10.0d0
h  = 0.10d0


if(h<1.0d0)then
    alpha = sqrt(sqrt(abs(1.0d0-h*h)))*math%i
else
    alpha = sqrt(h*h-1.0d0)
endif
call f%open("result.txt","w")

do timestep=1,1000
    !u(t) = exp(dt*(sqrt(h*h-1)-h ) )exp([w])u(+) + exp(dt*(-sqrt(h*h-1)-h ) )exp([w])u(-)
    !print *, dt*timestep, exp(dt*sqrt(dcmplx(h*h-1)) - dt*h )
    
    u_p = crs%tensor_exp_sqrt(u_p,tol = dble(1.0e-10),coeff=( dt*alpha - dt*h),itrmax=100)
    u_m = crs%tensor_exp_sqrt(u_m,tol = dble(1.0e-10),coeff=(-dt*alpha - dt*h),itrmax=100)
    u = u_p + u_m

    !u_p = exp( ( dt*alpha - dt*h)*sqrt(M_inv_K) )*u_p
    !u_m = exp( (-dt*alpha - dt*h)*sqrt(M_inv_K) )*u_m
    !u = u_p + u_m


    print *, dt*(timestep-1), abs(u)
    write(f%fh,*) dt*(timestep-1), dble(u),imag(u)
    !print *, dt*(timestep-1), exp( dt*alpha - dt*h )
enddo

call f%close()
call f%plot(option=" with lines;")


contains



end program main