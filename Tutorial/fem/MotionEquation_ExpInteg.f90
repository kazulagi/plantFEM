program main

use SparseClass
use IOClass
use FEMDomainClass
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
complex(real64) :: fix_disp
type(FEMDomain_) :: point(3)

! Motion Equation and Exponential integrator

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


DOF = 3
M_inv_K = 100.0d0

call coo%init(DOF)

call coo%add(1,1,   M_inv_K)
call coo%add(1,2, - M_inv_K)

call coo%add(2,1, - M_inv_K)
call coo%add(2,2, 2*M_inv_K)
call coo%add(2,3, - M_inv_K)

call coo%add(3,2, - M_inv_K)
call coo%add(3,3,   M_inv_K)
crs = coo%to_crs()


! u(+)
u_p = zeros(DOF)
u_p(1) = 0.0d0 
u_p(2) = -1.0d0 
u_p(3) = 1.5d0 
! u(-) 
u_m = zeros(DOF)
u_m(1) = 0.0d0 
u_m(2) = -1.0d0 
u_m(3) = 1.5d0 

u   = zeros(DOF)


! parameters
dt = 1.0d0/1.0d0
h  = 0.02d0


if(h<1.0d0)then
    alpha = sqrt(sqrt(abs(1.0d0-h*h)))*math%i
else
    alpha = sqrt(h*h-1.0d0)
endif
call f%open("result.txt","w")

call point(1)%create("Sphere3D")
call point(2)%create("Sphere3D")
call point(3)%create("Sphere3D")
call point(1)%resize(x=0.20d0,y=0.20d0,z=0.20d0)
call point(2)%resize(x=0.20d0,y=0.20d0,z=0.20d0)
call point(3)%resize(x=0.20d0,y=0.20d0,z=0.20d0)
call point(1)%move(z=0.0d0)
call point(2)%move(z=2.0d0)
call point(3)%move(z=4.0d0)

do timestep=1,100000
    !u(t) = exp(dt*(sqrt(h*h-1)-h ) )exp([w])u(+) + exp(dt*(-sqrt(h*h-1)-h ) )exp([w])u(-)
    !print *, dt*timestep, exp(dt*sqrt(dcmplx(h*h-1)) - dt*h )
    
    if(1000 < timestep .and. timestep <= 5000)then
        fix_disp= 0.10d0
    else
        fix_disp=0.0d0
    endif
    u_p = crs%tensor_exp_sqrt(u_p,&
        tol = dble(1.0e-10),coeff=( dt*alpha - dt*h),itrmax=100,&
        fix_idx=[1],fix_val=[fix_disp])
    u_m = crs%tensor_exp_sqrt(u_m,&
        tol = dble(1.0e-10),coeff=(-dt*alpha - dt*h),itrmax=100,&
        fix_idx=[1],fix_val=[fix_disp])
    
    call point(1)%move(x=dble(- u(1) + u_p(1) + u_m(1)) )
    call point(2)%move(x=dble(- u(2) + u_p(2) + u_m(2)) )
    call point(3)%move(x=dble(- u(3) + u_p(3) + u_m(3)) )
    u = u_p + u_m

    call point(1)%vtk("result_p1_"+zfill(timestep,6) )
    call point(2)%vtk("result_p2_"+zfill(timestep,6) )
    call point(3)%vtk("result_p3_"+zfill(timestep,6) )

    !u_p = exp( ( dt*alpha - dt*h)*sqrt(M_inv_K) )*u_p
    !u_m = exp( (-dt*alpha - dt*h)*sqrt(M_inv_K) )*u_m
    !u = u_p + u_m


    print *, dt*(timestep-1), abs(u)
    write(f%fh,*) dt*(timestep-1), dble(u),imag(u)
    !print *, dt*(timestep-1), exp( dt*alpha - dt*h )
enddo

call f%close()


contains



end program main