program main

use SparseClass
use IOClass
use FEMDomainClass
implicit none

type(COO_) :: coo, Kmatrix_COO,Mmatrix_COO
type(CRS_) :: crs, Kmatrix,Mmatrix
real(real64),allocatable :: x(:)
complex(real64),allocatable :: u(:),u_p(:),u_m(:)
real(real64),allocatable :: Force(:),K_inv_F(:)
complex(real64) :: alpha
real(real64) :: K,dt,h,b_c_spring,damping_coeff
integer(int32) :: timestep
type(Math_) :: math
type(IO_) :: f
integer(int32) :: DOF
complex(real64) :: fix_disp
type(FEMDomain_),allocatable :: point(:)
integer(int32) :: i

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


DOF = 2



call coo%init(DOF)
call Kmatrix_COO%init(DOF)
call Mmatrix_COO%init(DOF)

! create mass-matrix
do i=1,DOF-1
    call Mmatrix_COO%add(i  , i  , 0.50d0)
    call Mmatrix_COO%add(i  , i+1, 0.50d0)
    call Mmatrix_COO%add(i+1, i  , 0.50d0)
    call Mmatrix_COO%add(i+1, i+1, 0.50d0)
enddo

! create stiffness-matrix
do i=1,DOF-1
    K = 9.0d0
    call Kmatrix_COO%add(i  , i  ,   K)
    call Kmatrix_COO%add(i  , i+1, - K)
    call Kmatrix_COO%add(i+1, i  , - K)
    call Kmatrix_COO%add(i+1, i+1,   K)
enddo

Mmatrix = Mmatrix_COO%to_CRS()
Kmatrix = Kmatrix_COO%to_CRS()

crs = Kmatrix%divide_by(diag_vector=Mmatrix%diag(cell_centered=.true.))


!do i=1,DOF
!    if(i<DOF/2)then
!        K = 100.0d0
!    else
!        K = 10.0d0
!    endif
!    call coo%add(i  , i  ,   K)
!    call coo%add(i  , i+1, - K)
!    call coo%add(i+1, i  , - K)
!    call coo%add(i+1, i+1,   K)
!enddo
!crs = coo%to_crs()


! u(+)
u_p = zeros(DOF)
u_p(DOF) = 0.50d0 
!u_p(DOF) = 20.d0 
! u(-) 
u_m = zeros(DOF)
u_m(DOF) = 0.50d0 
!u_m(DOF) = 20.d0 
! f 
force = zeros(DOF)
force(DOF) = 1.0d0


b_c_spring = 100000.0d0
damping_coeff = 100.0d0

u   = zeros(DOF)
u(DOF)=1.0d0


! parameters
dt = 1.0d0/10.0d0
h  = 0.010d0


if(h<1.0d0)then
    alpha = sqrt(sqrt(abs(1.0d0-h*h)))*math%i
else
    alpha = sqrt(h*h-1.0d0)
endif
call f%open("result.txt","w")

allocate(point(DOF))
do i=1,DOF
    call point(i)%create("Cube3D",x_num=2,y_num=2,z_num=2)
    call point(i)%resize(x=0.80d0,y=0.80d0,z=0.80d0)
    call point(i)%move(z=(i-1)*2.0d0)
enddo

!call Kmatrix%fix(idx=[1],RHS=Force,val=[0.0d0])
K_inv_F = 0.0d0*Force/K
!call Kmatrix%BiCGSTAB(x=K_inv_F,b=Force)

!print *, dt*(timestep-1), abs(u)
write(f%fh,*) dt*(0), dble(u),imag(u)
do timestep=1,1000
    !u(t) = exp(dt*(sqrt(h*h-1)-h ) )exp([w])u(+) + exp(dt*(-sqrt(h*h-1)-h ) )exp([w])u(-)
    !print *, dt*timestep, exp(dt*sqrt(dcmplx(h*h-1)) - dt*h )
    print *, timestep

    !Force(1) = -b_c_spring*dble(u_p(1)+u_m(1) ) - damping_coeff*dble(- u(1) + u_p(1) + u_m(1))/dt
    
    
    if(timestep>=100) then
        K_inv_F=0.0d0
    endif
    fix_disp = 0.0d0

    u_p = crs%tensor_exp_sqrt(v=u_p,&
        tol = dble(1.0e-10),coeff= dt*alpha - dt*h,&
        itrmax=100,fix_idx=[1],fix_val=[0.0d0*math%i])+K_inv_F/2.0d0
    u_m = crs%tensor_exp_sqrt(v=u_m,&
        tol = dble(1.0e-10),coeff=-dt*alpha - dt*h,&
        itrmax=100,fix_idx=[1],fix_val=[0.0d0*math%i])+K_inv_F/2.0d0
    
    ! post processing
    do i=1,DOF
        call point(i)%move(x=dble(- u(i) + u_p(i) + u_m(i)) )
    enddo
    u = u_p + u_m
    do i=1,DOF
        call point(i)%vtk("result_p"+str(i)+"_"+zfill(timestep,6) )
    enddo
    
    !print *, dt*(timestep-1), abs(u)
    write(f%fh,*) dt*(timestep), dble(u),imag(u)
    
enddo

call f%close()

! eigen frequency is
! 1/2*pi*(k/m)^0.5 = 1/2*pi*(9)^0.5 = 1.5/3.1415... = 0.47746... (Hz)
! T = 2.09439... sec


end program main