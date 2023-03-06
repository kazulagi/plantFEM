use WaveKernelClass
use LoggerClass
implicit none

type(WaveKernel_) :: WK
type(FEMDomain_)  :: domain
type(Logger_)     :: logger(2)
type(Random_)     :: random
real(real64),allocatable :: YoungModulus(:)
real(real64),allocatable :: PoissonRatio(:)
real(real64),allocatable :: Density(:)
real(real64),allocatable :: u(:),u_n(:),v(:),v_n(:)
real(real64) :: dt
integer(int32),allocatable :: fix_boundary(:),input_boundary(:),log_point(:)
integer(int32) :: i,timestep
real(real64) :: Vs, cutoff_frequency,buf1,buf2
type(Time_) :: time


call domain%create("Cube3D",x_num=100,y_num=100,z_num=30)
call domain%resize(x=100.0d0,y=100.0d0,z=30.0d0)
call domain%move(z=-30.0d0)
call domain%vtk("domain.vtk")

Vs = 200.0d0
PoissonRatio = 0.30d0*ones( domain%ne() )
Density      = 1.80d0*ones( domain%ne() )
YoungModulus = Vs*Vs*Density(:)*(ones(domain%ne()) + PoissonRatio )*2.0d0


call WK%init(FEMDomain=domain,&
    YoungModulus=YoungModulus,&
    PoissonRatio=PoissonRatio,&
    Density=Density,&
    DOF=3)

WK%itrmax = 100
WK%tol    = dble(1.0e-25)
dt = 1.0d0/100.0d0
cutoff_frequency = 1.0d0/dt*5.0d0

fix_boundary   = domain%select(z_max=domain%z_min() )*3-2
fix_boundary   = fix_boundary // domain%select(z_max=domain%z_min() )*3-1
fix_boundary   = fix_boundary // domain%select(z_max=domain%z_min() )*3-0

input_boundary = domain%select(x_max=2.0d0,y_max=2.0d0,z_min=domain%z_max() )*3 



u   = zeros(domain%nn()*3)
u_n = zeros(domain%nn()*3)
v   = zeros(domain%nn()*3)
v_n = zeros(domain%nn()*3)

call logger(1)%set(femdomain=domain,position=[50.0d0,50.0d0,-0.10d0],dataset=v,name="L1_V")
call logger(1)%vtk("logger")
call logger(1)%save(t=0.0d0)


call logger(2)%set(femdomain=domain,position=[5.0d0,5.0d0,-0.10d0],dataset=v,name="L2_V")
call logger(2)%vtk("logger")
call logger(2)%save(t=0.0d0)


!v_n(input_boundary) = v_n(input_boundary) + random%gauss(mu=0.0d0,sigma=1.0d0/1000.0d0)        
v_n(input_boundary) = v_n(input_boundary) + 1.0d0/1000.0d0
print *, ">>> analysis started >>> "
do timestep=1,10000
    call time%start()
    u = WK%getDisplacement(u_n=u_n, v_n=v_n,&
        dt=dt,fix_idx=fix_boundary,cutoff_frequency=cutoff_frequency)
    v = WK%getVelocity(u_n=u_n, v_n=v_n,&
        dt=dt,fix_idx=fix_boundary,cutoff_frequency=cutoff_frequency)
    call time%show()
    
    print *, norm(u),norm(v) 

    call time%start()
    call WK%getDisplacement_and_Velocity(&
        u_n=u_n,v_n=v_n,dt=dt,fix_idx=fix_boundary,&
        cutoff_frequency=cutoff_frequency,&
        u=u,v=v)
    call time%show()
    print *, norm(u),norm(v)
    stop
    
    call logger(1)%save(t=dt*timestep)
    call logger(2)%save(t=dt*timestep)
    !call logger(3)%save(t=dt*timestep)
    u_n = u
    v_n = v
    print *, dt*timestep
enddo

end