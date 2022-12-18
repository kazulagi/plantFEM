program main
use FEMDomainClass
use DiffusionEquationClass
use SceneClass
use FactoryClass
implicit none

type(DiffusionEq_) :: solver
type(FEMDomain_) :: cube(4)
real(real64),allocatable :: c(:)
real(real64),allocatable :: DiffusionCoeff(:)
real(real64),allocatable :: Reaction(:)
real(real64),allocatable :: FixValue(:)
real(real64),allocatable :: C_n(:),x(:)
integer(int32),allocatable :: FixBoundary(:)
integer(int32) :: itr,offset,max_itr
real(real64) :: Length,epsi,dt,penalty,d,AL_lambda, AL_tol, AL_alpha
type(IO_) :: f
logical :: passed
type(Scene_) :: scene
type(Factory_) :: factory

type(COO_) :: coo
type(CRS_) :: crs

real(real64) :: test(2,3)

!call coo%init(3)
!call coo%add(1,1,1.0d0)
!call coo%add(1,2,2.0d0)
!
!call coo%add(2,1,2.0d0)
!call coo%add(2,2,1.0d0)
!call coo%add(2,3,2.0d0)
!
!call coo%add(2,3,2.0d0)
!call coo%add(3,3,1.0d0)
!crs = coo%to_crs()
!
!call crs%fix([3])
!
!call print(crs%to_dense())
!stop

! diffusion_benchmark_EbO_Scene.f90

Length=1.0d0
epsi=0.010d0
max_itr = 1000
dt = 0.030d0 
penalty = 10.0d0**(-1)
!penalty = 10.0d0**(2)
d       = 1.0e-2

AL_lambda= 0.00d0
AL_tol   = dble(5.0e-9)
AL_alpha =  3.00d0

! penaltyを上げるとgapが閉じずに発散する
cube = factory%cube([30,5,5],n=4)
call scene%add(cube)

call scene%resize(object=[1,2,3,4], &
    x=(Length+3*epsi)/4.0d0 ,y=Length/10.0d0,z=Length/10.0d0) 
    

call scene%move(object=[1], x=0.0d0)
call scene%move(object=[2], x=cube(1)%xmax()-epsi)
call scene%move(object=[3], x=cube(2)%xmax()-epsi)
call scene%move(object=[4], x=cube(3)%xmax()-epsi)

call scene%vtk("cube")
call scene%overset()

FixBoundary = scene%select_point_ID([1],x_max=scene%xmin([1]) ) &
    // scene%select_point_ID([4],x_min=scene%xmax([4]) )

FixValue = scene%full([1],values=0.0d0,x_max=scene%xmin([1]) ) &
    // scene%full([4],values=0.0d0,x_min=scene%xmax([4]) )

DiffusionCoeff = d*eyes(scene%nn() )
C_n = scene%full(func=sin_function)
call scene%vtk("init",scalar=C_n)


call f%open("init.csv","w")
x = scene%x()
do i_i=1,size(scene%x())
    write(f%fh,*) x(i_i), sin_function(x=[x(i_i)])  
enddo
call f%close()

call f%open("analytical.csv","w")
x = scene%x()
do i_i=1,size(scene%x())
    write(f%fh,*) x(i_i), analytical_solution(  &   
    x=[x(i_i), 0.0d0], &
    params=[DiffusionCoeff(1)]  )  
    
enddo
call f%close()


c = c_n

call solver%check_stability_condition(dt=dt,dx=Length/80,coefficient=dble(1.0e-4),passed=passed)
    if(.not.passed)stop

do itr=1,max_itr

    Reaction = zeros( scene%nn() )

    solver%solver%debug=True
    solver%solver%itrmax=100000
    
!    AL
    c = solver%getDiffusionField(&
        FEMDomains=cube, &
        DiffusionCoeff=DiffusionCoeff, &
        Reaction=Reaction, &
        Penalty=penalty ,&
        FixBoundary=FixBoundary, &
        FixValue=FixValue   ,    &
        C_n=C_n,&
        AL_lambda=AL_lambda,&
        AL_tol=AL_tol,&
        AL_alpha=AL_alpha,&
        dt=dt &
        )

!    c = solver%getDiffusionField(&
!        FEMDomains=cube, &
!        DiffusionCoeff=DiffusionCoeff, &
!        Reaction=Reaction, &
!        Penalty=penalty ,&
!        FixBoundary=FixBoundary, &
!        FixValue=FixValue   ,    &
!        C_n=C_n,&
!        dt=dt &
!        )
    C_n = c


    call f%open("result.csv","w")
    call f%write(scene%x(),c(:) )
    call f%close()

    call f%cp("result.csv","result"+str(penalty)+".csv")


    call f%open("analytical.csv","w")
    x = scene%x()
    do i_i=1,size(scene%x())
        call f%write(x(i_i), analytical_solution(  &   
            x=[x(i_i), dt*itr], &
            params=[DiffusionCoeff(1)]  )  )
    enddo
    call f%close()
    
enddo




contains

function sin_function(x,params) result(ret)
    real(real64),intent(in) :: x(:)
    real(real64),optional,intent(in) :: params(:)
    real(real64) :: ret
    type(Math_) :: math

    ret = sin(math%pi*x(1))

end function

function analytical_solution(x,params) result(ret)
    real(real64),intent(in) :: x(:)
    real(real64),optional,intent(in) :: params(:)
    real(real64) :: ret
    type(Math_) :: math

    ! x1 = x
    ! x2 = t
    ret = sin(math%PI*x(1) )&
                    *exp(-params(1)*math%PI*math%PI*x(2) )

end function


end program