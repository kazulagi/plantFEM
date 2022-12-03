use DiffusionEquationClass
use SparseClass
implicit none

type(DiffusionEq_) :: solver
type(FEMDomain_) :: cube(1)
real(real64),allocatable :: c(:)
real(real64),allocatable :: DiffusionCoeff(:)
real(real64),allocatable :: Reaction(:)
real(real64),allocatable :: FixValue(:)
real(real64),allocatable :: C_n(:)
real(real64),allocatable :: dCdt(:)
integer(int32),allocatable :: FixBoundary(:)
integer(int32) :: itr,DomainID,NodeID,offset,timestep,penalty_order
logical :: passed
type(math_) :: math
real(real64) :: Length,dt,coeff,L2_error,penalty,tol
type(IO_)::f
type(COO_) :: coo
type(CRS_) :: crs,crs_div
type(Random_) :: random

! debug
!call coo%init(3)
!call coo%add(1,1,1.0d0)
!call coo%add(1,2,2.0d0)
!call coo%add(1,3,1.0d0)
!
!call coo%add(2,2,2.0d0)
!call coo%add(2,3,3.0d0)
!
!call coo%add(3,1,3.0d0)
!call coo%add(3,3,3.0d0)
!crs = coo%to_crs()
!!call print(crs%diag(cell_centered=.true.) )
!crs_div = crs%divide_by(crs%diag(cell_centered=.true.))
!call print(crs_div%to_dense() )
!stop


! EbO-FEM diffusion


Length = 1.0d0!10.0d0/(37.0d0)
dt     = 5.0e+0
coeff  = 1.0e-4
timestep = 100
tol    = 1.0e-20
penalty_order=0
penalty= 0.0d0

call cube(1)%create("Cube3D",x_num=40,y_num=3,z_num=3)



call cube(1)%resize(x=Length,y=Length/4,z=Length/4)

call cube(1)%move(x=0.0d0)

call cube(1)%vtk("cube_1")

FixBoundary = cube(1)%select(x_max=cube(1)%xmin()) &
    // cube(1)%select(x_min= cube(1)%xmax())
FixValue    = zeros(size(cube(1)%select(x_max=cube(1)%xmin()) ))&
    // zeros(size(cube(1)%select(x_min=cube(1)%xmax() )))

C_n   = zeros( cube(1)%nn() )
! initial condition
! c|{x=x,t=0} = sin(PI*x)
offset = 0
do domainID=1,size(cube)
    do nodeID=1,cube(domainID)%nn()
        
        C_n(nodeID+offset) = sin(math%PI*cube(DomainID)%position_x(nodeID) )
    enddo
    offset = offset + cube(domainID)%nn()
enddo


DiffusionCoeff = coeff*eyes( cube(1)%ne())



c = c_n

do itr=1,timestep

    Reaction &
        = zeros( cube(1)%ne())

    solver%solver%debug=True
    solver%solver%itrmax=1000
    solver%solver%relative_er =tol
    call solver%check_stability_condition(&
        dt=dt,&
        dx=abs(cube(1)%position_x(1)-cube(1)%position_x(2)),&
        coefficient=minval(DiffusionCoeff),&
        passed=passed )
    if(.not.passed)then
        !stop
    endif
    !stop
    !stop
    c = solver%getDiffusionField(&
        FEMDomains=cube, &
        DiffusionCoeff=DiffusionCoeff, &
        Reaction=Reaction, &
        Penalty=penalty,&
        FixBoundary=FixBoundary,   &
        FixValue=FixValue   ,    &!
        algorithm="TensorExponential",&!algorithm="RK4",&!algorithm="ForwardEuler",&
        C_n=C_n,&
        dt=dt &
        )
    C_n = c
    !
        call cube(1)%vtk(name="small_bar_1_"+zfill(itr,4),scalar=c( 1 : cube(1)%nn() ) )
        L2_error = 0.0d0
        offset = 0
        call f%open("result_and_analytical.txt","w")
        do domainID=1,size(cube)
            do nodeID=1,cube(domainID)%nn()
                call f%write(cube(DomainID)%position_x(nodeID),&
                 C(nodeID+offset),sin(math%PI*cube(DomainID)%position_x(nodeID) )&
                *exp(-coeff*math%PI*math%PI*dt*itr))
            
                L2_error = L2_error + abs(C(nodeID+offset) &
                    - sin(math%PI*cube(DomainID)%position_x(nodeID) )&
                    *exp(-coeff*math%PI*math%PI*dt*itr) &
                    )
            enddo
            offset = offset + cube(domainID)%nn()
        enddo
        call f%close()

enddo


!call f%open("result_and_analytical_"+str(penalty_order)+"_.txt","w")




L2_error=L2_error/size(C)
print *, L2_error

end