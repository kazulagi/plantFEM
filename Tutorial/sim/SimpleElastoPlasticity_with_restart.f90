use IOClass
use ElastoPlasticityClass
use CivilItemClass
implicit none

type(ElastoPlasticity_) :: ep
type(FEMDomain_) :: cube
integer(int32),allocatable :: fix_nodes_x(:)
integer(int32),allocatable :: fix_nodes_y(:)
integer(int32),allocatable :: fix_nodes_z(:)
real(real64),allocatable :: fix_value_x(:)
real(real64),allocatable :: fix_value_y(:)
real(real64),allocatable :: fix_value_z(:)

real(real64),allocatable :: YoungModulus(:)
real(real64) :: PoissonRatio
real(real64) :: Density
real(real64) :: cohesion
real(real64) :: FrictionAngle
real(real64) :: DilatancyAngle
real(real64) :: z_disp
type(Random_) :: random
type(CivilItem_) :: ci
type(IO_) :: file
integer(int32) :: upd_mode
real(real64) :: load_displacement
integer(int32) :: step
real(real64),allocatable :: a(:,:,:),b(:,:,:),c(:,:)

call cube%create("Cylinder3D",x_num=8,y_num=8,z_num=16)
call cube%resize(x=0.020d0,y=0.020d0,z=0.040d0)
call cube%vtk("init")

! fix bottom
fix_nodes_x = cube%getNodeList(zmax=cube%zmin() )
fix_nodes_y = cube%getNodeList(zmax=cube%zmin() )
fix_nodes_z = cube%getNodeList(zmax=cube%zmin() )
fix_value_x = zeros(size(fix_nodes_x))
fix_value_y = zeros(size(fix_nodes_y))
fix_value_z = zeros(size(fix_nodes_z))


z_disp = 0.00000040d0
!fix_nodes_x = fix_nodes_x // cube%getNodeList(zmin=cube%zmax() )
!fix_value_x = fix_value_x // zeros(size(cube%getNodeList(zmin=cube%zmax() )))
!!
!fix_nodes_y = fix_nodes_y // cube%getNodeList(zmin=cube%zmax() )
!fix_value_y = fix_value_y // zeros(size(cube%getNodeList(zmin=cube%zmax() )))

fix_nodes_z = fix_nodes_z // cube%getNodeList(zmin=cube%zmax() )
fix_value_z = fix_value_z // - z_disp*eyes(size(cube%getNodeList(zmin=cube%zmax() )))

YoungModulus   = random%gauss(mu=100.0d0*1000.0d0,sigma=0.0d0,n=cube%ne() )
PoissonRatio   = 0.30d0
Density        = 1.80d0
cohesion       = 3.0d0
FrictionAngle  = radian(30.0d0)
DilatancyAngle = radian(0.0d0)

call ep%init(&
        femdomains=[cube],&
        default_YieldFunction        = MohrCoulomb,&
        default_YieldFunction_params = [Cohesion, FrictionAngle],&
        default_PlasticPotential        = DruckerPrager, &
        default_PlasticPotential_params = [Cohesion, DilatancyAngle] &
    )



! overwrite cohesion for yield function
!ep%ep_domain(1)%YieldFunction_params(:,1) = &
!    random%gauss(mu=3.0d0        ,sigma=0.0010d0,n=cube%ne() )
! overwrite friction angle for yield function
!ep%ep_domain(1)%YieldFunction_params(:,2) = &
!    random%gauss(mu=radian(20.0d0),sigma=radian(2.0d0),n=cube%ne() )
!
! overwrite cohesion for plastic potential
!ep%ep_domain(1)%PlasticPotential_params(:,1) = &
!    random%gauss(mu=0.0d0        ,sigma=5.0d0,n=cube%ne() )

ep%tol = 1.0e-7

call file%open("Force_curve.txt","w")
load_displacement = 0.0d0
step = 0

do i_i=1,15
    step = step + 1
    load_displacement = load_displacement + z_disp
    call ep%solve_increment(&
            delta_Density = 0.0d0*eyes(cube%ne() )        ,& ! material info
            YoungModulus=YoungModulus,& ! material info
            PoissonRatio=PoissonRatio*eyes(cube%ne() ),& ! material info
            fix_node_list_x=fix_nodes_x, & ! boundary conditions
            fix_node_list_y=fix_nodes_y, & ! boundary conditions
            fix_node_list_z=fix_nodes_z, & ! boundary conditions
            fix_value_list_x=fix_value_x,& ! boundary conditions
            fix_value_list_y=fix_value_y,& ! boundary conditions
            fix_value_list_z=fix_value_z & ! boundary conditions
        )
        print *, load_displacement,ep%getTractionForce(NodeList=cube%getNodeList(zmin=cube%zmax() ))
        write(file%fh,*) load_displacement,ep%getTractionForce(NodeList=cube%getNodeList(zmin=cube%zmax() ))
        call file%flush()
    call ep%export(name="UT_first",step=step,amp=10.0d0)
enddo

call ep%importField(name="UT_first",num_domain=1)

do i_i=1,15
    load_displacement = load_displacement - z_disp
    step = step + 1
    call ep%solve_increment(&
            delta_Density = 0.0d0*eyes(cube%ne() )        ,& ! material info
            YoungModulus=YoungModulus,& ! material info
            PoissonRatio=PoissonRatio*eyes(cube%ne() ),& ! material info
            fix_node_list_x=fix_nodes_x, & ! boundary conditions
            fix_node_list_y=fix_nodes_y, & ! boundary conditions
            fix_node_list_z=fix_nodes_z, & ! boundary conditions
            fix_value_list_x=fix_value_x,& ! boundary conditions
            fix_value_list_y=fix_value_y,& ! boundary conditions
            fix_value_list_z=-fix_value_z & ! boundary conditions
        )
        print *, load_displacement,ep%getTractionForce(NodeList=cube%getNodeList(zmin=cube%zmax() ))
        write(file%fh,*) load_displacement,ep%getTractionForce(NodeList=cube%getNodeList(zmin=cube%zmax() ))
        call file%flush()
    call ep%export(name="UT_second",step=step,amp=10.0d0)
enddo

end