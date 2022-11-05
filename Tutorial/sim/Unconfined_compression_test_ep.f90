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

call cube%create("Cylinder3D",x_num=7,y_num=7,z_num=15)
call cube%resize(x=0.020d0,y=0.020d0,z=0.0550d0)
call cube%vtk("init")

! fix bottom
fix_nodes_x = cube%getNodeList(zmax=cube%zmin() )
fix_nodes_y = cube%getNodeList(zmax=cube%zmin() )
fix_nodes_z = cube%getNodeList(zmax=cube%zmin() )
fix_value_x = zeros(size(fix_nodes_x))
fix_value_y = zeros(size(fix_nodes_y))
fix_value_z = zeros(size(fix_nodes_z))

z_disp = 0.00000020d0
fix_nodes_x = fix_nodes_x // cube%getNodeList(zmin=cube%zmax() )
fix_value_x = fix_value_x // zeros(size(cube%getNodeList(zmin=cube%zmax() )))
!
fix_nodes_y = fix_nodes_y // cube%getNodeList(zmin=cube%zmax() )
fix_value_y = fix_value_y // zeros(size(cube%getNodeList(zmin=cube%zmax() )))

fix_nodes_z = fix_nodes_z // cube%getNodeList(zmin=cube%zmax() )
fix_value_z = fix_value_z // - z_disp*eyes(size(cube%getNodeList(zmin=cube%zmax() )))

YoungModulus   = random%gauss(mu=100.0d0*1000.0d0,sigma=0.0d0,n=cube%ne() )
PoissonRatio   = 0.30d0
Density        = 1.80d0
cohesion       = 1.0d0
FrictionAngle  = radian(30.0d0)
DilatancyAngle = radian(0.0d0)

call ep%init(&
        femdomains=[cube],&
        default_YieldFunction        = MohrCoulomb,&
        default_YieldFunction_params = [Cohesion, FrictionAngle],&
        default_PlasticPotential        = DruckerPrager, &
        default_PlasticPotential_params = [Cohesion, DilatancyAngle] &
    )


ep%tol = 1.0e-6
! overwrite cohesion for yield function
ep%ep_domain(1)%YieldFunction_params(:,1) = &
    random%gauss(mu=3.0d0        ,sigma=0.10d0,n=cube%ne() )
! overwrite friction angle for yield function
!ep%ep_domain(1)%YieldFunction_params(:,2) = &
!    random%gauss(mu=radian(20.0d0),sigma=radian(2.0d0),n=cube%ne() )
!
! overwrite cohesion for plastic potential
!ep%ep_domain(1)%PlasticPotential_params(:,1) = &
!    random%gauss(mu=0.0d0        ,sigma=5.0d0,n=cube%ne() )
call file%open("Force_curve.txt","w")
write(file%fh,*) z_disp*(i_i),zeros(4)
do i_i=1,200
    call ep%solve(&
            Density = Density*eyes(cube%ne() )        ,& ! material info
            YoungModulus=YoungModulus,& ! material info
            PoissonRatio=PoissonRatio*eyes(cube%ne() ),& ! material info
            fix_node_list_x=fix_nodes_x, & ! boundary conditions
            fix_node_list_y=fix_nodes_y, & ! boundary conditions
            fix_node_list_z=fix_nodes_z, & ! boundary conditions
            fix_value_list_x=fix_value_x,& ! boundary conditions
            fix_value_list_y=fix_value_y,& ! boundary conditions
            fix_value_list_z=i_i*fix_value_z & ! boundary conditions
        )
        print *, z_disp*(i_i),ep%getTractionForce(NodeList=cube%getNodeList(zmin=cube%zmax() ))
        write(file%fh,*) z_disp*(i_i),ep%getTractionForce(NodeList=cube%getNodeList(zmin=cube%zmax() ))
        call file%flush()
    call ep%export(name="UT",step=i_i,amp=10.0d0)
    call ep%reset()
enddo

call file%close()
end