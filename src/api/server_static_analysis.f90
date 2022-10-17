
use FEMDomainClass
use FEMSolverClass
implicit none

! modal analysis

type(FEMDomain_) :: object
type(FEMSolver_) :: solver
type(IO_) :: f
character(256) :: fpath
real(real64) :: ground_level, YoungModulus, PoissonRatio, Density, amp,&
    fix_boundary_xmin,fix_boundary_xmax,fix_boundary_ymin,fix_boundary_ymax
real(real64),allocatable :: displ(:),sigma(:,:),tr_sigma(:),&
    sigma_11(:),sigma_12(:),sigma_13(:),sigma_22(:),sigma_23(:),sigma_33(:)
integer(int32),allocatable :: fix_nodes_x(:),fix_nodes_y(:),fix_nodes_z(:)
integer(int32) :: i

call get_command_argument(number=1,value=fpath)

! read mesh file
call object%read(trim(adjustl(fpath) ) )
! read condition file
call f%open(trim(adjustl(fpath)) + ".condition","r")
read(f%fh,*) YoungModulus
read(f%fh,*) PoissonRatio
read(f%fh,*) Density
read(f%fh,*) ground_level
read(f%fh,*) fix_boundary_xmin
read(f%fh,*) fix_boundary_xmax
read(f%fh,*) fix_boundary_ymin
read(f%fh,*) fix_boundary_ymax

call f%close()

fix_nodes_x = object%getNodeList(zmax=ground_level)
fix_nodes_y = object%getNodeList(zmax=ground_level)
fix_nodes_z = object%getNodeList(zmax=ground_level)

fix_nodes_x = fix_nodes_x // object%getNodeList(xmax=fix_boundary_xmin)
fix_nodes_y = fix_nodes_y // object%getNodeList(xmax=fix_boundary_xmin)
fix_nodes_z = fix_nodes_z // object%getNodeList(xmax=fix_boundary_xmin)

fix_nodes_x = fix_nodes_x // object%getNodeList(xmin=fix_boundary_xmax)
fix_nodes_y = fix_nodes_y // object%getNodeList(xmin=fix_boundary_xmax)
fix_nodes_z = fix_nodes_z // object%getNodeList(xmin=fix_boundary_xmax)

fix_nodes_x = fix_nodes_x // object%getNodeList(ymax=fix_boundary_ymin)
fix_nodes_y = fix_nodes_y // object%getNodeList(ymax=fix_boundary_ymin)
fix_nodes_z = fix_nodes_z // object%getNodeList(ymax=fix_boundary_ymin)

fix_nodes_x = fix_nodes_x // object%getNodeList(ymin=fix_boundary_ymax)
fix_nodes_y = fix_nodes_y // object%getNodeList(ymin=fix_boundary_ymax)
fix_nodes_z = fix_nodes_z // object%getNodeList(ymin=fix_boundary_ymax)


call solver%init(NumDomain=1)
call solver%setDomain(FEMDomain=object,DomainID=1)
call solver%setCRS(DOF=3)


!$OMP parallel 
!$OMP do
do i = 1, object%ne()
    call solver%setMatrix(DomainID=1,ElementID=i,DOF=3,&
       Matrix=object%StiffnessMatrix(ElementID=i,E=YoungModulus, v=PoissonRatio) )
    call solver%setVector(DomainID=1,ElementID=i,DOF=3,&
        Vector=object%MassVector(&
        ElementID=i,&
        DOF=object%nd() ,&
        Density=Density,&
        Accel=[0.0d0, 0.0d0, -9.80d0]&
        ) &    
    )
enddo
!$OMP end do
!$OMP end parallel

call solver%fix(DomainID=1,IDs=fix_nodes_x*3-2,FixValue=0.0d0)
call solver%fix(DomainID=1,IDs=fix_nodes_y*3-1,FixValue=0.0d0)
call solver%fix(DomainID=1,IDs=fix_nodes_z*3-0,FixValue=0.0d0)

displ = solver%solve()

tr_sigma = zeros(object%ne())
sigma_11 = zeros(object%ne())
sigma_12 = zeros(object%ne())
sigma_13 = zeros(object%ne())
sigma_22 = zeros(object%ne())
sigma_23 = zeros(object%ne())
sigma_33 = zeros(object%ne())
do i_i=1,object%ne()
    sigma = zeros(3,3)
    sigma = object%stressMatrix(ElementID=i_i,&
        disp=reshape(displ,object%nn(),object%nd() ),&
        E=YoungModulus, v=PoissonRatio)
    sigma_11(i_i) = sigma(1,1)
    sigma_12(i_i) = sigma(1,2)
    sigma_13(i_i) = sigma(1,3)
    sigma_22(i_i) = sigma(2,2)
    sigma_23(i_i) = sigma(2,3)
    sigma_33(i_i) = sigma(3,3)
    tr_sigma(i_i) = trace(sigma)/3.0d0
enddo


call object%deform(disp=displ)
call object%vtk("static_I1_"//trim(adjustl(fpath) ),scalar=tr_sigma)
call object%vtk("static_11_"//trim(adjustl(fpath) ),scalar=sigma_11)
call object%vtk("static_12_"//trim(adjustl(fpath) ),scalar=sigma_12)
call object%vtk("static_13_"//trim(adjustl(fpath) ),scalar=sigma_13)
call object%vtk("static_22_"//trim(adjustl(fpath) ),scalar=sigma_22)
call object%vtk("static_23_"//trim(adjustl(fpath) ),scalar=sigma_23)
call object%vtk("static_33_"//trim(adjustl(fpath) ),scalar=sigma_33)

end