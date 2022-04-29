use FEMSolverClass
implicit none


type(FEMDomain_) :: cube
type(FEMSolver_) :: solver
integer(int32),allocatable   :: FixBoundary(:)
integer(int32)   :: i 
real(real64),allocatable :: displ(:),sigma(:,:),tr_sigma(:)
type(IO_) :: f

call cube%create("Cube3D",x_num=10,y_num=10,z_num=100)
call cube%resize(z=10.0d0)
call solver%init(NumDomain=1)
call solver%setDomain(FEMDomain=cube,DomainID=1)
call solver%setCRS(DOF=3)

!$OMP parallel 
!$OMP do
do i = 1, cube%ne()
    call solver%setMatrix(DomainID=1,ElementID=i,DOF=3,&
       Matrix=cube%StiffnessMatrix(ElementID=i,E=21700000.0d0, v=0.000d0) )
    call solver%setVector(DomainID=1,ElementID=i,DOF=3,&
        Vector=cube%MassVector(&
        ElementID=i,&
        DOF=cube%nd() ,&
        Density=2.400d0,&
        Accel=[0.0d0, 0.0d0, -9.80d0]&
        ) &    
    )
enddo
!$OMP end do
!$OMP end parallel

print *, "matrices imported."
! disp. boundary
FixBoundary = cube%select(z_max = cube%z_min() )*3-2
call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)
FixBoundary = cube%select(z_max = cube%z_min() )*3-1
call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)
FixBoundary = cube%select(z_max = cube%z_min() )*3-0
call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)
!FixBoundary = cube%select(x_min = cube%x_max() )*3-0
!call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=-0.30d0)

print *, "b.c. imported."

call cube%vtk("cube_0")

! solve
solver%debug = .true.

displ = solver%solve()
call cube%deform(disp=displ )

tr_sigma = cube%getElementCauchyStress(option="I1",&
    displacement=displ,&
    E=[21700000.0d0],v=[0.00d0])

call cube%vtk("cube_I1",&
        scalar=tr_sigma)
call print(tr_sigma(1) )
! theoretical value:
! stress@ bottom element:
        !-234.024 kPa
! simulation:
        !-234.026 kPs


end