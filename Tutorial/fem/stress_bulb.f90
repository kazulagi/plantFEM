use FEMSolverClass
implicit none

! Stress bulb

type(FEMDomain_) :: cube
type(FEMSolver_) :: solver
integer(int32),allocatable   :: FixBoundary(:)
real(real64),allocatable :: u(:)
integer(int32)   :: i 
type(IO_) :: f

call cube%create("Cube3D",&
        x_axis=linspace([-100.0d0,100.0d0],50),&
        y_axis=linspace([-100.0d0,100.0d0],50),&
        z_axis=linspace([-100.0d0,0.0d0],50)&
    )

call solver%init(NumDomain=1)
call solver%setDomain(FEMDomain=cube,DomainID=1)
call solver%setCRS(DOF=3)



!!$OMP parallel 
!!$OMP do
do i = 1, cube%ne()
    call solver%setMatrix(DomainID=1,ElementID=i,DOF=3,&
       Matrix=cube%StiffnessMatrix(ElementID=i,E=300.0d0*1000.0d0, v=0.400d0) )
    !call solver%setVector(DomainID=1,ElementID=i,DOF=3,&
    !    Vector=cube%MassVector(&
    !    ElementID=i,&
    !    DOF=cube%nd() ,&
    !    Density=1.700d0,&
    !    Accel=[0.0d0, 0.0d0, -9.80d0]&
    !    ) &    
    !)
enddo
!!$OMP end do
!!$OMP end parallel



print *, "matrices imported."
!udisp. boundary
FixBoundary = cube%select(z_max = cube%z_min() )*3-2
call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)
FixBoundary = cube%select(z_max = cube%z_min() )*3-1
call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)
FixBoundary = cube%select(z_max = cube%z_min() )*3-0
call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)

! 点載荷
FixBoundary = cube%select(x_min = -3.0d0,x_max=3.0d0,z_min=-0.10d0,z_max=1.0d0,y_min=-3.0d0,y_max=3.0d0 )*3-0
call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=-1.00d0)

print *, "b.c. imported."

call cube%vtk("cube_0")

! solve
solver%debug = .true.

solver%er0 = dble(1.0e-4)
solver%relative_er = dble(1.0e-4)

u = solver%solve()
call cube%deform(disp=u )


call cube%vtk(name="cube_I1",&
    scalar=cube%getElementCauchyStress(option="I1",&
    displacement=u ,&
    E=[300000.0d0], v=[0.400d0] ))

call cube%vtk(name="cube_J2",&
    scalar=cube%getElementCauchyStress(option="J2",&
    displacement=u ,&
    E=[300000.0d0], v=[0.400d0] ))

end


