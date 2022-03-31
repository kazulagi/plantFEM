program main
use COOClass
use FEMDomainClass
use FEMSolverClass
implicit none


type(FEMDomain_) :: cube(3)
type(FEMDomainp_) :: cube_p(3)
type(FEMSolver_) :: solver
integer(int32),allocatable   :: FixBoundary(:)
integer(int32),allocatable   :: Overset_Elements(:,:)
integer(int32)   :: ElementID, DomainID,i
type(IO_) :: f
type(MPI_) :: mpid

integer(int32),allocatable :: row(:)
integer(int32),allocatable :: col(:)
real(real64),allocatable :: val(:),all_disp(:)
integer(int32) :: nn = 1000000.0

call mpid%start()

call cube(1)%create("Cube3D",x_num=30,y_num=3,z_num=3)
call cube(2)%create("Cube3D",x_num=30,y_num=3,z_num=3)
call cube(3)%create("Cube3D",x_num=30,y_num=3,z_num=3)

call cube(1)%resize(x=10.0d0)
call cube(2)%resize(x=10.0d0)
call cube(3)%resize(x=10.0d0)

call cube(2)%move(x=cube(1)%x_max()-0.10d0 )
call cube(3)%move(x=cube(2)%x_max()-0.10d0 )

call cube(1)%vtk("cube_no1")
call cube(2)%vtk("cube_no2")
call cube(3)%vtk("cube_no3")

print *, "[ok] Mesh creation"

! overset
call cube(2)%overset(cube(3),DomainID=3, &
    algorithm=FEMDomain_Overset_GPP,MyDomainID=2) ! or "P2P"

call cube(2)%overset(cube(1),DomainID=1, &
    algorithm=FEMDomain_Overset_GPP,MyDomainID=2) ! or "P2P"

print *, cube(2)%numOversetElements()


call solver%init(NumDomain=3)
call solver%setDomain(FEMDomains=cube(:),DomainIDs=[1,2,3])
call solver%setCRS(DOF=3)

call print(solver%CRS_ID_Starts_From ) 

!$OMP parallel 
!$OMP do
do DomainID=1,3
    do ElementID = 1, cube(DomainID)%ne()
        call solver%setMatrix(DomainID=DomainID,ElementID=ElementID,DOF=3,&
           Matrix=cube(DomainID)%StiffnessMatrix(ElementID=ElementID,E=5000000.0d0, v=0.330d0) )
        call solver%setVector(DomainID=DomainID,ElementID=ElementID,DOF=3,&
            Vector=cube(DomainID)%MassVector(&
                ElementID=ElementID,&
                DOF=cube(DomainID)%nd() ,&
                Density=1.700d0,&
                Accel=[0.0d0, 0.0d0, -9.80d0]&
                ) & 
            )
    enddo
enddo
!$OMP end do
!$OMP end parallel

print *, "[ok]Element-matrices done"

call solver%setEbOM(penalty=10000000.0d0, DOF=3)

!print *, "matrices imported."
! disp. boundary
FixBoundary = cube(1)%select(x_max = cube(1)%x_min() )*3-2
call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)
FixBoundary = cube(1)%select(x_max = cube(1)%x_min() )*3-1
call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)
FixBoundary = cube(1)%select(x_max = cube(1)%x_min() )*3-0
call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)
!FixBoundary = cube(3)%select(x_min = cube(3)%x_max() )*3-0
!FixBoundary(:) = FixBoundary(:) + solver%CRS_ID_Starts_From(3)-1
!call solver%fix(DomainID=3,IDs=FixBoundary,FixValue=-3.00d0)
!
print *, "b.c. imported."
!
call cube(1)%vtk("cube_1_step_0")
call cube(2)%vtk("cube_2_step_0")
call cube(3)%vtk("cube_3_step_0")
!
!! solve
solver%debug = .true.
!
!solver%er0 = 
solver%itrmax = 10000
solver%er0 = dble(1.0e-15)
all_disp = solver%solve(preconditioning="PointJacobi")
!all_disp = solver%solve()
call cube(1)%deform(disp=all_disp(1:solver%CRS_ID_Starts_From(2)-1  ) )
call cube(2)%deform(disp=all_disp(solver%CRS_ID_Starts_From(2):solver%CRS_ID_Starts_From(3)-1  ) )
call cube(3)%deform(disp=all_disp(solver%CRS_ID_Starts_From(3):  ) )
!
!
call cube(1)%vtk("cube_1_GaussJordan_step_1" )
call cube(2)%vtk("cube_2_GaussJordan_step_1" )
call cube(3)%vtk("cube_3_GaussJordan_step_1" )
!
call mpid%end()

! Analytical solution:
! Displacement: 3.919030 m @ x=19.9 m 

end program
