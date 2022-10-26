program main
use SparseClass
use FEMDomainClass
use FEMSolverClass
implicit none


type(FEMDomain_) :: cube(2)
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


call cube(1)%create("Cube3D",x_num=100,y_num=10,z_num=10)
call cube(2)%create("Cube3D",x_num=50,y_num=5,z_num=5)
call cube(1)%resize(x=10.0d0)
call cube(2)%resize(x=10.0d0)
call cube(2)%move(x=cube(1)%x_max()-0.10d0 )
call cube(1)%vtk("cube_no1")
call cube(2)%vtk("cube_no2")

print *, 101*11*11*3


! overset
call cube(1)%overset(cube(2),DomainID=2, &
    algorithm=FEMDomain_Overset_GPP,MyDomainID=1) ! or "P2P"

print *, cube(1)%numOversetElements()

call solver%init(NumDomain=2)
call solver%setDomain(FEMDomains=cube(:),DomainIDs=[1,2])
call solver%setCRS(DOF=3)

call print(solver%CRS_ID_Starts_From ) 

!$OMP parallel 
!$OMP do
do DomainID=1,2
    do ElementID = 1, cube(DomainID)%ne()
        call solver%setMatrix(DomainID=DomainID,ElementID=ElementID,DOF=3,&
           Matrix=cube(DomainID)%StiffnessMatrix(ElementID=ElementID,E=1000000.0d0, v=0.330d0) )
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


!
!print *, "matrices imported."
! disp. boundary
FixBoundary = cube(1)%select(x_max = cube(1)%x_min() )*3-2
call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)
FixBoundary = cube(1)%select(x_max = cube(1)%x_min() )*3-1
call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)
FixBoundary = cube(1)%select(x_max = cube(1)%x_min() )*3-0
call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)
!FixBoundary = cube%select(x_min = cube%x_max() )*3-0
!call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=-0.30d0)
!
print *, "b.c. imported."
!
call cube(1)%vtk("cube_1_step_0")
call cube(2)%vtk("cube_2_step_0")
!
!! solve
solver%debug = .true.
!
all_disp = solver%solve()
call cube(1)%deform(disp=all_disp(1:solver%CRS_ID_Starts_From(2)-1  ) )
call cube(2)%deform(disp=all_disp(solver%CRS_ID_Starts_From(2):  ) )
!
!
call cube(1)%vtk("cube_1_step_1" )
call cube(2)%vtk("cube_2_step_1" )
!
call mpid%end()
!
end program
