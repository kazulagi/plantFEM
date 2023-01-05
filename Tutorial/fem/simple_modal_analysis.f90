use FEMDomainClass
implicit none

type(FEMDomain_) :: cube
type(CRS_) :: MassMatrix,StiffnessMatrix
type(Time_) :: time
type(Math_) :: math
real(real64),allocatable :: X(:,:),lambda(:)
integer(int32) :: ElementID

! easy modal analysis
call cube%create("Cube3D",x_num=20,y_num=5,z_num=1)
call cube%resize(x=10.0d0,y=1.0d0,z=0.10d0)
print *, "mesh is created."
call cube%vtk("init")

! CRS-formatted 3-D ZeroMatrix
call time%start()
StiffnessMatrix = cube%StiffnessMatrix( &
    YoungModulus=dble(1.0e+6)*eyes(cube%ne() ),&
    PoissonRatio=0.30*eyes(cube%ne() ) )
call time%show()

call time%start()
MassMatrix = cube%MassMatrix( &
    DOF=3,&
    Density=ones(cube%ne() ) )
call time%show()

! Solve by LOBPCG
call LOBPCG(&
    A=StiffnessMatrix,&
    B=MassMatrix,&
    lambda=lambda,&
    X=X,&
    m=10,&
    MAX_ITR=1000000,&
    TOL=dble(1.0e-8),&
    debug=.true.)

! Solve by LAPACK
!call LAPACK_EIG(A=StiffnessMatrix,B=MassMatrix,x=X,lambda=lambda)

call print(sqrt(abs(lambda))/math%PI/2.0d0)

do i_i=1,size(X,2)
    call cube%deform(disp= X(:,i_i))
    call cube%vtk("mode_"+zfill(i_i,3) )
    call cube%deform(disp=-X(:,i_i))
enddo


end

