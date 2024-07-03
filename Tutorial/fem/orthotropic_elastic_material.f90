use FEMSolverClass
implicit none


type(FEMDomain_) :: cube
type(FEMSolver_) :: solver
integer(int32),allocatable   :: FixBoundary(:)
integer(int32)   :: i 
real(real64),allocatable :: displ(:),sigma(:,:),tr_sigma(:),E_G(:),v(:),&
    pressure(:)

call cube%create("Cube3D",x_num=100,y_num=3,z_num=3)
call cube%resize(x=20.0d0,y=1.0d0,z=1.0d0)
call solver%init(NumDomain=1)
call solver%setDomain(FEMDomain=cube,DomainID=1)
call solver%setCRS(DOF=3)

! elastic modulus for orthotropic elastic material
! https://www.frontistr.com/seminar/140730/orthotropic_elastic_material.pdf
E_G = zeros(6)
v = zeros(6)
E_G(1) = 100.0d0*10**4
E_G(2) = 1000.0d0*10**4
E_G(3) = 1000.0d0*10**4
E_G(4) = 7.69d0*10**3
E_G(5) = 7.69d0*10**3
E_G(6) = 7.69d0*10**3
v(:) = 0.30d0


pressure = zeros(cube%ne())
pressure(cube%getElementList(zmax=0.50d0))=15*1000.0d0

!$OMP parallel 
!$OMP do
do i = 1, cube%ne()
    call solver%setMatrix(DomainID=1,ElementID=i,DOF=3,&
       Matrix=cube%StiffnessMatrix(ElementID=i,E=E_G, v=v) )
    call solver%setVector(DomainID=1,ElementID=i,DOF=3, &
        Vector=cube%MassVector(&
                ElementID=i,&
                DOF=cube%nd() ,&
                Density=1.0d0,&
                Accel=[0.0d0, 0.0d0, -9.80d0]) &
            +cube%PressureVector(&
                ElementID=i,&
                Pressure=pressure(i) &
            )&
    )
enddo
!$OMP end do
!$OMP end parallel

print *, "matrices imported."
! disp. boundary
FixBoundary = cube%select(x_max = cube%x_min() )*3-2
call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)
FixBoundary = cube%select(x_max = cube%x_min() )*3-1
call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)
FixBoundary = cube%select(x_max = cube%x_min() )*3-0
call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)
!FixBoundary = cube%select(x_min = cube%x_max() )*3-2
!call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.10d0)

print *, "b.c. imported."

call cube%vtk("cube_0")

! solve
solver%debug = .true.
solver%relative_er = dble(1.0e-8)
solver%er0 = dble(1.0e-8)

displ = solver%solve()
call cube%deform(disp=displ )

!compute cell-averaged mean stress
!trace(sigma)
tr_sigma = zeros(cube%ne() )
do i_i=1,cube%ne()
    sigma = zeros(3,3)
    sigma = cube%stressMatrix(ElementID=i_i,&
        disp=reshape(displ,cube%nn(),cube%nd() ),&
        E=100.0d0, v=0.40d0)
    tr_sigma(i_i) = trace(sigma)/3.0d0
enddo

! x = X + u
cube%mesh%Nodcoord(:,:) = cube%mesh%Nodcoord(:,:) + reshape(displ,cube%nn(),cube%nd() )


! show result
call cube%vtk("result_pressure_15",scalar=tr_sigma)

call cube%remove()
call solver%remove()

end