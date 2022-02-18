program main
    use FEMSolverClass
    implicit none

    type(FEMSolver_) :: solver
    type(FEMDomain_),target :: domains(1)
    type(IO_) :: f
    integer(int32) :: i
    real(real64) :: Vs,t,dt,E_al
    real(real64),allocatable :: Mode_U(:),mode_Ut(:),freq(:),MassMatDiag(:),&
    MassMatInvDiag(:),eigen_vectors(:,:),eigen_value(:)

! This solver is useless
! Please use  Tutorial/fem/ModalAnalysis.f90
    !call domains(1)%create("Cube3D",x_num=100,y_num=100,z_num=100)
    call domains(1)%create("Cube3D",x_num=2,y_num=5,z_num=10)
    call domains(1)%resize(x=1.0d0)
    call domains(1)%resize(y=5.0d0)
    call domains(1)%resize(z=50.0d0)
    
    call domains(1)%vtk("cube")

    call solver%init(NumDomain=1,NumInterfaceElement=0)
    call solver%setDomain(FEMDomain=domains(1),DomainID=1)
    call solver%setCRS(DOF=3)
    
    print *, "Store Mass Matrix"
    !!$OMP parallel do
    do i=1,domains(1)%ne()
        call solver%setMatrix(&
            DomainID=1,&
            ElementID=i,&
            DOF=3,&
            Matrix=domains(1)%MassMatrix(ElementID=i,Density=2.70d0,DOF=3,Lumped=.true. )&
            )
        !call solver%setVector(&
        !    DomainID=1,&
        !    ElementID=i,&
        !    DOF=3,&
        !    Vector=domains(1)%MassVector(ElementID=1,Density=17.0d0,Accel=[0.0d0,0.0d0,-9.80d0],DOF=3) &
        !    )
    enddo
    !!$OMP end parallel do
    call solver%saveMatrix("M",CRS_as_dense=.true.)
    ! fill zero >> matrix
    MassMatDiag = diag(Solver)
    MassMatInvDiag = zeros(size(MassMatDiag) )
    MassMatInvDiag(:) = 1.0d0/MassMatDiag(:)
    
    
    call solver%zeros()

    print *, "Store Stiffness Matrix"
    
    !!$OMP parallel do
    E_Al = 70.0d0*1000.0d0*1000.0d0
    do i=1,domains(1)%ne()
        call solver%setMatrix(&
            DomainID=1,&
            ElementID=i,&
            DOF=3,&
            Matrix=domains(1)%StiffnessMatrix(ElementID=i,E=E_Al,v=0.000d0) &
            )
            
        !call solver%setVector(&
        !    DomainID=1,&
        !    ElementID=i,&
        !    DOF=3,&
        !    Vector=domains(1)%MassVector(ElementID=1,Density=17.0d0,Accel=[0.0d0,0.0d0,-9.80d0],DOF=3) &
        !    )
    enddo
    !!$OMP end parallel do
    
    call solver%matmulDiagMatrix(diagMat = MassMatInvDiag )
    call solver%saveMatrix("K",CRS_as_dense=.true.)
    
    ! UN USABLE
    eigen_vectors = solver%eig(tol=dble(1.0e-6),eigen_value=eigen_value,as_DENSE=.true.)

    do i=1,size(eigen_value)
        print *, eigen_value(i),"Hz"
    enddo
    
        
end program main

!Vs=   372.03123500454166     
!T (s)  0.10751785397672776        3.5839284658909253E-002   2.1503570795345553E-002
!Freq (1/s)   9.3007808751135421        27.902342625340623        46.503904375567707     