program main
    use FEMSolverClass
    implicit none

    type(FEMSolver_) :: solver
    type(FEMDomain_),target :: domains(1)
    type(IO_) :: f
    integer(int32) :: i
    real(real64) :: Vs,t,dt,E_Al
    real(real64),allocatable :: Mode_U(:),mode_Ut(:),freq(:),eigen_value(:),eigen_vectors(:,:)

    ! Modal analysis
!Create file or...
    call domains(1)%create("Cube3D",x_num=2,y_num=6,z_num=40)
    call domains(1)%resize(x=1.0d0)
    call domains(1)%resize(y=5.0d0)
    call domains(1)%resize(z=50.0d0)
    call domains(1)%vtk("AlminiumBar")
    
!read file
    !call domains(1) % read("AlminiumBar.vtk")
    
    call solver%init(NumDomain=1,NumInterfaceElement=0)
    call solver%setDomain(FEMDomain=domains(1),DomainID=1)
    call solver%setCRS(DOF=3)
    
    
    !$OMP parallel do
    do i=1,domains(1)%ne()
        call solver%setMatrix(&
            DomainID=1,&
            ElementID=i,&
            DOF=3,&
            Matrix=domains(1)%MassMatrix(ElementID=i,Density=2.70d0,DOF=3) &
            )
    enddo
    !$OMP end parallel do
    
    call solver%keepThisMatrixAs("B")
    call solver%zeros()

    print *, "Save Stiffness Matrix"
    
    E_Al = 70.0d0*1000.0d0*1000.0d0
    !$OMP parallel do
    do i=1,domains(1)%ne()
        call solver%setMatrix(&
            DomainID=1,&
            ElementID=i,&
            DOF=3,&
            Matrix=domains(1)%StiffnessMatrix(ElementID=i,E=70.0d0*1000.0d0*1000.0d0,v=0.300d0) &
            )
    enddo
    !$OMP end parallel do

    call solver%keepThisMatrixAs("A")
    
    ! Eigen value problem solver by scipy
    
    print *, "solver%eig"
    call solver%eig(num_eigen=domains(1)%nn()*domains(1)%nd(),eigen_value=eigen_value,eigen_vectors=eigen_vectors)
    
    ! read results
    freq = sqrt(abs(eigen_value))/2.0d0/3.141590d0
    dt = 0.10d0

    ! 20 modes
    do i_i=1,20
        mode_U = zeros(size(eigen_vectors,1))
        mode_U = eigen_vectors(:,i_i)
        dt = 1.0d0/freq(i_i)/100.0d0
        do j_j=1,100
            t = dt * dble(j_j-1)
            mode_Ut = mode_U*cos( 2.0d0*3.140d0*freq(i_i)*t )

            domains(1)%mesh%nodcoord = domains(1)%mesh%nodcoord &
            +10.0d0*reshape(mode_Ut,domains(1)%nn(),3 ) 

            call domains(1)%vtk("Mode_Fortran_"+str(i_i)+"_t_"+str(j_j))
            domains(1)%mesh%nodcoord = domains(1)%mesh%nodcoord &
            -10.0d0*reshape(mode_Ut,domains(1)%nn(),3 ) 
        enddo
    enddo
    

    print *, "Freq (Hz)"
    call print(sqrt(abs(eigen_value(1:30)))/2.0d0/3.141590d0 )
        
end program main
