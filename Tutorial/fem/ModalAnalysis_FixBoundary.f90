program main
    use FEMSolverClass
    implicit none

    type(FEMSolver_) :: solver
    type(FEMDomain_),target :: domains(1)
    type(IO_) :: f
    integer(int32) :: i
    real(real64) :: Vs,t,dt,E_Al
    real(real64),allocatable :: Mode_U(:),mode_Ut(:),freq(:),eigen_value(:),eigen_vectors(:,:)
    integer(int32),allocatable :: node_list(:)
    integer(int32),allocatable :: node_list_x(:)
    integer(int32),allocatable :: node_list_y(:)
    integer(int32),allocatable :: node_list_z(:)
    ! Modal analysis
    !Create file or...
    call domains(1)%create("Cube3D",x_num=2,y_num=4,z_num=20)
    call domains(1)%resize(x=1.0d0)
    call domains(1)%resize(y=5.0d0)
    call domains(1)%resize(z=50.0d0)
    call domains(1)%vtk("AlminiumBar")
    
    !read file
    !call domains(1) % read("mesh.vtk")
    
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
    
    E_Al = 70.0d0*1000.0d0!*1000.0d0
    !$OMP parallel do
    do i=1,domains(1)%ne()
        call solver%setMatrix(&
            DomainID=1,&
            ElementID=i,&
            DOF=3,&
            Matrix=domains(1)%StiffnessMatrix(ElementID=i,E=E_Al,v=0.300d0) &
            )
    enddo
    !$OMP end parallel do

    call solver%keepThisMatrixAs("A")
    
    ! Eigen value problem solver by scipy
    
    print *, "solver%eig"
    node_list = domains(1)%getNodeList(&
        zmin = domains(1)%zmax() )
    node_list_x =(node_list(:)-1)*3+1
    call solver%fix_eig(IDs=node_list_x)
    node_list_y =(node_list(:)-1)*3+2
    call solver%fix_eig(IDs=node_list_y)
    node_list_z =(node_list(:)-1)*3+3
    call solver%fix_eig(IDs=node_list_z)

    node_list = domains(1)%getNodeList(&
        zmax = domains(1)%zmin() )
    node_list_x =(node_list(:)-1)*3+1
    call solver%fix_eig(IDs=node_list_x)
    node_list_y =(node_list(:)-1)*3+2
    call solver%fix_eig(IDs=node_list_y)
    node_list_z =(node_list(:)-1)*3+3
    call solver%fix_eig(IDs=node_list_z)
    

    node_list = domains(1)%getNodeList(&
        ymax = domains(1)%ymin() )
    node_list_x =(node_list(:)-1)*3+1
    call solver%fix_eig(IDs=node_list_x)
    node_list_y =(node_list(:)-1)*3+2
    call solver%fix_eig(IDs=node_list_y)
    node_list_z =(node_list(:)-1)*3+3
    call solver%fix_eig(IDs=node_list_z)

    call solver%eig(eigen_value=eigen_value,eigen_vectors=eigen_vectors)
    
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
