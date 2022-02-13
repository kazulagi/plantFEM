program main
    use FEMSolverClass
    implicit none

    type(FEMSolver_) :: solver
    type(FEMDomain_),target :: domains(1)
    type(IO_) :: f
    integer(int32) :: i
    real(real64) :: Vs,t,dt,E_Al
    real(real64),allocatable :: Mode_U(:),mode_Ut(:),freq(:)

    ! Modal analysis

    call domains(1)%create("Cube3D",x_num=2,y_num=6,z_num=40)
    call domains(1)%resize(x=1.0d0)
    call domains(1)%resize(y=5.0d0)
    call domains(1)%resize(z=50.0d0)
    
    call domains(1)%vtk("AlminiumBar")
    
    call solver%init(NumDomain=1,NumInterfaceElement=0)
    call solver%setDomain(FEMDomain=domains(1),DomainID=1)
    call solver%setCRS(DOF=3)
    
    print *, "Save Mass Matrix"
    
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
    
    call solver%saveMatrix("M",CRS_as_dense=.true.)
    ! fill zero >> matrix
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

    call solver%saveMatrix("K",CRS_as_dense=.true.)
    
    
    ! Eigen value problem solver by scipy
    call system("python3 Tutorial/fem/eigen.py")
    
    
    ! read results
    freq = f%import("eigen_val.txt")
    freq = sqrt(abs(freq))/2.0d0/3.141590d0
    dt = 0.10d0

    ! 20 modes
    do i_i=1,20
        mode_U = f%import("U_"+str(i_i-1)+".txt")
        dt = 1.0d0/freq(i_i)/100.0d0
        do j_j=1,100
            t = dt * dble(j_j-1)
            mode_Ut = mode_U*cos( 2.0d0*3.140d0*freq(i_i)*t )

            domains(1)%mesh%nodcoord = domains(1)%mesh%nodcoord &
            +10.0d0*reshape(mode_Ut,domains(1)%nn(),3 ) 

            call domains(1)%vtk("Mode"+str(i_i)+"_t_"+str(j_j))
            domains(1)%mesh%nodcoord = domains(1)%mesh%nodcoord &
            -10.0d0*reshape(mode_Ut,domains(1)%nn(),3 ) 
        enddo
    enddo
    do i=1,60
        print *, freq(i),"Hz"
    enddo
    
        
end program main
