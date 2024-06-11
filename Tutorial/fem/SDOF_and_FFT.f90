program main

    use SparseClass
    use IOClass
    use RangeClass
    use FEMDomainClass
    use MathClass
    implicit none
    
    type(COO_) :: coo, Kmatrix_COO,Mmatrix_COO
    type(CRS_) :: crs, Kmatrix,Mmatrix
    real(real64),allocatable :: x(:)
    complex(real64),allocatable :: u(:),u_p(:),u_m(:),u_bar(:),k_u_bar(:)
    real(real64),allocatable :: Force(:),K_inv_F(:)
    complex(real64) :: alpha
    real(real64) :: K,dt,h,b_c_spring,damping_coeff
    integer(int32) :: timestep
    type(Math_) :: math
    type(IO_) :: f
    integer(int32) :: DOF
    complex(real64) :: fix_disp
    type(FEMDomain_),allocatable :: point(:)
    integer(int32) :: i
    
    ! Motion Equation and Exponential integrator
    
    ! [M]{d^2 u/dt^2} + [C]{du/dt} + [K]{u} = {f}
    ! {d^2 u/dt^2} + [M]^{-1}[C]{du/dt} + [M]^{-1}[K]{u} = [M]^{-1}{f}
    ! [M]^{-1}[C] = 2*h*([M]^{-1}[K])^{1/2}であるとする．
    ! [w] = [M]^{-1}[K]とおくと
    ! u(t) = exp( -h + sqrt(h^2 - 1) )t[w]) a + exp( -h - sqrt(h^2 - 1) )t[w]) b 
    
    ! u(t+dt) = exp( -h + sqrt(h^2 - 1) )(t+dt)[w]) a 
    !         + exp( -h - sqrt(h^2 - 1) )(t+dt)[w]) b 
    
    ! u(t+dt) = exp(( -h + sqrt(h^2 - 1) )(dt)[w])exp( -h + sqrt(h^2 - 1) )(t)[w]) a 
    !         + exp(( -h - sqrt(h^2 - 1) )(dt)[w])exp( -h - sqrt(h^2 - 1) )(t)[w]) b 
    
    ! u(t)^{+} := exp( -h + sqrt(h^2 - 1) )(t)[w]) a  
    ! u(t)^{-} := exp( -h - sqrt(h^2 - 1) )(t)[w]) a  
    
    ! u(t+dt) = exp( -h + sqrt(h^2 - 1)(dt) ) exp([w])u(t)^{+}
    !         + exp( -h - sqrt(h^2 - 1)(dt) ) exp([w])u(t)^{-}
    
    DOF = 2
    
    
    
    call coo%init(DOF)
    call Kmatrix_COO%init(DOF)
    call Mmatrix_COO%init(DOF)
    
    ! create mass-matrix
    do i=1,DOF-1
        call Mmatrix_COO%add(i  , i  , 0.50d0)
        call Mmatrix_COO%add(i  , i+1, 0.50d0)
        call Mmatrix_COO%add(i+1, i  , 0.50d0)
        call Mmatrix_COO%add(i+1, i+1, 0.50d0)
    enddo
    
    ! create stiffness-matrix
    do i=1,DOF-1
        K = 9.0d0
        call Kmatrix_COO%add(i  , i  ,   K)
        call Kmatrix_COO%add(i  , i+1, - K)
        call Kmatrix_COO%add(i+1, i  , - K)
        call Kmatrix_COO%add(i+1, i+1,   K)
    enddo
    
    Mmatrix = Mmatrix_COO%to_CRS()
    Kmatrix = Kmatrix_COO%to_CRS()
    
    crs = Kmatrix%divide_by(diag_vector=Mmatrix%diag(cell_centered=.true.))
    
    
    !do i=1,DOF
    !    if(i<DOF/2)then
    !        K = 100.0d0
    !    else
    !        K = 10.0d0
    !    endif
    !    call coo%add(i  , i  ,   K)
    !    call coo%add(i  , i+1, - K)
    !    call coo%add(i+1, i  , - K)
    !    call coo%add(i+1, i+1,   K)
    !enddo
    !crs = coo%to_crs()
    
    
    ! u(+)
    u_p = 0.0d0*ones(DOF)
    u_p(DOF) = 0.50d0 
    !u_p(DOF) = 20.d0 
    ! u(-) 
    u_m = 0.0d0*ones(DOF)
    u_m(DOF) = 0.50d0 
    !u_m(DOF) = 20.d0 
    ! f 
    force = zeros(DOF)
    !force(DOF) = 1.0d0
    
    
    b_c_spring = 100000.0d0
    damping_coeff = 100.0d0
    
    u   = zeros(DOF)
    
    
    ! parameters
    dt = 1.0d0/10.0d0
    h  = 0.00d0
    
    
    if(h<1.0d0)then
        alpha = sqrt(sqrt(abs(1.0d0-h*h)))*math%i
    else
        alpha = sqrt(h*h-1.0d0)
    endif
    call f%open("result.txt","w")
    
    allocate(point(DOF))
    do i=1,DOF
        call point(i)%create("Sphere3D",x_num=8,y_num=8,z_num=8)
        call point(i)%resize(x=0.80d0,y=0.80d0,z=0.80d0)
        call point(i)%move(z=(i-1)*2.0d0)
    enddo
    
    !call Kmatrix%fix(idx=[1],RHS=Force,val=[0.0d0])
    K_inv_F = 0.0d0*Force/K
    
    write(f%fh,*) dt*(0), dble(u),imag(u)
    do timestep=1,300
        !u(t) = exp(dt*(sqrt(h*h-1)-h ) )exp([w])u(+) + exp(dt*(-sqrt(h*h-1)-h ) )exp([w])u(-)
        print *, timestep
    
        if(timestep>=100) then
            K_inv_F=0.0d0
        endif
        fix_disp = 0.0d0
        if( timestep .in. to_range([50,150]) )then
            Force = zeros(DOF)
            Force(DOF) = sin(11.0d0*timestep*dt)
            K_inv_F = Force
            !call Kmatrix%BiCGSTAB(x=K_inv_F,b=Force,fix_idx=fix_idx)

            u_p = crs%tensor_exp_sqrt(v=u_p,&
                tol = dble(1.0e-20),coeff= dt*alpha - dt*h,&
                itrmax=100,fix_idx=[1] )+K_inv_F/2.0d0
            u_m = crs%tensor_exp_sqrt(v=u_m,&
                tol = dble(1.0e-20),coeff=-dt*alpha - dt*h,&
                itrmax=100,fix_idx=[1])+K_inv_F/2.0d0
        else
            u_p = crs%tensor_exp_sqrt(v=u_p,&
                tol = dble(1.0e-20),coeff= dt*alpha - dt*h,&
                itrmax=100,fix_idx=[1])+K_inv_F/2.0d0
            u_m = crs%tensor_exp_sqrt(v=u_m,&
                tol = dble(1.0e-20),coeff=-dt*alpha - dt*h,&
                itrmax=100,fix_idx=[1])+K_inv_F/2.0d0
        endif
        
        
        
        ! post processing
        do i=1,DOF
            call point(i)%move(x=dble(- u(i) + u_p(i) + u_m(i)) )
        enddo
        u = u_p + u_m
        do i=1,DOF
            call point(i)%vtk("result_p"+str(i)+"_"+zfill(timestep,6) )
        enddo
        
        !print *, dt*(timestep-1), abs(u)
        write(f%fh,*) dt*(timestep), dble(u),imag(u)
        
    enddo
    
    call f%close()
    print *, fft(infile="result.txt",outfile="fft.txt",window_size=256,dt=dt,column=3)
    
    
    end program main