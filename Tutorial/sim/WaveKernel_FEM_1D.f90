
program main
    use RangeClass
    use WaveKernelClass
    implicit none
    
    type(WaveKernel_) :: WK
    real(real64),allocatable :: u(:),v(:),a(:),u_n(:),v_n(:),a_n(:),Vp(:),force(:)
    real(real64),allocatable :: YoungModulus(:), Density(:),u_xyz(:,:)

    integer(int32),allocatable :: fix_idx(:)
    type(FEMDomain_) :: object
    real(real64) :: dt,t,cp,cutoff_frequency

    type(IO_) :: f
    integer(int32) :: total_step
    
    ! 1D problem
    call object%create("Line1D",x_num=2000)
    call object%resize(x=2000.0d0)
    call object%vtk("Line1D")
    
    Vp           = 200.0d0*ones(object%ne() )
    Density      = 2.50d0*ones(object%ne() )
    YoungModulus = Vp*Vp*density
    total_step = 2000
    cutoff_frequency = 10000.0

    call WK%init(FEMDomain=object,&
        YoungModulus=YoungModulus,&
        Density=Density,&
        DOF=1)
        
    force = zeros(object%nn() )
    
    t = 0.0d0
    v = zeros(object%nn() )
    v(object%select(x_min=400.0d0,x_max=600.0d0 ) ) = 1.0d0/1000.0d0
    u = zeros(object%nn() )
    a = zeros(object%nn() )
    
    u_n = u
    v_n = v
    a_n = a

    cp = Vp(1)
    dt = 2.00d0/cp
    
    print *, "YoungModulus",YoungModulus(1)
    print *, "Vp",cp ,"m/s"!= sqrt(YoungModulus(1)/Density(1))
    print *,"dx",1.0d0
    print *,"dt",dt
    print *,"Frequency (Hz)",1.0d0/dt
    print *,"Nyquist (Hz)",1.0d0/dt/2.0d0
    print *, "CFL (Courant number)",cp*dt/1.0d0
    print *, "U (m)", dt*total_step*cp

!    
!    i_i=0
!    u_xyz = transpose(reshape(dble(v),[1,object%nn() ]))
!    call f%open("result_FE_v"+zfill(i_i,6) +".txt","w")
!    call f%write( object%mesh%nodcoord(:,1) .h. u_xyz(:,1) )
!    call f%close()
!    v(:) = 0.0d0
!    v(object%select(x_min=800.0d0,x_max=1000.0d0 ) ) = 1.0d0/1000.0d0/2.0d0
!    v(object%select(x_min=0.0d0,x_max=200.0d0 ) ) = 1.0d0/1000.0d0/2.0d0
!
!    i_i=2000
!    u_xyz = transpose(reshape(dble(v),[1,object%nn() ]))
!    call f%open("result_FE_analytical_v"+zfill(i_i,6) +".txt","w")
!    call f%write( object%mesh%nodcoord(:,1) .h. u_xyz(:,1) )
!    call f%close()
!
!
!    stop
!    ! fix_boundary
    !fix_idx = [1,object%nn() ]
    !fix_idx = [object%nn() ]
    
    do i_i=1,total_step
        print *, i_i
        if(i_i>10)then
            force  = 0.0d0
        endif
        t = t + dt
        
        WK%itrmax = 30
        WK%tol = dble(1.0e-25)
        
        u =   WK%getDisplacement(u_n=u_n, v_n=v_n,&
            dt=dt,fix_idx=fix_idx,cutoff_frequency=cutoff_frequency)

        v =   WK%getVelocity(u_n=u_n, v_n=v_n,&
            dt=dt,fix_idx=fix_idx,cutoff_frequency=cutoff_frequency)

        u(fix_idx)=0.0d0
        v(fix_idx)=0.0d0
        u_n = u
        v_n = v
        a_n = a

        u_xyz = transpose(reshape(dble(v),[1,object%nn() ]))
        call f%open("result_FE_v"+zfill(i_i,6) +"_10kHz.txt","w")
        call f%write( object%mesh%nodcoord(:,1) .h. u_xyz(:,1) )
        call f%close()
        u_xyz = transpose(reshape(dble(u),[1,object%nn() ]))
        call f%open("result_FE_u"+zfill(i_i,6) +"_10kHz.txt","w")
        call f%write( object%mesh%nodcoord(:,1) .h. u_xyz(:,1) )
        call f%close()
        
    enddo
    
    print *, "YoungModulus",YoungModulus(1)
    print *, "Vp",cp
    print *,"dx",1.0d0
    print *,"dt",dt
    print *,"step",total_step
    print *, "CFL (Courant number)",cp*dt/1.0d0
    print *, "U (m)", dt*total_step*cp
end program main