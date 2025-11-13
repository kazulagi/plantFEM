program main
    use ODEClass
    use IOClass
    implicit none
    
    type(ODE_) :: ode
    type(IO_)  :: f
    real(real64),allocatable :: x(:)
    real(real64) :: dt
    
    call ode%init(2)
    call ode%set(d_dt=1,params=[ 0.00100d0,  0.020d0],const=-0.020d0)
    call ode%set(d_dt=2,params=[0.0200d0,  0.0010d0],const=-0.01d0)
    ode%real_positive_value = True
    ode%x = [1.0d0,1.50d0]
    dt = 1.0d0

    call print(exp(dt*ode%A))
    call print(det_mat(exp(dt*ode%A,order=100),size(ode%A,1)))

    call f%open("result.txt","w")
    do i_i=1,100
        x = ode%solve(dt=dt)
        
        write(f%fh,*) dt*i_i, x(:)
        ode%x = x
        
    enddo
    call f%close()
end program main