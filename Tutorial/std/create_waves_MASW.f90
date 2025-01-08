use plantfem
implicit none

type(IO_) :: f
integer(int32) :: ch_id,time_id
real(real64) :: dt,t,t0_c1,t0_c2,t0_c3,c1,c2,c3,dx,sigma
type(Math_) :: math

dt = 1.0d0/1000.0d0/1000.0d0
dx = 0.010d0
c1  = 100.0d0
c2  = 200.0d0
c3  = 400.0d0

do ch_id=1,20
    call f%open("sample_wave_mult_channel_"+zfill(ch_id,4)+".txt")
    t0_c1 = dx/c1*ch_id 
    t0_c2 = dx/c2*ch_id 
    t0_c3 = dx/c3*ch_id 
    do time_id=1,20000
        t = dt*time_id
        sigma = 0.00010d0
        write(f%fh,*) t,1.0d0/1000.0d0/1000.0d0/1000.0d0* RickerFunction(t=t,sigma=sigma,center=t0_c1+0.002) &
            + 1.0d0/1000.0d0/1000.0d0/1000.0d0* RickerFunction(t=t,sigma=sigma*2,center=t0_c2+0.002) &! 
            + 1.0d0/1000.0d0/1000.0d0/1000.0d0* RickerFunction(t=t,sigma=sigma*3,center=t0_c3+0.002)! 
!             + RickerFunction(t=t,sigma=sigma*32,center=t0+0.002)    &
!             + RickerFunction(t=t,sigma=sigma*4,center=t0  + 0.002)    &
!             + RickerFunction(t=t,sigma=sigma/4,center=t0  + 0.002)    &
!             + RickerFunction(t=t,sigma=sigma/32,center=t0 + 0.002)    &
!             + RickerFunction(t=t,sigma=sigma/64,center=t0 + 0.002)    
    enddo
    call f%close()
enddo

end