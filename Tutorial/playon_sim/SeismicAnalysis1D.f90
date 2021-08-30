use SeismicAnalysisClass

type(SeismicAnalysis_) :: seismic
real(real64) :: u(1),v(1),a(1),u_n(1),v_n(1),a_n(1)
real(real64) :: u_upd(1),v_upd(1),a_upd(1)
real(real64) :: beta, gamma, dt, T
real(real64) :: M(1,1), K(1,1), C(1,1),force(1)
real(real64) :: Amat(1,1),bvec(1),omega

integer(int32) :: i
type(Math_) :: math
type(IO_) :: f,fu,fv,fa
! 1-D problem
! with sin wave

M = 1.0d0
K = 2000.0d0
!C =0.0d0
C = 0.52400d0*M + 0.00129d0*K
C = 0.50d0*C
omega = sqrt( K(1,1)/M(1,1) ) ! natural period
T = 2.0d0*math%PI/omega

beta = 0.250d0
gamma = 0.50d0


u_n = 1.0d0
v_n = 0.0d0
a_n = 0.0d0

call f%open("1D_seismic_result.csv")
call fu%open("seismic_u.txt")
call fv%open("seismic_v.txt")
call fa%open("seismic_a.txt")
! V&V
! fix time = t=smt, caluculate numerical/analytical solution, plot errors.

do i=1,30000

    dt = dble(1.0e-4)

    ! do not destroy the second order accuracy.
    !force =0.10d0*sin(dble(i)*dt/T)
    force = 0.0d0
    !if(i<10)then
    !    force = 1.0d0
    !else
    !    force = 0.0d0    
    !endif

    Amat = seismic%getNewmarkBetaMatrix(M=M, C=C,K=K,beta=beta,gamma=gamma,dt=dt)
    bvec = seismic%getNewmarkBetaVector(M=M,C=C,u_n=u_n,v_n=v_n, a_n=a_n, &
        force=force,beta=beta,gamma=gamma,dt=dt)
    
    ! update
    ! Solve Au = b
    u_upd = 1.0d0/Amat(1,1)*bvec(1)

    v_upd = seismic%updateVelocityNewmarkBeta(u=u_upd,u_n=u_n,v_n=v_n,a_n=a_n,&
        gamma=gamma,beta=beta,dt=dt)
    a_upd = seismic%updateAccelNewmarkBeta(u=u_upd,u_n=u_n,v_n=v_n,a_n=a_n,&
        gamma=gamma,beta=beta,dt=dt)

    u_n = u_upd
    v_n = v_upd
    a_n = a_upd
    
    write(fu%fh,*)dble(i-1)*dt,u_n
    write(fv%fh,*)dble(i-1)*dt,v_n
    write(fa%fh,*)dble(i-1)*dt,a_n 

    write(f%fh,*)dble(i-1)*dt,",",u_n,",",v_n,",",a_n,",",force 
enddo
call f%close()
call fu%close()
call fv%close()
call fa%close()
print *, "Natural period : ",T," sec."


end