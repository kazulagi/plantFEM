use SeepageFlowClass
implicit none

type(IO_) :: f
real(Real64),allocatable :: theta(:),psi(:)
real(Real64),allocatable :: theta_inv(:),psi_inv(:)
integer(int32) :: n = 1000000


psi = linspace([0.0d0,10000.0d0],n)
theta = SWCC(model="van Genuchten",&
    params=[0.1d0,1.00d0,0.30d0,2.0d0],psi=psi,n=n)
call f%open("SWCC_van_Genuchten_1.txt","w")
call f%write(psi,theta)
call f%close()
call f%plot(option="with lines; set logscale x; replot;")

theta_inv = linspace([0.1000010d0,1.0d0],n)
psi_inv = SWCC(model="van Genuchten",&
    params=[0.1d0,1.00d0,0.30d0,2.0d0],theta=theta_inv,n=n)
call f%open("SWCC_van_Genuchten_2.txt","w")
call f%write(psi_inv,theta_inv)
call f%close()
call f%plot(option="with lines; set logscale x; replot;")

end