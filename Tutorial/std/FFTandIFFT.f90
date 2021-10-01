use plantfem
implicit none

type(IO_) :: f
type(Math_) :: math
type(Random_) :: random
complex(complex64),allocatable :: t(:), Ft(:),Fw(:),Fw_bar(:),Ft_(:),omega(:)
real(real64) :: domega, n
integer(int32) :: i

! Time
t = linspace([0.0d0, 10.0d0],1024)

! Frequency
omega = linspace([0.0d0, 1024.0d0/10.0d0],1024)

Ft = zeros(1024)
!Ft(:) = 1.0d0/2.0d0/Math%PI*domega*(sin( (2.0d0*n + 1)/2.0d0*domega*t ) )/( sin( 1.0d0/2.0d0*domega*t  ))
do i=1,size(Ft)
    Ft(i) = random%gauss(mu=0.0d0,sigma=0.50d0)
enddo
Ft(:) = Ft(:)+5.0d0*sin(2.0d0*Math%PI*t) 

Fw = FFT(Ft)

Fw( 516-400:516+400 )=0.0d0

Ft_ = IFFT(Fw)

Fw_bar = Fw
Fw_bar = Fw*Fw

call f%open("Fw.txt","w")
!call f%write(real(t), real(Fw) )
call f%write(real(omega(1:size(t)/2 ) ), real(Fw(1:size(t)/2) ) )
call f%close()

call f%open("Ft.txt","w")
!call f%write(real(t), real(Fw) )
call f%write(real(t), real(Ft) )
call f%close()

call f%open("Ft_.txt","w")
!call f%write(real(t), real(Fw) )
call f%write(real(t), real(Ft_) )
call f%close()

call f%plot("Ft.txt","w l")
call f%plot("Fw.txt","w l")
call f%plot("Ft_.txt","w l")
end