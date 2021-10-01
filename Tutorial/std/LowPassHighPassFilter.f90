use plantfem
implicit none

type(IO_)::f
type(Random_) :: random
type(Console_) :: console
complex(complex64),allocatable :: t(:), omega(:),Fw(:),ft(:),&
    Fw512(:),Fw256(:),Fw128(:),Fw064(:),&
    Ft512(:),Ft256(:),Ft128(:),Ft064(:)
integer(int32) :: i, mode_id

call console%log("Low-pass mode = 0, high-pass mode=1")
mode_id = fint(console%readline())


t  = linspace([0.0d0,30.0d0], 1024)
omega  = linspace([0.0d0,30.0d0/1024.0d0], 1024)
Fw = zeros(1024)
Fw512 = zeros(1024)
Fw256 = zeros(1024)
Fw128 = zeros(1024)
Fw064 = zeros(1024)
Ft512 = zeros(1024)
Ft256 = zeros(1024)
Ft128 = zeros(1024)
Ft064 = zeros(1024)

ft = zeros(1024)

! create impulse
do i=400,600
    ft(i) = 1.0d0
enddo

fw = fft(ft)

if(mode_id==0)then
    ! Low-pass filter
    fw512(1:512) = Fw(1:512)
    fw256(1:256) = Fw(1:256)
    fw128(1:128) = Fw(1:128)
    fw064(1:64) = Fw(1:64)
else
    ! High-pass filter
    fw256(512-256:512+256) = Fw(512-256:512+256)
    fw128(512-128:512+128) = Fw(512-128:512+128)
    fw064(512-64 :512+64 ) = Fw(512-64 :512+64 )
endif

ft = ifft(Fw)
ft512 = ifft(Fw512)
ft256 = ifft(Fw256)
ft128 = ifft(Fw128)
ft064 = ifft(Fw064)

call f%open("impulse100.txt","w")
call f%write(real(t(:) ),real(Ft(:) ))
call f%close()
call f%open("impulse512.txt","w")
call f%write(real(t(:) ),real(Ft512(:) ))
call f%close()
call f%open("impulse256.txt","w")
call f%write(real(t(:) ),real(Ft256(:) ))
call f%close()
call f%open("impulse128.txt","w")
call f%write(real(t(:) ),real(Ft128(:) ))
call f%close()
call f%open("impulse064.txt","w")
call f%write(real(t(:) ),real(Ft064(:) ))
call f%close()

call f%plot("impulse100.txt","with lines")
call f%plot("impulse512.txt","with lines")
call f%plot("impulse256.txt","with lines")
call f%plot("impulse128.txt","with lines")
call f%plot("impulse064.txt","with lines")


end