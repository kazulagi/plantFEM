program main
    use plantFEM
    implicit none

    real(real64),parameter :: dt = 0.010d0
    type(Random_) :: random
    type(IO_) :: f,z
    type(Math_) :: Math
    integer(int32) :: i
    complex(kind(0d0) ),allocatable :: wave(:),spectre(:)
    

    wave = zeros(4096)
    do i=1, size(wave)
        wave(i) = random%gauss(mu=0.0d0, sigma=1.0d0)
        wave(i) = sin( dt*dble(i) )+sin( 3.0d0*dt*dble(i) )+random%gauss(mu=0.0d0, sigma=1.0d0)
    enddo
    
    call f%open("gauss.txt")
    call f%write( real(wave) )
    call f%close()

    spectre = zeros(1024    )
    spectre =  FFT(wave)
    call z%open("spectre.txt")
    do i=1,size(spectre)/2
        call z%write( sqrt( spectre(i)*spectre(i) ))
    enddo
    call z%close()

    
end program main