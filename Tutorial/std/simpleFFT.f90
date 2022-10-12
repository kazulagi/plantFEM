program main
    use plantFEM
    implicit none

    real(real64),parameter :: duration=10.0d0
    integer(int32),parameter :: sampling = 4096
    type(Random_) :: random
    type(IO_) :: f,z
    type(Math_) :: Math
    integer(int32) :: i
    complex(complex64 ),allocatable :: wave(:),spectre(:)
    real(real64),allocatable :: omega(:), t(:)
    real(real64) :: dt,set_omega

    set_omega = (20.0d0/2.0d0/math%PI) ! 15.92356687898089

    print *, set_omega
    create_input_wave: block
        t    = linspace([0.0d0, duration], sampling)
        dt   = duration/dble(sampling)
        wave = zeros(4096)
        do i=1, size(wave)
            wave(i) = random%gauss(mu=0.0d0, sigma=1.0d0)
            wave(i) = sin( set_omega*t(i) )!+sin( 3.0d0*dt*dble(i) )!+random%gauss(mu=0.0d0, sigma=1.0d0)
        enddo
    end block create_input_wave

    show_input_wave: block
        call f%open("input_wave.txt")
        call f%write( t,real(wave) )
        call f%close()
        call f%plot(option="with lines")
    end block show_input_wave

    run_FFT: block
        omega   = linspace([0.0d0, dble(sampling)/duration*2.0d0*math%PI ], sampling)
        spectre = zeros(sampling)
        spectre =  FFT(wave)
        call z%open("spectre.txt")
        do i=1,size(spectre)/2
            call z%write(omega(i),abs( spectre(i)))
        enddo
        call z%close()
        call z%plot(option="with lines",logscale=true)
    end block run_FFT

end program main