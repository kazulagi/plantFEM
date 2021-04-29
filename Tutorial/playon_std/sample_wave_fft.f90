program main
    use RandomClass
    use MathClass
    use IOClass
    implicit none

    integer(int32),parameter :: sample = 2**11
    real(real64),parameter :: dt = 0.0010d0
    type(Random_) :: random
    type(IO_) :: f,z
    type(Math_) :: Math
    integer(int32) :: i, n, nn,count
    real(real64) :: g_n, g_nn, period, period_list(sample)
    complex(kind(0d0) ),allocatable :: wave(:)
    real(real64) :: spectre(sample),t
    real(real64),allocatable :: histogram(:,:)

    ! get random wave & zero crossing
    call f%open("random_wave.csv")
    
    g_n = -1.0d0
    g_nn= 0.0d0
    n = 0
    nn = 0
    period = 0.0d0 
    count=0
    period_list(:) = 0.0d0
    wave = period_list
    do i=1,sample
        ! generate wave

        ! Gaussian process
        !g_nn = random%Gauss(mu=0.0d0, sigma=1.0d0) 
        
        ! wave
        !g_nn = sin(dble(i)*2*Math%PI)+10.0d0*sin(dble(i)/2.0d0*Math%PI)
        t = dble(i)*dt
        
        ! wave
        g_nn =    1.0d0*sin( 2.0d0*Math%PI*10.0d0*t )&
                + 0.5d0*sin( 2.0d0*Math%PI*20.0d0*t )&
                + 0.8d0*sin( 2.0d0*Math%PI*40.0d0*t )
        
        wave(i) = g_nn
        !g_nn = random%ChiSquared(k=1.0d0) 
        
        if( g_n*g_nn < 0.0d0 )then
            count=count+1
            nn = i
            period = dble(nn) - dble(n)
            period_list(count) = period
            n = nn
            g_n = g_nn
        endif
        
        call f%write( trim(str(dble(i))) //", "//str(g_nn) )
        
    enddo
    call f%close()

    ! get histogram
    histogram =  random%histogram(list=period_list,division=20)
    call z%open("zero_cross.csv")
    
    do i=1,size(histogram,1)
        write(z%fh,*) histogram(i,1),histogram(i,2)
    enddo
    call z%close()

    call z%open("wave.txt")
    do i=1,sample
        call z%write(str(dble(i)*dt )//" "//str(dble(wave(i) )) )     
    enddo
    call z%close()

    spectre =  FFT(wave)
    call z%open("spectre.txt")
    do i=1,sample
        call z%write( str( dble(i)*2.0d0/dble(sample) )//" "//str(dble( abs(spectre(i)) )) )    
    enddo
    call z%close()

    
end program main