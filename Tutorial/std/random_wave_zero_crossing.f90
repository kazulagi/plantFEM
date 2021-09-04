program main
    use plantfem
    implicit none

    type(Random_) :: random
    type(IO_) :: f,z
    integer(int32) :: i, n, nn,count
    real(real64) :: g_n, g_nn, period, period_list(1000)
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
    do i=1,1000
        g_nn = random%Gauss(mu=0.0d0, sigma=1.0d0) 
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
    
end program main