program main
    use LTISystemClass
    implicit none

    type(LTISystem_) :: system
    type(Random_) :: random
    type(IO_) :: file,SpectreFile
    real(real64) :: F(2,2),G(2,1),H(1,2),x(2)
    complex(complex64) :: y(4096)
    complex(complex64),allocatable :: spectre(:),in_spectre(:)
    integer(int32) :: i,j

    ! Yule (1927), autoreguressive model
    F(1,:) = [ 0.0d0, -0.65504d0 ]
    F(2,:) = [ 1.0d0,  1.34254d0 ]

    G(1:2,1) = [-0.65504d0, 1.34254d0]

    H(1,1:2) = [1.0d0, 0.0d0]

    x(1) = random%gauss(mu=0.0d0,sigma=1.0d0) 
    x(2) = random%gauss(mu=0.0d0,sigma=1.0d0)

    call system%init(&
        State=x,&
        StateTransition=F,&
        Driving=G,&
        ObservMat=H)
    system%sigma = 15.41d0

    call file%open("Observation.txt")

    do i=1,size(y)
        call file%write(i, system%update()+44.333d0 )
        y(i) = system%ObservVec(1)      
        if(i>=256)then
            spectre = fft(y(i-255:i) )
            call Spectrefile%open("FFT_Result"//str(i)//".txt")
            do j=1,256/2
                call Spectrefile%write(j, dble(sqrt(spectre(j)*spectre(j))) )
            enddo
            
            call Spectrefile%close()
        endif
    enddo
    call file%close()
    

    
    


end program main