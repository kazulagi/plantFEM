program main
    use ElasticityClass
    use IOCLass
    implicit none

    type(Elasticity_) :: elast
    type(IO_) :: f
    integer(int32) :: Freq, i
    
    real(real64),parameter :: Density(1:2) = [  1.8d0,  1.8d0]
    real(real64),parameter :: Vs(1:2)      = [250.0d0,170.0d0]
    
    real(real64) :: ans(10), exact_value(10)


    ! Q1
    ! Impedance and (R, T) for SH wave

    ans(1) = elast%to_ImpedanceRatio(Density=Density,Vs=Vs) 
    ans(2) = elast%to_R(Density=Density,Vs=Vs) 
    ans(3) = elast%to_T(Density=Density,Vs=Vs)
    exact_value(1) = Density(2)*Vs(2)/(Density(1)*Vs(1))
    exact_value(2) = (1.0d0-exact_value(1))/(1.0d0+exact_value(1))
    exact_value(3) = 2.0d0/(1.0d0+exact_value(1))

    print *, "Ans,    ","Exact,     ","ERROR"
    do i=1,3
        print *, ans(i),exact_value(i),abs(exact_value(i)-ans(i) )
    enddo

    call f%open("SurfaceResponse.txt","w")
    do Freq = 1, 100
        write(f%fh,*) dble(Freq)/10.0d0, abs(elast%to_SurfaceResponse(&
            Density=Density,Vs=Vs, H=19.70d0, Hz=dble(Freq)/10.0d0))
    enddo
    call f%close()
    call f%plot(option="with lines; set yr[0:]; replot;")


    
end program main