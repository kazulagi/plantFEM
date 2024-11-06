program main
    use plantfem
    implicit none!
    type(IO_) :: fp
    ! time increment
    real(real64) :: dt!
    real(real64) :: P

    dt = 60.0d0 ! 60 s!
    call fp%open("rain_profile.txt","w")
    do i_i=1,60*24*10 ! 30-day
        if(dt*i_i<=dble(60*60*24))then
            ! 10 
            P = 200.0d0/24.0d0/60.0d0/60.0d0 ! 200 mm/d
        elseif(dt*i_i<=dble(60*60*24)*2)then
            P = 100.0d0/24.0d0/60.0d0/60.0d0 ! 100 mm/d
        elseif(dt*i_i<=dble(60*60*24)*4)then
            P = 0.0d0
        elseif(dt*i_i<=dble(60*60*24)*5)then
            P = 300.0d0/24.0d0/60.0d0/60.0d0 ! 100 mm/d
        else
            P = 0.0d0
        endif
        write(fp%fh,*) dt*i_i/60.0/60.0d0/24.0d0, P ! mm/s
        
    enddo
    call fp%close()!
end program main
 