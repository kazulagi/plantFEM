program main
    use GeneticControlClass
    use EarthClass
    implicit none

    type(GeneticControl_) :: gctrl
    type(Earth_) :: earth
    type(datetime_)  :: time
    
    ! あとはTsubokura 2013の63系統でフィッティング

    ! 
    call earth%init()
    !call earth%setTimeZone(name="Tokyo")
    !earth%MyPosition = [35.026182, 135.780792] ! Kyoto University
    earth%MyPosition = [36.02924179876086, 140.0978372515609] ! Tsukuba
    !earth%MyPosition = [42.912568, 143.162336] ! Obihiro
    time = to_datetime(2010,6,3,0,0,0.0d0)
    

    ! load genetic relations and input/output
    call gctrl%init(config="Tutorial/std/soy_flowering.json")

    call gctrl%show_Allele_Database()
    call gctrl%show_input_signal_Database()
    call gctrl%show_output_signal_Database()
    
    call gctrl%set_genotype("e1-nl")
    call gctrl%set_genotype("e2")
    call gctrl%set_genotype("E3")
    call gctrl%set_genotype("E4")
    call gctrl%set_genotype("E9")
    call gctrl%set_genotype("GmFT5a")
    
    print *, gctrl%genotype

    ! initial conditions
    call gctrl%set_input_signal_value("X",1.0d0/100.0d0)
    
    gctrl%half_life = 60.0d0*60.0d0*24*2

    do i_i=1,60*60*24*120
        time = time + to_datetime(0,0,0,0,0,1.0d0)
        call earth%setTime(time)

        call gctrl%set_input_signal_value("LD",earth%getDayLength()) ! hour
        call gctrl%update_time(dt=1.0d0) ! 1 sec.
        
        if(mod(i_i,60*60*24)==0) then
            !call print(real(gctrl%ode%A))
            print *, time%year,time%month,time%date,earth%getDayLength(), "hour"
            print *, gctrl%input_signal(3)%name, gctrl%input_signal(3)%current_value,&
                gctrl%input_signal(3)%weight*sigmoid(&
                x=gctrl%input_signal(3)%current_value,&
                params=[gctrl%input_signal(3)%gain,gctrl%input_signal(3)%threshold] &
            )
            !call gctrl%write("e1e2e3e4E9",time_unit="Days")
            call gctrl%write("Tsukuba-2010-06-03-e1e2E3E4E9",time_unit="Days")
        endif
    enddo


end program main