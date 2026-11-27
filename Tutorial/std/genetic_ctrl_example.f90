program main
    use GeneticControlClass
    use EarthClass
    implicit none

    type(GeneticControl_) :: gctrl
    type(Random_) :: random
    type(Earth_) :: earth
    type(datetime_)  :: time


    call earth%init()
    !call earth%setTimeZone(name="Tokyo")
    earth%MyPosition = [35.026182,135.780792] ! Kyoto University
    time = to_datetime(2026,6,25,0,0,0.0d0)

    ! load genetic relations and input/output
    call gctrl%init(config="Tutorial/std/soy_flowering.json")

    call gctrl%show_Allele_Database()
    call gctrl%show_input_signal_Database()
    call gctrl%show_output_signal_Database()
    
    call gctrl%set_genotype("E1")
    call gctrl%set_genotype("E2")
    call gctrl%set_genotype("E3")
    call gctrl%set_genotype("E4")
    call gctrl%set_genotype("E9")
    call gctrl%set_genotype("GmFT5a")
    
    print *, gctrl%genotype

    ! initial conditions
    call gctrl%set_input_signal_value("X",1.0d0/100.0d0)
    
    
    !> long daylength
    gctrl%half_life = 60.0d0*60.0d0
    
    do i_i=1,60*60*24*30
        call gctrl%set_input_signal_value("LD",random%random()*24) ! hour
        call gctrl%update_time(dt=1.0d0) ! 1 sec.
        if(mod(i_i,1000)==0) then
            !call gctrl%write("e1e2e3e4E9",time_unit="Days")
            call gctrl%write("12hr-E1E2E3E4E9",time_unit="Days")
        endif
        
    enddo

    
!    !> short daylength
!    call gctrl%set_input_signal_value("LD",7.0d0) ! hour
!    do i_i=1,60*60*24*30
!        call gctrl%update_time(dt=1.0d0) ! 1 sec.    
!        if(mod(i_i,1000)==0) then
!            call gctrl%write("e1e2E3e4E9",time_unit="Days")
!        endif
!        
!    enddo

end program main