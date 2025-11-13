
program main
    use GeneticControlClass
    use IOClass
    implicit none
    
    type(GeneticControl_) :: gctrl
    
    ! registering dysfunctional Allele
    call gctrl%define_Allele(wild="E1",mutant="e1-nl",max_expression=0.0d0) ! 0:完全な機能欠損，0 < && < 1: 部分的な機能欠損，> 1 過剰発現
    call gctrl%define_Allele(wild="E1",mutant="e1-re",max_expression=0.0d0)
    call gctrl%define_Allele(wild="E1",mutant="e1-as",max_expression=0.1d0) ! 10%だけは機能

    call gctrl%define_Allele(wild="E2",mutant="e2",max_expression=0.0d0)
    call gctrl%define_Allele(wild="E3",mutant="e3",max_expression=0.0d0)
    call gctrl%define_Allele(wild="E4",mutant="e4",max_expression=0.0d0)
    call gctrl%define_Allele(wild="E9",mutant="e9",max_expression=0.0d0)
    call gctrl%define_Allele(wild="GmFT5a",mutant="GmFT5a-null",max_expression=0.0d0)

    
    ! Cao et al., 2017

    ! define Molecular diversity 
    ! activation (Gene or QTL)
    ! unit: 0.1% / sec.
    call gctrl%add_relation("E3","->","E1",1.0d0/1000000.0d0) ! "Factor#1","->: activation/ -| supression", "Factor#2" ", coefficient
    call gctrl%add_relation("E3","->","E2",1.0d0/2000000.0d0)
    ! activation
    call gctrl%add_relation("E4","->","E1",1.0d0/1000000.0d0)
    call gctrl%add_relation("E4","->","E2",1.0d0/2000000.0d0)
    ! supression
    call gctrl%add_relation("E1","-|","GmFT5a",1.0d0/100000.0d0)
    call gctrl%add_relation("E1","-|","E9",1.0d0/100000.0d0)
    ! supression
    call gctrl%add_relation("E2","-|","E9",1.0d0/500000.0d0)


    ! define input signal
    call gctrl%add_input_signal("X","->","GmFT5a",weight=1.0d0/2000000.0d0,threshold=0.0d0,gain=0.0d0) 
    call gctrl%add_input_signal("X","->","E9",    weight=1.0d0/1000000.0d0,threshold=0.0d0,gain=0.0d0)

    call gctrl%add_input_signal("LD","->","E3",weight=1.0d0/100000.0d0,threshold=12.0d0,gain=1000.0d0) ! 10-hour with sigmoid function
    call gctrl%add_input_signal("LD","->","E4",weight=1.0d0/200000.0d0,threshold=12.0d0,gain=1000.0d0) ! 10-hour with sigmoid function


    ! define output signal
    ! outputも正規化すべきでは．
    call gctrl%add_output_signal("GmFT5a","->","FT",weight=1.0d0/100000.0d0)
    call gctrl%add_output_signal("E9","->","FT",weight=1.0d0/100000.0d0)

    
    call gctrl%show_Allele_Database()
    call gctrl%show_input_signal_Database()
    call gctrl%show_output_signal_Database()
    

    ! set genotype (degree of activation)
    call gctrl%set_genotype("e1-nl")
    call gctrl%set_genotype("e2")
    call gctrl%set_genotype("E3")
    call gctrl%set_genotype("e4")
    call gctrl%set_genotype("E9")
    call gctrl%set_genotype("GmFT5a")

    ! or
    !call gctrl%set_genotype("E1")
    !call gctrl%set_genotype("E2")
    !call gctrl%set_genotype("E3")
    !call gctrl%set_genotype("E4")
    !call gctrl%set_genotype("E9")
    !call gctrl%set_genotype("GmFT5a")
    
    print *, gctrl%genotype

    ! initial conditions
    call gctrl%set_input_signal_value("X",1.0d0/1000.0d0) 
    
    !> long daylength
    gctrl%half_life = 60.0d0*60.0d0
    call gctrl%set_input_signal_value("LD",15.0d0) ! hour
    do i_i=1,60*60*24*5
        call gctrl%update_time(dt=1.0d0) ! 1 sec.
        if(mod(i_i,1000)==0) then
            call gctrl%write("e1e2E3e4E9",time_unit="Days")
            !call gctrl%write("E1E2E3E4E9",time_unit="Days")
        endif
        
    enddo

    
    !> short daylength
    call gctrl%set_input_signal_value("LD",7.0d0) ! hour
    do i_i=1,60*60*24*15
        call gctrl%update_time(dt=1.0d0) ! 1 sec.    
        if(mod(i_i,1000)==0) then
            call gctrl%write("e1e2E3e4E9",time_unit="Days")
            !call gctrl%write("E1E2E3E4E9",time_unit="Days")
        endif
        
    enddo

    
end program main