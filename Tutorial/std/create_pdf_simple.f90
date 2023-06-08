program main
    use AnalystClass
    implicit none
    
    type(Analyst_) :: analyst
    real(real64),allocatable :: x(:)
    real(real64),allocatable :: y1(:),y2(:)

    ! create data


    x = linspace([(i_i-1)*100.0d0 + 0.0d0,(i_i-1)*100.0d0 + 1000.0d0],100000)
    y1 = sin(0.01*x)/(0.01*x)
    y2 = Bessel_J0(0.01*x)
    

    call analyst%pdf(&
            name  = "report.pdf",&
            x_list = x ,&
            y_list = y1 .h. y2 ,&
            x_label="Time (s)",&
            y_label="Velocity (m/s)", &
            title  = "Waveform", &
            with_line = .true., &
            logscale = .false.  &
        )

end program main