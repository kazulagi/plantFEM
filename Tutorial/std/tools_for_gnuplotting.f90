program main
    use AnalystClass
    use RandomClass
    use ListClass
    implicit none
    
    type(Analyst_) :: analyst
    
    ! for data
    real(real64),allocatable :: x(:)
    real(real64),allocatable :: y1(:),y2(:),y3(:)
    type(Random_) :: random ! only for data generation

    ! plots
    type(Plot_),allocatable :: plots(:,:)

    ! pdf
    type(PDF_) :: pdf(1:2)

    ! setting
    integer(int32),parameter :: num_logger = 6 ! six loggers

    ! tools
    type(List_) :: direction

    ! iterator
    integer(int32) :: logger_id,direction_id

    direction = to_list("EW","NS","UD")
            
    ! >> data generation >> 
    allocate(plots(num_logger,3) )
    do logger_id=1,num_logger
        do direction_id = 1,3
            
            ! x_axis
            x = linspace([0.0d0,200.0d0],2000)

            ! functions
            y1 = exp(-0.1*x)        + random%gauss(mu=0.0d0,sigma=0.10d0, n=2000)
            y2 = Bessel_J0(0.1*x)   + random%gauss(mu=0.0d0,sigma=0.10d0, n=2000)
            y3 = sin(0.1*x)/(0.1*x) + random%gauss(mu=0.0d0,sigma=0.10d0, n=2000)

            ! plot function
            plots(logger_id,direction_id) = analyst%to_plot(&
                x_list = x ,&
                y_list = y1 .h. y2 .h. y3, & ! .h. is horizontal stack
                x_label="Time (s)"      , &  ! x-axis label
                y_label="Velocity (m/s)", &  ! y-axis label
                title  = "Logger #"+str(logger_id)+" "+direction%get(direction_id), & ! Graph title
                with_line = .true. , & ! plot as line
                logscale = .false. , & ! disable logscale
                regend = to_list("exp(-0.1x)","J_0(0.1x)","sin(0.1x)") & ! Regends for each curves
            )
        enddo
    enddo
    ! << data generation << 

    pdf(1) = analyst%to_pdf(plot=plots(1:3,1:3),option="confidential") ! use watermark for confidential document
    pdf(2) = analyst%to_pdf(plot=plots(4:6,1:3))

    ! rendering
    call analyst%render(name="report.pdf",pdf=pdf(1:2) )
    
end program main