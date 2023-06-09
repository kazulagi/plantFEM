program main
    use AnalystClass
    use RandomClass
    use ListClass
    use SpectreAnalysisClass
    use MathClass
    implicit none
    
    type(Analyst_) :: analyst
    
    ! for data
    type(Math_) :: math
    real(real64),allocatable :: x(:)
    real(real64),allocatable :: y1(:),y2(:),y3(:)
    type(Random_) :: random ! only for data generation


    ! fft and spectre analysis
    type(SpectreAnalysis_) :: speana
    real(real64),allocatable :: all_data(:,:)
    real(real64),allocatable :: all_segments(:,:,:)
    real(real64),allocatable :: all_spectrum(:,:,:)
    real(real64),allocatable :: all_phase(:,:,:)

    ! plots
    type(Plot_),allocatable :: plots(:,:)

    ! pdf
    type(PDF_),allocatable :: pdf_wave(:),pdf_spec(:),pdf_pha(:)

    ! setting
    integer(int32),parameter :: num_logger = 20 ! Fifty loggers

    ! tools
    type(List_) :: direction

    ! iterator
    integer(int32) :: logger_id,direction_id
    integer(int32),allocatable :: idx(:)
    type(IO_) :: f
    character(:),allocatable :: filename


    ! >> data generation >> 
    allocate(plots(num_logger,3) )

    ! data => FFT => segemnt plot & average plot

    ! dummy data
    do logger_id=1,num_logger
        ! x_axis
        x = linspace([0.0d0,100.0d0],20000)
        ! functions
        y1 = sin(10.0*math%pi*x)+sin(3.0*math%pi*x)  + random%gauss(mu=0.0d0,sigma=0.0010d0, n=20000)
        y2 = sin(20.0*math%pi*x) + random%gauss(mu=0.0d0,sigma=0.0050d0, n=20000)
        y3 = sin(30.0*math%pi*x)+ random%gauss(mu=0.0d0,sigma=0.030d0, n=20000)
        call f%open("Logger"+zfill(logger_id,2)+".tsv","w" )
        call f%write(x .h. y1 .h. y2 .h. y3 )
        call f%close()    
    enddo


    ! Files::
    ! Wave data :: format >> [t(:),1ch(:),2ch(:),3ch(:),...]
    ! Wave data :: name   >> ./Logger+zfill(logger_id,2)+".tsv"


!http://www.gnuplotting.org/data/world_10m.txt

    ! create segment and fft
    do logger_id=1,num_logger
        ! (1) load data
        filename = "Logger"+zfill(logger_id,2)+".tsv"
        ! (2) load data (t, Ax, Ay, Az)
        all_data = f%import(name=filename,num_column=4)
        ! (3) Divide data into segments
        all_segments = speana%to_segment(time_and_components=all_data,n=4096)
        
        ! (4) Tapering
        call speana%applyTaper(segments=all_segments,percent=5)
        ! (5) fft
        all_spectrum = speana%FourierAmplitude(all_segments)
        all_phase    = speana%FourierPhase(all_segments)
        ! (6) Export spectrum for all segments
        call speana%write(name="Logger"+zfill(logger_id,2)+"_fft",all_spectrum=all_spectrum)
        call speana%write(name="Logger"+zfill(logger_id,2)+"_pha",all_spectrum=all_phase)
        
    enddo



    direction = to_list("NS","EW","UD")
    

    ! <<<< WAVEFORM >>>>
    ! <<<< WAVEFORM >>>>
    ! import to plot
    do logger_id=1,num_logger
        ! load data
        filename = "Logger"+zfill(logger_id,2)+".tsv"
        ! x_axis
        all_data  = f%import(name=filename,num_column=4)
        
        do direction_id = 1,3
            
            plots(logger_id,direction_id) = analyst%to_plot(&
                x_list = all_data(:,1) ,&
                y_list = all_data(:,1+direction_id) , &
                x_label="Time (s)"      , &  ! x-axis label
                y_label="Acceleration (mg)", &  ! y-axis label
                title  = "Logger #"+str(logger_id)+" "+direction%get(direction_id), & ! Graph title
                with_line = .true. , & ! plot as line
                logscale = .false.  & ! disable logscale
            )
        enddo
    enddo
    ! <<<< WAVEFORM >>>>
    ! <<<< WAVEFORM >>>>

    ! to script
    pdf_wave    = analyst%to_pdf(plot=plots,row_per_page=3,option="confidential") 


    ! <<<< SPECTRUM >>>>
    ! <<<< SPECTRUM >>>>
    ! import to plot
    do logger_id=1,num_logger
        ! load data
        filename = "Logger"+zfill(logger_id,2)+"_fft*.tsv"
        ! x_axis
        all_data  = f%import(name=filename,num_column=4)
        
        ! plot data
        do direction_id = 1,3
            idx = [(i_i, i_i=1 + direction_id,size(all_data,2),4 )]
            
            plots(logger_id,direction_id) = analyst%to_plot(&
                x_list = all_data(:,1) ,&
                y_list = all_data(:,idx) , &
                x_label="Frequency (Hz)"      , &  ! x-axis label
                y_label="Fourier amplitude (mg sec)", &  ! y-axis label
                title  = "Logger #"+str(logger_id)+" "+direction%get(direction_id), & ! Graph title
                with_line = .true. , & ! plot as line
                logscale = .true.  & ! disable logscale
            )
        enddo
    enddo
    ! <<<< SPECTRUM >>>>
    ! <<<< SPECTRUM >>>>
    pdf_spec    = analyst%to_pdf(plot=plots,row_per_page=3,option="confidential")  


    ! <<<< PHASE >>>>
    ! <<<< PHASE >>>>
    ! import to plot
    do logger_id=1,num_logger
        ! load data
        filename = "Logger"+zfill(logger_id,2)+"_pha*.tsv"
        ! x_axis
        all_data  = f%import(name=filename,num_column=4)
        
        ! plot data
        do direction_id = 1,3
            idx = [(i_i, i_i=1 + direction_id,size(all_data,2),4 )]
            
            plots(logger_id,direction_id) = analyst%to_plot(&
                x_list = all_data(:,1) ,&
                y_list = all_data(:,idx) , &
                x_label="Frequency (Hz)"      , &  ! x-axis label
                y_label="Fourier phase (rad)", &  ! y-axis label
                title  = "Logger #"+str(logger_id)+" "+direction%get(direction_id), & ! Graph title
                with_line = .true. , & ! plot as line
                x_logscale = .true.  & ! disable logscale
            )
        enddo
    enddo
    ! <<<< PHASE >>>>
    ! <<<< PHASE >>>>

    ! to script
    pdf_pha    = analyst%to_pdf(plot=plots,row_per_page=3,option="confidential")  

    ! rendering
    call analyst%render(name="report.pdf",pdf=pdf_wave // pdf_spec // pdf_pha)
    
end program main