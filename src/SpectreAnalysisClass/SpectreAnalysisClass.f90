module SpectreAnalysisClass
    use IOClass
    use MathClass
    use ArrayClass
    implicit none

    type :: SpectreAnalysis_
        type(IO_) :: display
        type(IO_),allocatable :: files(:)
        integer(int32),allocatable :: num_lines(:,:),BOL(:)
        real(real32)   :: sampling_Hz
        integer(int32) :: max_file_num = 10000
        integer(int32) :: last_file_id 
        real(real64),allocatable :: Freq(:)

        complex(real64),allocatable :: FDD_S(:,:) !(singular-value idx,frequency)
        complex(real64),allocatable :: FDD_Phi(:,:,:) !(mode-idx, point-idx,frequency)

        logical :: initialized = .false.
    contains
        procedure,public :: init => initSpectreAnalysis
        procedure,public :: add  => addSpectreAnalysis
        procedure,public :: channel => channelSpectreAnalysis
        procedure,public :: plot    => plotSpectreAnalysis
        procedure,public :: FFT => FFTSpectreAnalysis
        procedure,public :: PSD => PSDSpectreAnalysis
        procedure,public :: PowerSpectrum => PowerSpectrumSpectreAnalysis
        procedure,public :: freq_axis => freq_axisSpectreAnalysis
        procedure,public :: FDD => FDD_SpectreAnalysis
        procedure,public :: export => exportSpectreAnalysis
        
    end type

contains


! ##########################################################
subroutine initSpectreAnalysis(this,sampling_Hz,max_file_num)
    class(SpectreAnalysis_),intent(inout) :: this
    real(real32),intent(in) :: sampling_Hz
    integer(int32),optional,intent(in) :: max_file_num
    
    this%sampling_Hz = sampling_Hz
    if(present(max_file_num) )then
        this%max_file_num = max_file_num
    endif

    if(allocated(this%files) )then
        deallocate(this%files)
    endif

    if(allocated(this%num_lines) )then
        deallocate(this%num_lines)
    endif
    
    allocate(this%files(this%max_file_num) )
    allocate(this%num_lines(this%max_file_num,2) )
    allocate(this%BOL(this%max_file_num) )
    this%last_file_id = 0

    this%initialized = .true.

end subroutine
! ##########################################################


! ##########################################################
subroutine addSpectreAnalysis(this,name,lines,BOL)
    class(SpectreAnalysis_),intent(inout) :: this
    character(*),intent(in) :: name
    integer(int32),intent(in) :: lines(1:2), BOL !Beginning of line

    if(.not. this%initialized)then
        print *, "Please initialize SpectreAnalysis, by %init(sampling_Hz)"
        stop
    endif

    if(access(name,"r")/=0 )then
        print *, "[Caution!] File :: ",name,"[404]"
        return
    endif

    this%files(this%last_file_id + 1)%filename = name
    this%num_lines(this%last_file_id + 1,1:2) = lines(1:2) 
    this%BOL(this%last_file_id + 1) = BOL

    this%last_file_id = this%last_file_id + 1

end subroutine
! ##########################################################

! ##########################################################
function channelSpectreAnalysis(this,channel) result(vec_data)
    class(SpectreAnalysis_),intent(inout) :: this
    integer(int32),intent(in) :: channel
    integer(int32) :: i,j,numline,n,loc_data_id
    integer(int32),allocatable :: starts_from(:) 
    real(real64),allocatable :: vec_data(:),row_data(:)
    character(:),allocatable :: line


    allocate(row_data(channel) )
    numline = 0
    allocate(starts_from(this%last_file_id) )
    do i=1,this%last_file_id
        numline = numline + this%num_lines(i,2) - this%num_lines(i,1) + 1  
        starts_from(i) = numline - ( this%num_lines(i,2)- this%num_lines(i,1))
    enddo


    if(channel==0)then
        vec_data = linspace([1.0d0/dble(this%sampling_Hz),1.0d0/dble(this%sampling_Hz)*dble(numline)  ],numline)
        return
    endif

    allocate(vec_data(numline) )
    vec_data(:) = 0.0d0

    !!$OMP parallel do default(shared) private(n, i, loc_data_id, line,row_data)
    do i=1,this%last_file_id

        call this%files(i)%open(this%files(i)%filename)
        n = 0
        loc_data_id = 0
        do  
            n = n + 1
            if(this%files(i)%EOF ) exit
            line = this%files(i)%readline()
            

            if( n < this%num_lines(i,1)  .or. this%num_lines(i,2) < n )then
                cycle
            else
                loc_data_id = loc_data_id + 1
                read(line(this%BOL(i): ),*) row_data(1:channel)
                vec_data(starts_from(i)-1 + loc_data_id ) = row_data(channel)
            endif
        enddo
        call this%files(i)%close()
    enddo
    !!$OMP end parallel do
end function
! ##########################################################


subroutine plotSpectreAnalysis(this,channels)
    class(SpectreAnalysis_),intent(inout) :: this
    integer(int32),intent(in) :: channels(:)


end subroutine
! ##########################################################


! ##########################################################
function FFTSpectreAnalysis(this,channel,Taper,Stacking,window_size) result(FFT_result)
    class(SpectreAnalysis_),intent(inout) :: this
    integer(int32),intent(in) :: channel
    integer(int32),intent(in) :: Taper    ! taper-ratio (0 to 100 )
    real(real32),intent(in)   :: Stacking(:)! size=number of stacking, i-th dataframes starts from stacking(i)
    integer(int32),intent(in) :: window_size ! 2^n
    integer(int32) :: i,j,n,data_id

    complex(complex64),allocatable :: FFT_result(:), wave(:),total_wave(:),fft_loc(:)

    ! get all data
    total_wave = this%channel(channel)

    if(window_size > size(total_wave) )then
        print *, "ERROR :: FFTSpectreAnalysis >>  window size",window_size," is too large"
        print *, "Data size is ",size(total_wave)
        stop
    endif

    if(window_size+int(maxval(stacking)*this%sampling_Hz) > size(total_wave) )then
        print *, "ERROR :: FFTSpectreAnalysis >>  window size",window_size," is too large"
        print *, "this operation needs",window_size+int(maxval(stacking)*this%sampling_Hz)," samples"
        print *, "but you only have",size(total_wave)," data plots"
        stop
    endif

    FFT_result = zeros(window_size)
    
    do i=1,size(stacking)
        data_id = int(Stacking(i)*this%sampling_Hz)
        if(data_id<=0)then
            data_id = 1
        endif
        wave = total_wave(data_id:data_id+window_size-1)
        wave(:) = wave(:) - average(dble(wave))
        wave = wave(:)*taper_function(n=size(wave),percent=taper)
        fft_loc = FFT(wave )
        FFT_result = FFT_result + sqrt( dble(fft_loc*conjg(fft_loc)) ) 
    enddo

    FFT_result = FFT_result(1:window_size/2)
    FFT_result = FFT_result/dble(size(stacking))
end function
! ##########################################################

function taper_function(n,percent) result(taper_f)
    integer(int32),intent(in) :: n, percent
    real(real64),allocatable :: taper_f(:)
    integer(int32) :: i
    ! taper window
    taper_f = eyes(n)
    do i=1,int(dble(percent)/100.0d0*dble(n))
        taper_f(i) = taper_f(i)*dble(i-1)/(dble(percent)/100.0d0*dble(n))
        taper_f(n-i+1) = taper_f(n-i+1)*dble(i-1)/(dble(percent)/100.0d0*dble(n))
    enddo

end function

! ##########################################################
function PowerSpectrumSpectreAnalysis(this,channel,Taper,Stacking,window_size) result(PowerSpectrum)
    class(SpectreAnalysis_),intent(inout) :: this
    integer(int32),intent(in) :: channel
    integer(int32),intent(in) :: Taper    ! taper-ratio (0 to 100 )
    real(real32),intent(in)   :: Stacking(:)! size=number of stacking, i-th dataframes starts from stacking(i)
    integer(int32),intent(in) :: window_size ! 2^n
    integer(int32) :: i,j,n,data_id
    real(real64) :: T

    complex(complex64),allocatable :: FFT_result(:), wave(:)
    real(real64),allocatable :: PowerSpectrum(:)

    FFT_result = this%FFT(&        
            channel = channel,&
            Taper   = Taper,&
            Stacking= Stacking,&
            window_size = window_size &
        )


    T = dble(window_size*this%sampling_Hz)
    PowerSpectrum = dble(FFT_result(:)*conjg(FFT_result(:)))/T

end function
! ##########################################################



function freq_axisSpectreAnalysis(this,window_size) result(freq)
    class(SpectreAnalysis_),intent(in) :: this
    integer(int32),intent(in) :: window_size
    real(real64),allocatable :: freq(:)
    real(real64) :: df
    df   = 1.0d0/(window_size/this%sampling_Hz)
    freq = linspace( [df, dble(this%sampling_Hz/2)], window_size/2) 

end function


! ###########################################################
subroutine FDD_SpectreAnalysis(this,wave_data,window_size,channel,taper,stacking)  
	class(SpectreAnalysis_),intent(inout) :: this
    type(SpectreAnalysis_),intent(inout) :: wave_data(:)

    real(real64),allocatable :: freq(:)
    complex(real64),allocatable :: FourierSpectrum(:,:),PSD(:,:,:)
    integer(int32),intent(in) :: channel
    

    integer(int32),intent(in) :: Taper    ! taper-ratio (0 to 100 )
    real(real32),intent(in)   :: Stacking(:)! size=number of stacking, i-th dataframes starts from stacking(i)
    integer(int32),intent(in) :: window_size ! 2^n
    
    integer(int32) :: i,freq_idx,n_data

    ! FOR LAPACK
    INTEGER M, N, LDA, LDU, LDV, LWORK, LRWORK,INFO
    logical :: INFO2
    INTEGER,allocatable :: IWORK(:)
    REAL (real64),allocatable :: SVA(:), RWORK(:)
    COMPLEX (real64),allocatable :: A(:,:), U(:,:), V(:,:), CWORK(:)
    CHARACTER(1) JOBA, JOBU, JOBV, JOBR, JOBT, JOBP
    real(real64) :: df
    df   = 1.0d0/(window_size/this%sampling_Hz)


    ! PSD用のFunction作ったほうがよいか？
    PSD = this%PSD(channel,Taper,Stacking,window_size,wave_data)
    this%freq = linspace( [df, dble(this%sampling_Hz/2)], window_size/2) 
    
    n_data = size(wave_data)
    
    this%FDD_S   = zeros(n_data, window_size/2)
    this%FDD_Phi = zeros(n_data,n_data, window_size/2)
    
	do freq_idx = 1,window_size/2
        ! 特異値分解
        JOBA = "C"
        JOBU = "U"
        JOBV = "V"
        JOBR = "N"
        JOBT = "N"
        JOBP = "N"
        M    = size(PSD,1)
        N    = size(PSD,2)
        A    = PSD(:,:,freq_idx)
        LDA  = M
        SVA  = zeros(M)
        U    = zeros(M,N)
        LDU  = M
        V    = zeros(N,N)
        LDV  = N
        LWORK = 6*N + 2*N*N
        CWORK = zeros(LWORK)

        LRWORK = N+2*M
        RWORK  = zeros(LRWORK)

        IWORK = zeros(M + 3*N)

        INFO  = 0
        !call print(real(A) )
        !call print(">> ")
        !call print(imag(A) )
        !call print(">> ")
        
        call ZGEJSV(JOBA, JOBU, JOBV, JOBR, JOBT, JOBP, M, N, A, LDA, &
            SVA, U, LDU, V, LDV, CWORK, LWORK, RWORK, LRWORK, &
            IWORK, INFO)

        this%FDD_Phi(:,:,freq_idx) = V(:,:)
        this%FDD_S(:,freq_idx) = SVA
    enddo


end subroutine
! ###########################################################

! ##########################################################
function PSDSpectreAnalysis(this,channel,Taper,Stacking,window_size,wave_data) result(PSD_result)
    class(SpectreAnalysis_),intent(inout) :: this
    type(SpectreAnalysis_),intent(inout) :: wave_data(:)
    integer(int32),intent(in) :: channel
    integer(int32),intent(in) :: Taper    ! taper-ratio (0 to 100 )
    real(real32),intent(in)   :: Stacking(:)! size=number of stacking, i-th dataframes starts from stacking(i)
    integer(int32),intent(in) :: window_size ! 2^n
    integer(int32) :: i,j,n,data_id,data_idx,data_idx_I,data_idx_J

    complex(complex64),allocatable :: FFT_result(:), wave(:),total_waves(:,:),fft_loc(:),&
    PSD_result(:,:,:),wave_I(:),wave_J(:),fft_loc_I(:),fft_loc_J(:)

    ! get all data
    total_waves = zeros(size(wave_data(1)%channel(channel)) , size(wave_data))
    do data_idx = 1,size(wave_data)
        total_waves(:,data_idx) = wave_data(data_idx)%channel(channel)
    enddo
    
    ! check data size
    if(window_size > size(total_waves,1) )then
        print *, "ERROR :: PSDSpectreAnalysis >>  window size",window_size," is too large"
        print *, "Data size is ",size(total_waves,1)
        stop
    endif

    if(window_size+int(maxval(stacking)*this%sampling_Hz) > size(total_waves,1) )then
        print *, "ERROR :: PSDSpectreAnalysis >>  window size",window_size," is too large"
        print *, "this operation needs",window_size+int(maxval(stacking)*this%sampling_Hz)," samples"
        print *, "but you only have",size(total_waves)," data plots"
        stop
    endif

    !
    FFT_result = zeros(window_size)
    ! G_{xx}{f}: PSD
    PSD_result = zeros(size(wave_data),size(wave_data),window_size)
    !$OMP parallel private(data_idx_J,data_id,wave_I,wave_J,fft_loc_I,fft_loc_J)
    !$OMP do
    do data_idx_I=1,size(wave_data)
        do data_idx_J=1,size(wave_data)
            do i=1,size(stacking)
                data_id = int(Stacking(i)*this%sampling_Hz)
                if(data_id<=0)then
                    data_id = 1
                endif
                
                wave_I = total_waves(data_id:data_id+window_size-1,data_idx_I)

                wave_I(:) = wave_I(:) - average(dble(wave_I))
                wave_I = wave_I(:)*taper_function(n=size(wave_I),percent=taper)
                fft_loc_I = FFT(wave_I )

                wave_J = total_waves(data_id:data_id+window_size-1,data_idx_J)
                wave_J(:) = wave_J(:) - average(dble(wave_J))
                wave_J = wave_J(:)*taper_function(n=size(wave_J),percent=taper)
                fft_loc_J = FFT(wave_J )
                PSD_result(data_idx_I,data_idx_J,:) =   PSD_result(data_idx_I,data_idx_J,:) &
                    + fft_loc_I(:)*conjg(fft_loc_J(:) )

            enddo
        enddo
    enddo
    !$OMP end do
    !$OMP end parallel

    PSD_result = PSD_result(:,:,1:window_size/2)
    PSD_result = PSD_result/dble(size(stacking))

end function
! ##########################################################


subroutine exportSpectreAnalysis(this,name)
    class(SpectreAnalysis_),intent(in) :: this
    character(*),intent(in) :: name
    type(IO_) :: f
    integer(int32) :: freq_idx, mode_idx

    if(allocated(this%FDD_S) )then
        do mode_idx=1,size(this%FDD_S,1)
            call f%open(name+"_FDD_mode_"+zfill(mode_idx,5),"w")
            ! first mode
            do freq_idx=1,size(this%Freq)
                write(f%fh,* ) this%Freq(freq_idx), real(this%FDD_S(mode_idx,freq_idx))
            enddo
            call f%close()
        enddo
    endif

end subroutine
! ##########################################################

end module SpectreAnalysisClass
