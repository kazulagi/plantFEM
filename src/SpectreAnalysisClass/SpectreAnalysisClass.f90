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

        real(real64) :: in1 = 0.0d0
        real(real64) :: in2 = 0.0d0
        real(real64) :: out1 = 0.0d0
        real(real64) :: out2 = 0.0d0


        complex(real64) :: in1_cmplx = 0.0d0
        complex(real64) :: in2_cmplx = 0.0d0
        complex(real64) :: out1_cmplx = 0.0d0
        complex(real64) :: out2_cmplx = 0.0d0

        
        complex(real64),allocatable :: FDD_S(:,:) !(singular-value idx,frequency)
        complex(real64),allocatable :: FDD_Phi(:,:,:) !(mode-idx, point-idx,frequency)

        logical :: initialized = .false.
    contains
        procedure,public :: init => initSpectreAnalysis
        procedure,public :: add  => addSpectreAnalysis
        procedure,public :: channel => channelSpectreAnalysis
        procedure,public :: plot    => plotSpectreAnalysis
        procedure,pass :: FFTSpectreAnalysis
        procedure,pass :: FFT_for_vector_SpectreAnalysis
        generic :: FFT => FFTSpectreAnalysis, FFT_for_vector_SpectreAnalysis
        procedure,public :: PSD => PSDSpectreAnalysis
        procedure,public :: PowerSpectrum => PowerSpectrumSpectreAnalysis
        procedure,public :: freq_axis => freq_axisSpectreAnalysis
        procedure,public :: FDD => FDD_SpectreAnalysis
        procedure,public :: export => exportSpectreAnalysis

        procedure,pass   :: bandpass_complex64_scalar_SpectreAnalysis
        procedure,pass   :: bandpass_complex64_SpectreAnalysis 
        procedure,pass   :: bandpass_real64_scalar_SpectreAnalysis
        procedure,pass   :: bandpass_real64_SpectreAnalysis

        generic,public :: bandpass => bandpass_complex64_scalar_SpectreAnalysis &
            ,bandpass_complex64_SpectreAnalysis  &
            ,bandpass_real64_scalar_SpectreAnalysis &
            ,bandpass_real64_SpectreAnalysis 

        procedure,pass :: cutifSpectreAnalysis
        procedure,pass :: cutif_loggers_SpectreAnalysis
        generic ::  cutif => cutifSpectreAnalysis,cutif_loggers_SpectreAnalysis
         
        ! create wave
        procedure,public :: whiteNoize => whiteNoizeSpectreAnalysis
        procedure,public :: time => timeSpectreAnalysis
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
    
    if(allocated(this%files) ) deallocate(this%files)
    if(allocated(this%num_lines) ) deallocate(this%num_lines)
    if(allocated(this%BOL) ) deallocate(this%BOL)
    if(allocated(this%FDD_S) ) deallocate(this%FDD_S)
    if(allocated(this%FDD_Phi) ) deallocate(this%FDD_Phi)

    allocate(this%files(this%max_file_num) )
    allocate(this%num_lines(this%max_file_num,2) )
    allocate(this%BOL(this%max_file_num) )

    this % in1 = 0.0d0
    this % in2 = 0.0d0
    this % out1 = 0.0d0
    this % out2 = 0.0d0

    this % in1_cmplx = 0.0d0
    this % in2_cmplx = 0.0d0
    this % out1_cmplx = 0.0d0
    this % out2_cmplx = 0.0d0

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
        print *, "[ERROR] Please initialize SpectreAnalysis, by %init(sampling_Hz)"
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


    if(.not. this%initialized)then
        print *, "[ERROR] Please initialize SpectreAnalysis, by %init(sampling_Hz)"
        stop
    endif

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

! ##########################################################
function FFT_for_vector_SpectreAnalysis(this,vector,Taper,Stacking,window_size) result(FFT_result)
    class(SpectreAnalysis_),intent(inout) :: this
    real(real64),intent(in) :: vector(:)
    integer(int32),intent(in) :: Taper    ! taper-ratio (0 to 100 )
    real(real32),intent(in)   :: Stacking(:)! size=number of stacking, i-th dataframes starts from stacking(i)
    integer(int32),intent(in) :: window_size ! 2^n

    integer(int32) :: i,j,n,data_id

    complex(complex64),allocatable :: FFT_result(:), wave(:),total_wave(:),fft_loc(:)


    if(.not. this%initialized)then
        print *, "[ERROR] Please initialize SpectreAnalysis, by %init(sampling_Hz)"
        stop
    endif

    ! get all data
    total_wave = vector

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


    if(.not. this%initialized)then
        print *, "[ERROR] Please initialize SpectreAnalysis, by %init(sampling_Hz)"
        stop
    endif

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


    if(.not. this%initialized)then
        print *, "[ERROR] Please initialize SpectreAnalysis, by %init(sampling_Hz)"
        stop
    endif

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
! ##########################################################

function bandpass_real64_SpectreAnalysis(this,x,freq_range) result(ft)
    class(SpectreAnalysis_),intent(inout) :: this
    real(real64),allocatable :: ft(:)
    real(real32),intent(in) :: freq_range(1:2)
    real(real64),intent(in) :: x(:)
    type(Math_) :: math
    real(real64) :: omega
    real(real64) :: alpha
    real(real64) :: a0
    real(real64) :: a1
    real(real64) :: a2
    real(real64) :: b0
    real(real64) :: b1
    real(real64) :: b2,samplerate,freq,bw

    integer(int32) :: i
    
    freq = minval(freq_range)
    bw = maxval(freq_range) - minval(freq_range)

    if(.not. this%initialized)then
        print *, "ERROR >> bandpassSpectreAnalysis >> please call %init(sampling_Hz)"
        return
    endif

    samplerate = this%sampling_Hz

    ! それぞれの変数は下記のとおりとする
    ! float samplerate … サンプリング周波数
    ! float freq … カットオフ周波数
    ! float bw   … 帯域幅


    omega = 2.0d0 * math%pi *  freq/samplerate;
    alpha = sin(omega) * sinh(log(2.0d0) / 2.0d0 * bw * omega / sin(omega));
    a0 =  1.0d0 + alpha;
    a1 = -2.0d0 * cos(omega);
    a2 =  1.0d0 - alpha;
    b0 =  alpha;
    b1 =  0.0d0;
    b2 = -alpha;

    ! https://www.utsbox.com/?page_id=523
    ! それぞれの変数は下記のとおりとする
    ! 　float input[]  …入力信号の格納されたバッファ。
    ! 　flaot output[] …フィルタ処理した値を書き出す出力信号のバッファ。
    ! 　int   size     …入力信号・出力信号のバッファのサイズ。
    ! 　float in1, in2, out1, out2  …フィルタ計算用のバッファ変数。初期値は0。
    ! 　float a0, a1, a2, b0, b1, b2 …フィルタの係数。 別途算出する。
    !for(int i = 0; i < size; i++)
    ft = zeros(size(x))
    do i=1,size(x)
    	! 入力信号にフィルタを適用し、出力信号として書き出す。
    	ft(i) = b0/a0 * x(i) + b1/a0 * this%in1  + b2/a0 * this%in2 - a1/a0 * this%out1 - a2/a0 * this%out2

        this%in2  = this%in1;       ! 2つ前の入力信号を更新
        this%in1  = x(i);  ! 1つ前の入力信号を更新

        this%out2 = this%out1;      ! 2つ前の出力信号を更新
        this%out1 = ft(i); ! 1つ前の出力信号を更新

    enddo
end function
! ##########################################################
function bandpass_complex64_SpectreAnalysis(this,x,freq_range) result(ft)
    class(SpectreAnalysis_),intent(inout) :: this
    complex(real64),allocatable :: ft(:)
    real(real32),intent(in) :: freq_range(1:2)
    complex(real64),intent(in) :: x(:)
    type(Math_) :: math
    real(real64) :: omega
    real(real64) :: alpha
    real(real64) :: a0
    real(real64) :: a1
    real(real64) :: a2
    real(real64) :: b0
    real(real64) :: b1
    real(real64) :: b2,samplerate,freq,bw

    integer(int32) :: i
    
    freq = minval(freq_range)
    bw = maxval(freq_range) - minval(freq_range)

    if(.not. this%initialized)then
        print *, "ERROR >> bandpassSpectreAnalysis >> please call %init(sampling_Hz)"
        return
    endif

    samplerate = this%sampling_Hz

    ! それぞれの変数は下記のとおりとする
    ! float samplerate … サンプリング周波数
    ! float freq … カットオフ周波数
    ! float bw   … 帯域幅


    omega = 2.0d0 * math%pi *  freq/samplerate;
    alpha = sin(omega) * sinh(log(2.0d0) / 2.0d0 * bw * omega / sin(omega));
    a0 =  1.0d0 + alpha;
    a1 = -2.0d0 * cos(omega);
    a2 =  1.0d0 - alpha;
    b0 =  alpha;
    b1 =  0.0d0;
    b2 = -alpha;

    ! https://www.utsbox.com/?page_id=523
    ! それぞれの変数は下記のとおりとする
    ! 　float input[]  …入力信号の格納されたバッファ。
    ! 　flaot output[] …フィルタ処理した値を書き出す出力信号のバッファ。
    ! 　int   size     …入力信号・出力信号のバッファのサイズ。
    ! 　float in1, in2, out1, out2  …フィルタ計算用のバッファ変数。初期値は0。
    ! 　float a0, a1, a2, b0, b1, b2 …フィルタの係数。 別途算出する。
    !for(int i = 0; i < size; i++)
    ft = zeros(size(x))
    do i=1,size(x)
    	! 入力信号にフィルタを適用し、出力信号として書き出す。
    	ft(i) = b0/a0 * x(i) + b1/a0 * this%in1_cmplx  + b2/a0 * this%in2_cmplx - a1/a0 * this%out1_cmplx - a2/a0 * this%out2_cmplx

        this%in2_cmplx  = this%in1_cmplx;       ! 2つ前の入力信号を更新
        this%in1_cmplx  = x(i);  ! 1つ前の入力信号を更新

        this%out2_cmplx = this%out1_cmplx;      ! 2つ前の出力信号を更新
        this%out1_cmplx = ft(i); ! 1つ前の出力信号を更新

    enddo
end function
! ##########################################################

! ##########################################################
function bandpass_complex64_scalar_SpectreAnalysis(this,x,freq_range) result(ft)
    class(SpectreAnalysis_),intent(inout) :: this
    complex(real64) :: ft
    real(real32),intent(in) :: freq_range(1:2)
    complex(real64),intent(in) :: x
    type(Math_) :: math
    real(real64) :: omega
    real(real64) :: alpha
    real(real64) :: a0
    real(real64) :: a1
    real(real64) :: a2
    real(real64) :: b0
    real(real64) :: b1
    real(real64) :: b2,samplerate,freq,bw

    integer(int32) :: i
    
    freq = minval(freq_range)
    bw = maxval(freq_range) - minval(freq_range)

    if(.not. this%initialized)then
        print *, "ERROR >> bandpassSpectreAnalysis >> please call %init(sampling_Hz)"
        return
    endif

    samplerate = this%sampling_Hz

    ! それぞれの変数は下記のとおりとする
    ! float samplerate … サンプリング周波数
    ! float freq … カットオフ周波数
    ! float bw   … 帯域幅


    omega = 2.0d0 * math%pi *  freq/samplerate;
    alpha = sin(omega) * sinh(log(2.0d0) / 2.0d0 * bw * omega / sin(omega));
    a0 =  1.0d0 + alpha;
    a1 = -2.0d0 * cos(omega);
    a2 =  1.0d0 - alpha;
    b0 =  alpha;
    b1 =  0.0d0;
    b2 = -alpha;

    ! https://www.utsbox.com/?page_id=523
    ! それぞれの変数は下記のとおりとする
    ! 　float input[]  …入力信号の格納されたバッファ。
    ! 　flaot output[] …フィルタ処理した値を書き出す出力信号のバッファ。
    ! 　int   size     …入力信号・出力信号のバッファのサイズ。
    ! 　float in1, in2, out1, out2  …フィルタ計算用のバッファ変数。初期値は0。
    ! 　float a0, a1, a2, b0, b1, b2 …フィルタの係数。 別途算出する。
    !for(int i = 0; i < size; i++)
    
    
	! 入力信号にフィルタを適用し、出力信号として書き出す。
	ft = b0/a0 * x + b1/a0 * this%in1_cmplx  + b2/a0 * this%in2_cmplx - a1/a0 * this%out1_cmplx - a2/a0 * this%out2_cmplx
    this%in2_cmplx  = this%in1_cmplx;       ! 2つ前の入力信号を更新
    this%in1_cmplx  = x;  ! 1つ前の入力信号を更新
    this%out2_cmplx = this%out1_cmplx;      ! 2つ前の出力信号を更新
    this%out1_cmplx = ft; ! 1つ前の出力信号を更新

    
end function
! ##########################################################

! ##########################################################
function bandpass_real64_scalar_SpectreAnalysis(this,x,freq_range) result(ft)
    class(SpectreAnalysis_),intent(inout) :: this
    real(real64) :: ft
    real(real32),intent(in) :: freq_range(1:2)
    real(real64),intent(in) :: x
    type(Math_) :: math
    real(real64) :: omega
    real(real64) :: alpha
    real(real64) :: a0
    real(real64) :: a1
    real(real64) :: a2
    real(real64) :: b0
    real(real64) :: b1
    real(real64) :: b2,samplerate,freq,bw

    integer(int32) :: i
    
    freq = minval(freq_range)
    bw = maxval(freq_range) - minval(freq_range)

    if(.not. this%initialized)then
        print *, "ERROR >> bandpassSpectreAnalysis >> please call %init(sampling_Hz)"
        return
    endif

    samplerate = this%sampling_Hz

    ! それぞれの変数は下記のとおりとする
    ! float samplerate … サンプリング周波数
    ! float freq … カットオフ周波数
    ! float bw   … 帯域幅


    omega = 2.0d0 * math%pi *  freq/samplerate;
    alpha = sin(omega) * sinh(log(2.0d0) / 2.0d0 * bw * omega / sin(omega));
    a0 =  1.0d0 + alpha;
    a1 = -2.0d0 * cos(omega);
    a2 =  1.0d0 - alpha;
    b0 =  alpha;
    b1 =  0.0d0;
    b2 = -alpha;

    ! https://www.utsbox.com/?page_id=523
    ! それぞれの変数は下記のとおりとする
    ! 　float input[]  …入力信号の格納されたバッファ。
    ! 　flaot output[] …フィルタ処理した値を書き出す出力信号のバッファ。
    ! 　int   size     …入力信号・出力信号のバッファのサイズ。
    ! 　float in1, in2, out1, out2  …フィルタ計算用のバッファ変数。初期値は0。
    ! 　float a0, a1, a2, b0, b1, b2 …フィルタの係数。 別途算出する。
    !for(int i = 0; i < size; i++)
    
    
	! 入力信号にフィルタを適用し、出力信号として書き出す。
	ft = b0/a0 * x + b1/a0 * this%in1_cmplx  + b2/a0 * this%in2_cmplx - a1/a0 * this%out1_cmplx - a2/a0 * this%out2_cmplx
    this%in2_cmplx  = this%in1_cmplx;       ! 2つ前の入力信号を更新
    this%in1_cmplx  = x;  ! 1つ前の入力信号を更新
    this%out2_cmplx = this%out1_cmplx;      ! 2つ前の出力信号を更新
    this%out1_cmplx = ft; ! 1つ前の出力信号を更新

    
end function
! ##########################################################


function whiteNoizeSpectreAnalysis(this,n,t) result(x)
    class(SpectreAnalysis_),intent(in) :: this
    real(real64),allocatable,optional,intent(inout) :: t(:)
    real(real64),allocatable :: x(:)

    integer(int32),intent(in) :: n

    integer(int32) :: i
    type(Random_) :: random
    
    if(.not. this%initialized)then
        print *, "ERROR >> whiteNoizeSpectreAnalysis >> please call %init(sampling_Hz)"
        return
    endif

    x = zeros(n)
    if(present(t) )then
        t = linspace([0.0d0,dble(size(x))/dble(this%sampling_Hz)],size(x) )
    endif

    do i=1,size(x)
        x(i) = random%gauss(mu=0.0d0,sigma=1.0d0)
    enddo

end function
! ##########################################################
function timeSpectreAnalysis(this,n) result(t)
    class(SpectreAnalysis_),intent(in) :: this
    real(real64),allocatable :: t(:)

    integer(int32),intent(in) :: n

    integer(int32) :: i
    type(Random_) :: random
    
    if(.not. this%initialized)then
        print *, "ERROR >> whiteNoizeSpectreAnalysis >> please call %init(sampling_Hz)"
        return
    endif

    t = linspace([0.0d0,dble(n)/dble(this%sampling_Hz)],n)
    
end function


! ##########################################################
function cutifSpectreAnalysis(this,x_t,window_size,sigma) result(x_r)
    class(SpectreAnalysis_),intent(in) :: this
    real(real64),intent(in) :: x_t(:)
    
    real(real64),allocatable :: dummy_x(:),x_r(:)
    integer(int32),intent(in) :: window_size
    real(real64),optional,intent(in) :: sigma
    real(real64) :: sd,ave
    integer(int32),allocatable :: active_segment(:)
    integer(int32) :: i,from,to
    type(Random_) :: random

    if(present(sigma))then
        ! remove if 
        ! 全体のsdをとり，
        sd  = standardDeviation(x_t)
        ave = average(x_t)
        ! ホワイトガウスノイズ生成
        
        do i=1,size(x_t)/window_size
            from = (i-1)*window_size + 1
            to =  i   *window_size 
            dummy_x = x_t( from:to )
            if(standardDeviation(dummy_x) > sigma*sd )then 
                cycle
            else
                if(.not.allocated(x_r) )then
                    x_r = dummy_x
                else
                    x_r = x_r // dummy_x
                endif
            endif
        enddo
    endif



end function


! ##########################################################
function cutif_loggers_SpectreAnalysis(this,x_t,window_size,sigma,log) result(x_r)
    class(SpectreAnalysis_),intent(in) :: this
    real(real64),intent(in) :: x_t(:,:) ! t, channel
    character(*),optional,intent(in) :: log
    
    real(real64),allocatable :: dummy_x(:),x_r(:,:)
    integer(int32),intent(in) :: window_size
    real(real64),optional,intent(in) :: sigma
    
    integer(int32),allocatable :: active_segment(:),remove_segment_ids(:)
    integer(int32) :: i,from,to,itr,j
    type(Random_) :: random
    real(real64),allocatable :: sd_data(:,:),sd(:),ave(:),sd_vector(:),zero_window(:,:)
    type(IO_) :: log_original,log_remained
    if(present(sigma))then
        ! remove if 
        ! 全体のsdをとり，
        remove_segment_ids = int(zeros(size(x_t,1)/window_size ) )
        sd  = zeros(size(x_t,2 ))
        ave = zeros(size(x_t,2 ))
        do i=1,size(x_t,2)
            sd(i)  = standardDeviation(x_t(:,i) )
            ave(i) = average(x_t(:,i) )
        enddo

        sd_data = zeros(size(x_t)/window_size,size(x_t,2))
        
        do i=1,size(x_t,1)/window_size
            from = (i-1)*window_size + 1
            to =  i   *window_size 
            
            do j=1,size(x_t,2)
                dummy_x = x_t( from:to,j )
                sd_data(i,j) = standardDeviation(dummy_x)
            enddo
        enddo

        do i=1,size(x_t,1)/window_size
            from = (i-1)*window_size + 1
            to =  i   *window_size 
            
            sd_vector = sd_data(i,:)
            if( maxval(sd_vector-sd*sigma) > 0.0d0 )then
                remove_segment_ids(i) = 1
            endif
        enddo
        

        if(maxval(remove_segment_ids)==0 )then
            deallocate(remove_segment_ids)
        endif

        if(present(log) )then
            call log_original%open(log+"_original_wave.csv")
            call log_remained%open(log+"_remained_wave.csv")
        endif
        
        if(allocated(remove_segment_ids) )then
            x_r = eyes( size(x_t,1)-window_size*sum(remove_segment_ids),size(x_t,2)  )
            zero_window = zeros( window_size,size(x_t,2) )
            from = 1
            to   = 0
            do i=1,size(x_t,1)/window_size
                if( remove_segment_ids(i)==1 )then
                    if(present(log) )then
                        call log_remained%write(zero_window,separator=", ")
                    endif
                    cycle
                else
                    to = from -1 +window_size
                    x_r(from:to,:)= x_t( window_size*(i-1)+1:window_size*i , :) 
                    from = from + window_size
                    if(present(log) )then
                        call log_remained%write(x_r(from:to,:),separator=", ")
                    endif
                endif
            enddo
            
            
            x_r(from:,:) = x_t(window_size*int(size(x_t,1)/window_size)+1:,:)
            if(present(log) )then
                call log_original%write(x_t,separator=", ")
            endif

        else
            x_r = x_t
            if(present(log) )then
                call log_original%write(x_t,separator=", ")
                call log_remained%write(x_r,separator=", ")
            endif
        endif


        if(present(log) )then
            call log_original%flush()
            call log_remained%flush()
            call log_original%close()
            call log_remained%close()
        endif
    endif



end function


end module SpectreAnalysisClass
