module Tank_TankModelClassClass
    use IOClass
    use ArrayClass
    use RandomClass
    implicit none
    
    

    type :: Tank_
        ! タンクの仕様:
        ! 内部変数として水位をもつ．
        ! パラメータとして，流出孔係数群をもつ．
        character(:),allocatable :: name
        ! 流出孔係数
        real(real64),allocatable   :: coeff(:)

        ! 流出孔高さ
        real(real64),allocatable   :: height(:)

        ! 流出フラックス
        real(real64),allocatable   :: runoff(:)

        ! タンクから直接流入
        real(real64)   :: P = 0.0d0

        ! タンクから直接流出
        real(real64)   :: E = 0.0d0
        
        ! n, n+1ステップ目の水位の高さ
        real(real64) :: S_n = 0.0d0
        real(real64) :: S   = 0.0d0
        
        ! 他のタンクへと接続できる．(tank pointerをもつ)
        real(real64) :: out_coeff = 0.0d0
        real(real64) :: out_height = 0.0d0
        type(Tank_),pointer :: out => null()

        ! 水頭計算用: タンク底のGL
        real(real64) :: GL_bottom = 0.0d0
        

        type(Tank_),pointer :: in => null()

    contains
        procedure,public :: init => initTank_TankModelClass
        procedure,public :: connect => connectTank_TankModelClass
        procedure,public :: update => updateTank_TankModelClass
        procedure,public :: to_waterhead => to_waterhead_Tank_TankModelClass
    end type


    type :: TankModel_
        type(Tank_),allocatable :: tanks(:)
    contains
        procedure,public :: init    => initTankModel_TankModelClass
        procedure,pass   :: addTankModel_TankModelClass
        !procedure,pass   :: addTankModel_by_config_TMC
        generic,public :: add     => addTankModel_TankModelClass


        procedure,public :: connect => connectTankModel_TankModelClass
        procedure,public :: optimize => optimizeTankModel_TankModelClass
        procedure,public :: simulate => simulateTankModel_TankModelClass
        procedure,public :: setParams => setParamsTankModel_TankModelClass

        ! check unit for space, time and force.
        procedure,public :: showUnit => showUnit_TankModelClass
        procedure,public :: show => show_TankModelClass
    end type

    interface simulate
        module procedure simulate_Tank_TankModelClass
    end interface simulate

    interface optimize
        module procedure optimize_Tank_TankModelClass
    end interface optimize


    interface get_params
        module procedure get_params_tank_TankModelClassclass
    end interface

    interface operator(.with.)
        module procedure tanks_with_parameters
    end interface 

    interface to_Tank
        module procedure to_tank_TankModelClass
    end interface

    
contains

subroutine initTank_TankModelClass(this,coeff,height,GL_bottom,name)
    class(Tank_),intent(inout) :: this
    real(real64),intent(in) :: coeff(:),height(:),GL_bottom
    character(*),intent(in) :: name

    this%coeff  = coeff
    this%height = height
    this%runoff = zeros(size(height))
    this%S_n   = 0.0d0
    this%S     = 0.0d0
    this%P     = 0.0d0
    this%E     = 0.0d0
    this%GL_bottom = GL_bottom
    this%out   => null()
    this%in    => null()
    this%name = name
    

end subroutine
! #################################


! #################################
function to_tank_TankModelClass(coeff,height,GL_bottom,name) result(ret_tank)
    type(Tank_) :: ret_tank
    real(real64),intent(in) :: coeff(:),height(:),GL_bottom
    character(*),intent(in) :: name

    call ret_tank%init(coeff=coeff,height=height,GL_bottom=GL_bottom,name=name)

end function
! #################################


! #################################
subroutine initTankModel_TankModelClass(this)
    class(TankModel_),intent(inout) :: this
    integer(int32) :: num_tank

    if(allocated(this%tanks) )then
        deallocate(this%tanks)
    endif

end subroutine
! #################################


! #################################
subroutine addTankModel_TankModelClass(this,tank)
    class(TankModel_),intent(inout) :: this
    type(Tank_),intent(in) :: tank
    type(Tank_),allocatable :: buf(:)
    integer(int32) :: num_tank

    if(.not.allocated(this%tanks) )then
        allocate(this%tanks(1) )
        this%tanks(1) = tank
    else
        buf = this%tanks
        deallocate(this%tanks)
        allocate(this%tanks(size(buf)) )
        this%tanks(1:size(buf)) = buf(:)
        this%tanks(size(buf)+1) = tank
    endif
    

end subroutine
! #################################


! #################################
!subroutine addTankModel_by_config_TMC(this,config)
!
!end subroutine
! #################################


! #################################
subroutine connectTankModel_TankModelClass(this,tankIdx,coeff,height)
    class(TankModel_),intent(inout) :: this
    type(Tank_),allocatable :: buf(:)
    integer(int32),intent(in) :: tankIdx(1:2)
    integer(int32) :: i,j
    real(real64),intent(in) :: coeff,height

    i = tankIdx(1)
    j = tankIdx(2)
    call this%tanks(i)%connect(tank=this%tanks(j),coeff=coeff,height=height)

end subroutine

! #################################

! #################################
function optimizeTankModel_TankModelClass(this,rainfall,waterheads,GLs,dt) result(params)
    class(TankModel_),intent(inout) :: this
    real(real64),intent(in) :: rainfall(:),waterheads(:,:),GLs(:),dt
    real(real64),allocatable :: params(:)
    
    params = optimize(tanks=this%tanks,rainfall=rainfall,waterheads=waterheads,GLs=GLs,dt=dt)

end function

! #################################
function simulateTankModel_TankModelClass(this,rainfall,GLs,dt) result(waterheads)
    class(TankModel_),intent(inout) :: this
    real(real64),intent(in) :: rainfall(:),GLs(:),dt
    real(real64),allocatable :: waterheads(:,:)
    
    waterheads = simulate(tanks=this%tanks,GLs=GLs,dt=dt,rainfall=rainfall)

end function


! #################################
subroutine setParamsTankModel_TankModelClass(this,params) 
    class(TankModel_),intent(inout) :: this
    real(real64),intent(in) :: params(:)
    
    this%tanks = this%tanks .with. params

end subroutine

! #################################
subroutine connectTank_TankModelClass(this,tank,coeff,height)
    class(Tank_),target,intent(inout) :: this
    type(Tank_),target,intent(inout) :: tank
    real(real64),intent(in) :: coeff,height

    this%out => tank
    tank%in  => this
    this%out_coeff = coeff
    this%out_height = height


end subroutine

! #################################

! ###############################################################
subroutine updateTank_TankModelClass(this,dt)
    class(Tank_),intent(inout) :: this
    real(real64),intent(in) :: dt
    real(real64),allocatable :: Q(:)
    real(real64) :: out_val,out_q,in_val,in_q
    integer(int32) :: i

    allocate(Q(size(this%coeff)) )
    do i=1,size(this%coeff)
        Q(i) = maxval([0.0d0,this%coeff(i)*(this%S_n - this%height(i)) ])
        this%runoff(i) = Q(i)
    enddo
    !       
    if(.not. associated(this%in)) then
        in_q = 0.0d0
    else
        in_q = maxval([0.0d0,this%in%out_coeff*(this%in%S_n - this%in%out_height) ])
    endif

    if(.not. associated(this%out)) then
        out_q = 0.0d0
    else
        out_q = maxval([0.0d0,this%out_coeff*(this%S_n - this%out_height) ])
    endif

    this%S = this%S_n + this%P*dt - this%E*dt - sum(Q(:))*dt + in_q*dt - out_q*dt
    
    this%S_n = this%S

end subroutine
! ###############################################################


function simulate_Tank_TankModelClass(tanks,GLs,dt,rainfall) result(waterhead)
    type(Tank_),intent(inout) :: tanks(:)
    real(real64),intent(in) :: GLs(:),dt
    real(real64),intent(in) :: rainfall(:) ! mm/s
    real(real64),allocatable :: waterhead(:,:)
    integer(int32) :: max_timestep,i,t_step

    max_timestep = size(rainfall)
    allocate(waterhead( max_timestep,size(GLs)) )
    
    do t_step=1,max_timestep
        ! 1st tank is the top tank
        tanks(1)%P = rainfall(t_step)
        
        !if (tanks(1)%S_n < tanks(1)%E*dt)then
        !    tanks(1)%E = 0.0d0
        !endif

        do i=1,size(tanks)
            call tanks(i)%update(dt=dt)
        enddo
        
        do i=1,size(GLs)
            waterhead(t_step,i) = tanks(1)%to_waterhead(GL=GLs(i))
        enddo
        !write(f%fh,*) dt*i_i/60.0/60.0d0/24.0d0, tanks(1)%S, tanks(2)%S, tanks(1)%runoff(:),tanks(2)%runoff(:)
        ! -10cm点の水頭(-mm)を返す．
        !write(fw%fh,*) dt*i_i/60.0/60.0d0/24.0d0, tanks(1)%to_waterhead(GL=-100.0d0)
        !write(fp%fh,*) dt*i_i/60.0/60.0d0/24.0d0, tanks(1)%P*60*60 ! mm/hour
        
    enddo

end function



! ###############################################################
function optimize_Tank_TankModelClass(tanks,rainfall,waterheads,GLs,dt,debug) result(final_params)
    type(Tank_),intent(inout) :: tanks(:)
    real(real64),intent(in) :: waterheads(:,:),rainfall(:),GLs(:),dt
    logical,optional,intent(in) :: debug

    real(real64),allocatable :: params(:), trial_params(:,:),residuals(:),buf(:,:),this_waterheads(:,:),&
        this_params(:),final_params(:),last_residuals(:)
    real(real64) :: sigma
    type(Tank_),allocatable :: this_tanks(:)
    integer(int32) :: num_params,i,j, max_itr,itr,num_trial_params,same_count
    type(Random_) :: random
    
    max_itr = 100
    num_trial_params = 100 ! it should be greater than 2
    sigma = 0.20d0
    ! optimize parameters to reproduce rainfall-waterhead response
    ! initial params
    params = get_params(tanks)
    num_params = size(params)
    ! if zero exists, change
    do i=1,size(params)
        if (params(i)<=0.0d0)then
            params(i) = dble(1.0e-16)
        endif
    enddo

    ! optimization
    do itr=1,max_itr
        trial_params = zeros(num_trial_params,num_params)
        residuals        = zeros(num_trial_params)      
        
        

        ! generate trial params
        do i=1,num_trial_params
            trial_params(i,:) = abs(params(:) * random%gauss(mu=1.0d0,sigma=sigma,n=size(params)))
        enddo
        ! 1st one should be same as the original parameter
        trial_params(1,:) = params(:)

        do i=1,num_trial_params
            this_tanks = tanks .with. trial_params(i,:)
            this_waterheads = simulate(tanks=this_tanks ,GLs=GLs,dt=dt,rainfall=rainfall)
            buf = waterheads - this_waterheads
            residuals(i) = sqrt(sum(buf*buf))
        enddo
        
        params = trial_params(minvalID(residuals),:)
        if (present(debug) )then
            if (debug) then
                print *, itr, real(minval(residuals)),real(maxval(residuals)), real(params(:)),real(sigma)
            endif
        endif
        if (.not. allocated(last_residuals) )then
            last_residuals = residuals
        else
            if( minval(last_residuals) == minval(residuals))then
                same_count = same_count + 1
                
            else 
                same_count = 0
            endif
            last_residuals = residuals
        endif

        if (same_count >= 4)then
            sigma=sigma*0.20d0
            num_trial_params = num_trial_params/2
            same_count = 0
        endif

        if(sigma < dble(1.0e-5))then
            ! early exit
            exit
        endif
    enddo
    final_params = params

    

end function
! ###############################################################

function tanks_with_parameters(tanks,params) result(ret_tanks)
    type(Tank_),intent(in) :: tanks(:)
    real(real64),intent(in) :: params(:)
    type(Tank_),allocatable :: ret_tanks(:)
    integer(int32) :: i, j, k

    ret_tanks = tanks
    k = 0
    do i=1,size(tanks)
        do j=1,size(tanks(i)%coeff)
            k = k + 1
            ret_tanks(i)%coeff(j) = params(k)
        enddo
        if (associated(ret_tanks(i)%out))then
            k = k + 1
            ret_tanks(i)%out_coeff = params(k)
        endif
    enddo

end function

! ###############################################################

! ###############################################################

function get_params_tank_TankModelClassclass(tanks) result(params)
    type(Tank_),intent(in) :: tanks(:)
    real(real64),allocatable :: params(:)
    integer(int32) :: i, j, k

    
    k = 0
    do i=1,size(tanks)
        do j=1,size(tanks(i)%coeff)
            k = k + 1
        enddo
        if (associated(tanks(i)%out))then
            k = k + 1
        endif
    enddo
    allocate(params(k))
    k = 0
    do i=1,size(tanks)
        do j=1,size(tanks(i)%coeff)
            k = k + 1
            params(k) = tanks(i)%coeff(j) 
        enddo
        if (associated(tanks(i)%out))then
            k = k + 1
            params(k) = tanks(i)%out_coeff 
        endif
    enddo

end function

! ###############################################################

function to_waterhead_Tank_TankModelClass(this,GL) result(waterhead_mm)
    class(Tank_),intent(in) :: this
    real(real64),intent(in) :: GL
    real(real64) :: waterhead_mm

    ! タンク底が -100 mm, タンク底からの高さが10mm, ターゲットのGLが-20mmのとき
    ! 水位 = (-100 mm + 10 mm) = -90 mm 
    ! ターゲットのGLの水頭 = 水位GL - ターゲットGL = -90 mm - (- 20 mm) = -70 mm
    waterhead_mm = (this%S + this%GL_bottom) - GL

end function
! ###############################################################

! ###############################################################
subroutine showUnit_TankModelClass(this) 
    class(TankModel_),intent(in) :: this

    print *, "Length     :: mm"
    print *, "Water head :: mm H2O"
    print *, "Time       :: s "

end subroutine
! ###############################################################

! ###############################################################
subroutine show_TankModelClass(this) 
    class(TankModel_),intent(in) :: this

    ! visualize tank configulations
    ! but how?

end subroutine
! ###############################################################


end module Tank_TankModelClassClass