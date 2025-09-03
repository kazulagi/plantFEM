module DynamicEPClass
    use SparseClass
    
    use SpectreAnalysisClass
    use ElastoPlasticityClass
    use WaveKernelClass
    use TimeClass
    implicit none

    integer(int32),public :: NONE    = 0
    integer(int32),public :: JAUMANN_STRESS_RATIO    = 1
    integer(int32),public :: TRUESDELL_STRESS_RATIO  = 2
    integer(int32),public :: OLDROYD_STRESS_RATIO    = 3
    integer(int32),public :: CONVECTIVE_STRESS_RATIO = 4
    
    integer(int32),public :: TI_EXPONENTIAL_INTEGRATOR = 0
    integer(int32),public :: TI_NEWMARK_BETA           = 1
    integer(int32),public :: TI_FORWARDEULER           = 2
    integer(int32),public :: TI_HEUN             = 3
    integer(int32),public :: TI_RungeKutta4            = 4

    type :: DynamicEP_
        type(EP_Model_),allocatable  :: EP_Models(:)
        type(FEMDomain_),pointer :: femdomain => null()
        

        real(real64),allocatable :: Density(:)
        real(real64),allocatable :: ElasticParams(:,:),YieldParams(:,:),PlasticParams(:,:)
        integer(int32),allocatable :: ElemEP_ModelIdx(:)
        real(real64),allocatable :: u(:,:)
        real(real64),allocatable :: u_n(:,:)
        real(real64),allocatable :: v(:,:)
        real(real64),allocatable :: v_n(:,:)

        ! traction boundary
        real(real64),allocatable :: TractionBoundaryIdx(:)
        real(real64),allocatable :: TractionBoundaryVal(:)


        logical :: force_elastic = .false.

        ! control parameters
        integer(int32) :: stress_ratio = 2 ! TRUESDELL_STRESS_RATIO
        !real(real64) :: time_integration_factor = 0.50d0 ! time-integration factor (theta \in [0,1])

        ! >> for computation
        type(BCRS_) :: Upsilon
        real(real64),allocatable :: Psi(:) ! unknowns
        real(real64),allocatable :: Psi_n(:) ! unknowns
        real(real64),allocatable :: Psi_tr(:) ! unknowns
        real(real64),allocatable :: ElasticStrain(:,:),PlasticStrain(:,:)
        real(real64),allocatable :: yield_function_values_n(:,:)

        real(real64) :: cutoff_frequency=0.0d0

        integer(int32) :: max_itr = 100
        integer(int32) :: max_Newton_itr = 1000
        real(real64) :: bicgstab_er = dble(1.0e-14)
        integer(int32) :: max_bicgstab_itr = 10000


        integer(int32) :: TimeIntegrationScheme = 0 !

    contains
        procedure,public :: init => init_DynamicEP
        procedure,public :: update => update_DynamicEP
        procedure,public :: remove => remove_DynamicEP

        ! >> for matrix assembly
        procedure,public :: globalStressDivMatrix => globalStressDivMatrix_DynamicEP
        procedure,public :: LumpedMassDiag  => LumpedMassDiag_DynamicEP
        procedure,public :: globalStiffnessMatrix => globalStiffnessMatrix_DynamicEP
        procedure,public :: globalStressRatiolMatrix => globalStressRatiolMatrix_SymEP
        
        procedure,public :: localExternalForceVector => localExternalForceVector_SymEP
        procedure,public :: globalExternalForceVector => globalExternalForceVector_SymEP
        procedure,public :: StressRatioMatrix => StressRatioMatrix_SymEP

        ! >> >> for Newton-raphson return-mapping
        
        procedure,public :: localInternalForceVector => localInternalForceVector_SymEP
        procedure,public :: globalInternalForceVector => globalInternalForceVector_SymEP

        procedure,public :: returnMapping => returnMapping_SymEP
        


        ! getter
        procedure,public :: getCauchyStress => getCauchyStress_SymEP
        procedure,public :: get_yield_function_values => get_yield_function_values_DynamicEP
        procedure,public :: get_yield_function_value => get_yield_function_value_DynamicEP

        ! post-processing
        procedure,public :: getStrainField => getStrainField_SymEP
        procedure,public :: getStressField => getStressField_SymEP

        ! get traction force 
        


    end type

    public :: assignment(=)
    interface assignment(=)
        module procedure assign_vector_from_array
    end interface
    

contains

! ###############################################################
subroutine init_DynamicEP(this,femdomain,EP_Models,ElemEP_ModelIdx,&
        ElasticParams,YieldParams,PlasticParams,Density)
    class(DynamicEP_),intent(inout) :: this
    type(FEMDomain_),target,intent(in) :: femdomain
    type(EP_Model_),intent(in)  :: EP_Models(:)
    integer(int32),intent(in) :: ElemEP_ModelIdx(:)
    real(real64),intent(in) :: ElasticParams(:,:),YieldParams(:,:),PlasticParams(:,:),Density(:)
    integer(int32) :: n,m,nsig

    
    this%femdomain => femdomain
    this%EP_Models = EP_Models
    this%ElemEP_ModelIdx = ElemEP_ModelIdx
    this%ElasticParams = ElasticParams 
    this%YieldParams   = YieldParams   
    this%PlasticParams = PlasticParams 

    ! m = number of Dimension
    ! n = {number of node} \times { m(m-1)/2 + 2*m }
    ! e.g. m=3, n=4 => (3+2*3)*4 = 9*4
    m = this%femdomain%nd()
    nsig = this%femdomain%nd()*(this%femdomain%nd()+1)/2 * this%femdomain%ngp()
    this%Psi = zeros( nsig*this%femdomain%ne() + this%femdomain%nd()*this%femdomain%nn() )
    
    this%density = Density(:)
    this%u    = zeros(this%femdomain%nn(),this%femdomain%nd())
    this%v    = zeros(this%femdomain%nn(),this%femdomain%nd())

end subroutine
! ###############################################################

! ###############################################################
subroutine update_DynamicEP(this,dt,fixNodeList_x,fixNodeList_y,fixNodeList_z,&
        fixValueList_x,fixValueList_y,fixValueList_z)
    class(DynamicEP_),intent(inout) :: this
    real(real64),intent(in) :: dt
    integer(int32),optional,intent(in) :: fixNodeList_x(:)
    integer(int32),optional,intent(in) :: fixNodeList_y(:)
    integer(int32),optional,intent(in) :: fixNodeList_z(:)

    real(real64),optional,intent(in) :: fixValueList_x(:)
    real(real64),optional,intent(in) :: fixValueList_y(:)
    real(real64),optional,intent(in) :: fixValueList_z(:)

    real(real64),allocatable :: X(:,:),fix_value(:),&
        yield_function_values(:,:),u(:),du(:),ddu(:),RHS(:),&
        k_1(:),k_2(:),k_3(:),k_4(:),Psi_buf(:)
    type(CRS_) :: M_inv_P, K, W
    type(BCRS_) :: Upsilon_n,Upsilon_mid
    type(Time_) :: time
    integer(int32),allocatable :: fix_idx(:)

    integer(int32) :: itr_Newton
    real(real64) :: theta
    

    

    call time%start()

    if(this%cutoff_frequency==0.0d0)then
        ! default value is 1.0d0/dt
        this%cutoff_frequency = 1.0d0/dt
    endif
    
    !<<弾性除荷に戻らない問題についてのコメント用>>
    !<3001 step目開始>
    
    fix_idx   = int(zeros(0))
    fix_value = zeros(0)
    if(present(fixNodeList_x))then
        fix_idx   = fix_idx   // fixNodeList_x(:)*this%femdomain%nd()-2
        fix_value = fix_value // fixValueList_x(:)
    endif
    if(present(fixNodeList_y))then
        fix_idx   = fix_idx // fixNodeList_y(:)*this%femdomain%nd()-1
        fix_value = fix_value // fixValueList_y(:)
    endif
    if(present(fixNodeList_z))then
        fix_idx   = fix_idx // fixNodeList_z(:)*this%femdomain%nd()-0
        fix_value = fix_value // fixValueList_z(:)
    endif
    !<<弾性除荷に戻らない問題についてのコメント用>>
    !<<境界面で速度が負値となる．>>
    
    
    ![TODO]
    ! (2) this%StiffnessMatrix(), this%globalStressRatiolMatrix(), 
    !     this%globalStressDivMatrix(), this%LumpedMassDiag()

    ! perform time integration by the symplectic method.
    ! first, create matrix
    !if(.not. allocated(this%Upsilon%CRS))then
    
    ! DIRICHLET BOUNDARY CONDITION
    ! DIRICHLET BOUNDARY CONDITION
    if(present(fixNodeList_x) .and. present(fixValueList_x) )then
        this%Psi(fixNodeList_x(:)*3-2) = fixValueList_x(:)
        this%v(fixNodeList_x(:),1)   = fixValueList_x(:)
    endif
    if(present(fixNodeList_y) .and. present(fixValueList_y) )then
        this%Psi(fixNodeList_y(:)*3-1) = fixValueList_y(:)
        this%v(fixNodeList_y(:),2)   = fixValueList_y(:)
    endif
    if(present(fixNodeList_z) .and. present(fixValueList_z) )then
        this%Psi(fixNodeList_z(:)*3-0) = fixValueList_z(:)
        this%v(fixNodeList_z(:),3)   = fixValueList_z(:)
    endif
    ! DIRICHLET BOUNDARY CONDITION
    ! DIRICHLET BOUNDARY CONDITION
    
    !<<弾性除荷に戻らない問題についてのコメント用>>
    !<<ここまでで，前ステップの速度およびΨベクトルに速度固定条件が入る．>>
    
    ! 前ステップのu,vを保存
    this%v_n = this%v
    this%u_n = this%u
    Upsilon_n = this%Upsilon
    this%Psi_n = this%Psi

! >>> auto-tuning algorithm    ! 現在の応力状態から降伏関数の値を計算(廃止予定)
! >>> auto-tuning algorithm    this%yield_function_values_n = this%get_yield_function_values()
    
    call print("debug >> 1")
    call time%show()
    call time%start()
! !    ! この時点で，節点座標は初期配置Xのまま
! !    X = this%femdomain%mesh%nodcoord(:,:)
! !    ! だが，dN/dxを求めなくてはならない．
! !    ! そのため，粗い近似ではあるが，x = X + u でxを更新
! !    this%femdomain%mesh%nodcoord(:,:) = this%femdomain%mesh%nodcoord(:,:) + this%u_n(:,:)
! !    ! また，メッシュを更新したので，変位は0に戻る
! !    this%u(:,:) = 0.0d0

    
    
    ! 現配置(x)で係数マトリクスを計算
    M_inv_P = this%globalStressDivMatrix()
    call print("debug >> 1-1")
    call time%show()
    call time%start()

    call M_inv_P%divide_by_vector(this%LumpedMassDiag())

    
    !<<弾性除荷に戻らない問題についてのコメント用>>
    !<<この時点では，前ステップの応力を参照して剛性を決定している．
    !<<前ステップでは弾塑性状態にあるので，計算される剛性Kは小さいまま．>>
    !<<では，まずここで弾性仮定を強制する>>
    this%force_elastic = .true.
    call print("debug >> 1-2")
    call time%show()
    call time%start()
    K = this%globalStiffnessMatrix()
    call print("debug >> 1-3")
    call time%show()
    call time%start()
    W = this%globalStressRatiolMatrix() 

    call print("debug >> 2")
    call time%show()
    call time%start()
    call this%Upsilon%set([1,2],(-1.0d0)*M_inv_P)
    call this%Upsilon%set([2,1],K)
    call this%Upsilon%set([2,2],(-1.0d0)*W)
    call print("debug >> 3")    
    call time%show()
    call time%start()

    ![new!] 2025/05/16
    !call this%Upsilon%fill_zero_row(row=fix_idx)
    call print("debug >> 4")    
    call time%show()
    call time%start()
!    ! 一旦，x を X に戻しておく
!    this%femdomain%mesh%nodcoord(:,:) = X(:,:)
    if(.not.allocated(Upsilon_n%CRS))then

        Upsilon_n  = this%Upsilon
    endif

    
    ! EXP-INTEGによる時間積分を実行
    ! Psi^{\rm tr} 0 -> dt
    if (this%TimeIntegrationScheme==TI_EXPONENTIAL_INTEGRATOR)then
        ![ok] call print(fix_idx)
        ![ok] stop
        
        if(this%cutoff_frequency==0.0d0)then
            ! disable cutoff frequency
            this%Psi = this%Upsilon%exp(dt*this%Psi_n,fix_idx=fix_idx,&
                max_itr=this%max_itr)
        else
            this%Psi = this%Upsilon%exp(this%Psi_n,dt=dt,cutoff_frequency=this%cutoff_frequency,fix_idx=fix_idx,&
                max_itr=this%max_itr)
        endif
    elseif(this%TimeIntegrationScheme==TI_FORWARDEULER)then
        !! Forward Euler method

        this%Psi = this%Psi_n + dt*Upsilon_n%matmul(this%Psi_n)
        !if(this%cutoff_frequency==0.0d0)then
        !    ! disable cutoff frequency
        !    this%Psi = this%Upsilon%exp(dt*this%Psi_n,fix_idx=fix_idx,&
        !        max_itr=1)
        !else
        !    this%Psi = this%Upsilon%exp(this%Psi_n,dt=dt,cutoff_frequency=this%cutoff_frequency,fix_idx=fix_idx,&
        !        max_itr=1)
        !endif

    elseif(this%TimeIntegrationScheme==TI_RungeKutta4)then
        !! 4-th order Runge-Kutta method
        k_1 = Upsilon_n%matmul(this%Psi_n)
        k_1(fix_idx(:)) = 0.0d0

        ! 現配置(x)で係数マトリクスを計算
        M_inv_P = this%globalStressDivMatrix(Psi=this%Psi_n + dt/2.0d0*k_1)
        call M_inv_P%divide_by_vector(this%LumpedMassDiag())
        this%force_elastic = .true.
        K = this%globalStiffnessMatrix(   Psi=this%Psi_n + dt/2.0d0*k_1)        
        W = this%globalStressRatiolMatrix(Psi=this%Psi_n + dt/2.0d0*k_1) 
        call Upsilon_mid%set([1,2],(-1.0d0)*M_inv_P)
        call Upsilon_mid%set([2,1],K)
        call Upsilon_mid%set([2,2],(-1.0d0)*W)
        k_2 = Upsilon_mid%matmul(this%Psi_n + dt/2.0d0*k_1 )
        k_2(fix_idx(:)) = 0.0d0


        M_inv_P = this%globalStressDivMatrix(Psi=this%Psi_n + dt/2.0d0*k_2)
        call M_inv_P%divide_by_vector(this%LumpedMassDiag())
        this%force_elastic = .true.
        K = this%globalStiffnessMatrix(   Psi=this%Psi_n + dt/2.0d0*k_2)        
        W = this%globalStressRatiolMatrix(Psi=this%Psi_n + dt/2.0d0*k_2) 
        call Upsilon_mid%set([1,2],(-1.0d0)*M_inv_P)
        call Upsilon_mid%set([2,1],K)
        call Upsilon_mid%set([2,2],(-1.0d0)*W)
        k_3 = Upsilon_mid%matmul(this%Psi_n + dt/2.0d0*k_2 )
        k_3(fix_idx(:)) = 0.0d0


        M_inv_P = this%globalStressDivMatrix(Psi=this%Psi_n + dt/2.0d0*k_3)
        call M_inv_P%divide_by_vector(this%LumpedMassDiag())
        this%force_elastic = .true.
        K = this%globalStiffnessMatrix(   Psi=this%Psi_n + dt/2.0d0*k_3)        
        W = this%globalStressRatiolMatrix(Psi=this%Psi_n + dt/2.0d0*k_3) 
        call Upsilon_mid%set([1,2],(-1.0d0)*M_inv_P)
        call Upsilon_mid%set([2,1],K)
        call Upsilon_mid%set([2,2],(-1.0d0)*W)
        k_4 = this%Upsilon%matmul(this%Psi_n + dt*k_3 )
        k_4(fix_idx(:)) = 0.0d0

        this%Psi = this%Psi_n &
            + dt/6.0d0*(k_1 + 2.0d0*k_2 + 2.0d0*k_3 + k_4)
        this%Psi(fix_idx) = fix_value(:)

    elseif(this%TimeIntegrationScheme==TI_HEUN)then
        !! Heun method
        !k_1 = this%Upsilon%matmul(this%Psi_n)
        !k_2 = this%Upsilon%matmul(this%Psi_n + dt*k_1)
        !this%Psi = this%Psi_n &
        !    + dt/2.0d0*(k_1 + k_2)

        k_1 = this%Upsilon%matmul(this%Psi_n)
        k_1(fix_idx(:)) = 0.0d0
        !k_2 = this%Upsilon%matmul(this%Psi_n + dt*k_1)

        M_inv_P = this%globalStressDivMatrix(Psi=this%Psi_n + dt/2.0d0*k_1)
        call M_inv_P%divide_by_vector(this%LumpedMassDiag())
        this%force_elastic = .true.
        K = this%globalStiffnessMatrix(   Psi=this%Psi_n + dt/2.0d0*k_1)        
        W = this%globalStressRatiolMatrix(Psi=this%Psi_n + dt/2.0d0*k_1) 
        call Upsilon_mid%set([1,2],(-1.0d0)*M_inv_P)
        call Upsilon_mid%set([2,1],K)
        call Upsilon_mid%set([2,2],(-1.0d0)*W)
        k_2 = this%Upsilon%matmul(this%Psi_n + dt*k_1)
        k_2(fix_idx(:)) = 0.0d0

        this%Psi = this%Psi_n &
            + dt/2.0d0*(k_1 + k_2)
        this%Psi(fix_idx) = fix_value(:)
    else
        call print("[ERROR] >> update_DynamicEP >> no such TimeIntegrationSceme :: "&
            +str(this%TimeIntegrationScheme))
    endif

    !<<弾性除荷に戻らない問題についてのコメント用>>
    !<<ここで応力更新される．ただし，剛性は小さいままで応力が更新されるので．>>
    !<<降伏したままで，応力は減らない．>>
    !<<剛性を更新する際に，一旦弾性仮定が必要！>>

    if(present(fixNodeList_x) .and. present(fixValueList_x) )then
        this%Psi(fixNodeList_x(:)*3-2) = fixValueList_x(:)
    endif
    if(present(fixNodeList_y) .and. present(fixValueList_y) )then
        this%Psi(fixNodeList_y(:)*3-1) = fixValueList_y(:)
    endif
    if(present(fixNodeList_z) .and. present(fixValueList_z) )then
        this%Psi(fixNodeList_z(:)*3-0) = fixValueList_z(:)
    endif


    call print("debug >> 5")
    call time%show()
    call time%start()
    ! 速度場を更新
    this%v = transpose( &
        reshape(this%Psi(1:this%femdomain%nd()*this%femdomain%nn()),&
        [this%femdomain%nd(),this%femdomain%nn()]))
    
    ! 変位場を更新
    this%u = this%u_n + dt/2.0d0 * (this%v + this%v_n)

    call print("debug >> 6")
    call time%show()
    call time%start()
! !    ! ここまでで，trialのu, vを計算した．
! !    ! 得られたu, v, σが正しいとして，係数行列Upsilonを計算し，前ステップのUpsilonとの平均を取る．
! !    ! そうして作られた新たなUpsilonにより，真のu, v, σを得る．
    

! !    ! この時点で，節点座標は初期配置Xのまま
! !    ! だが，dN/dxを求めなくてはならない．
! !    ! そのため，粗い近似ではあるが，x = X + u でxを更新
! !    this%femdomain%mesh%nodcoord(:,:) = this%femdomain%mesh%nodcoord(:,:) + this%u(:,:)
! !    ! また，メッシュを更新したので，変位は0に戻る
! !    this%u(:,:) = 0.0d0

    ! update matrices for trial displacement
    ! 現配置(x)で係数マトリクスを計算

    !<<弾性除荷に戻らない問題についてのコメント用>>
    !<<ここまでのtrialの応力と速度は弾性仮定のもとでの値>>
    !<<実際には弾塑性になっているかもしれないので，ここで弾塑性判定と剛性更新を行う．>>
    this%force_elastic = .false.
    M_inv_P = this%globalStressDivMatrix()
    call print("debug >> 6-1")
    call time%show()
    call time%start()
    call M_inv_P%divide_by_vector(diag_vector=this%LumpedMassDiag())    
    call print("debug >> 6-2")
    call time%show()
    call time%start()
    ! 弾性係数の変化を考慮して，f=0をキープするよう重み付け
    K = this%globalStiffnessMatrix(auto_tuning=.True.) ! weighted global matrix
    call print("debug >> 6-3")
    call time%show()
    call time%start()
    W = this%globalStressRatiolMatrix() ! global matrix
    

    call print("debug >> 6")
    call time%show()
    call time%start()
    !call this%Upsilon%add([1,2],(-1.0d0)*dt/2.0d0*M_inv_P)
    !call this%Upsilon%add([2,1],dt/2.0d0*K)
    !call this%Upsilon%add([2,2],(-1.0d0)*dt/2.0d0*W)
    if(.not.allocated(Upsilon_n%CRS))then
        Upsilon_n = this%Upsilon
    endif

    call this%Upsilon%set([1,2],(-1.0d0)/2.0d0*M_inv_P + 1.0d0/2.0d0*Upsilon_n%CRS(1,2))
    call this%Upsilon%set([2,1],K) ! auto-tuning done!
    call this%Upsilon%set([2,2],(-1.0d0)/2.0d0*W + 1.0d0/2.0d0*Upsilon_n%CRS(2,2))

    call print("debug >> 7")
    call time%show()
    call time%start()
    ![new!] 2025/05/16
    
    
! >>> auto-tuning algorithm    if(allocated(Upsilon_n%CRS))then
! >>> auto-tuning algorithm        ! 試しに，1要素のときのみthetaのオートチューニングを入れてみる．
! >>> auto-tuning algorithm        yield_function_values = this%get_yield_function_values()
! >>> auto-tuning algorithm
! >>> auto-tuning algorithm        if ( (yield_function_values(1,1) <= 0  ) .and. (this%yield_function_values_n(1,1) <= 0  ))then
! >>> auto-tuning algorithm            ! e -> e
! >>> auto-tuning algorithm            theta = 0.50d0
! >>> auto-tuning algorithm        elseif( (yield_function_values(1,1) <= 0  ) .and. (this%yield_function_values_n(1,1) >= 0  ))then
! >>> auto-tuning algorithm            ! p -> e
! >>> auto-tuning algorithm            theta        = this%yield_function_values_n(1,1)/(this%yield_function_values_n(1,1) - yield_function_values(1,1))
! >>> auto-tuning algorithm        elseif( (yield_function_values(1,1) >= 0  ) .and. (this%yield_function_values_n(1,1) <= 0  ))then
! >>> auto-tuning algorithm            ! e -> p 
! >>> auto-tuning algorithm            theta        = this%yield_function_values_n(1,1)/(this%yield_function_values_n(1,1) - yield_function_values(1,1))
! >>> auto-tuning algorithm            !call print([theta,yield_function_values(1,1),this%yield_function_values_n(1,1)])
! >>> auto-tuning algorithm            !stop
! >>> auto-tuning algorithm        elseif( (yield_function_values(1,1) >= 0  ) .and. (this%yield_function_values_n(1,1) <= 0  ))then
! >>> auto-tuning algorithm            ! p -> p
! >>> auto-tuning algorithm            theta        = this%yield_function_values_n(1,1)/(this%yield_function_values_n(1,1) - yield_function_values(1,1))
! >>> auto-tuning algorithm        endif
! >>> auto-tuning algorithm        this%Upsilon = (1.0d0-theta)*this%Upsilon
! >>> auto-tuning algorithm        this%Upsilon = this%Upsilon + (theta)*Upsilon_n
! >>> auto-tuning algorithm    endif
    
    ! ↓遅すぎるのでリストラ
    !call this%Upsilon%fill_zero_row(row=fix_idx)

    call print("debug >> 8")
    call time%show()
    call time%start()
    
    ! get updated solution
    if(this%TimeIntegrationScheme==TI_EXPONENTIAL_INTEGRATOR)then
        !this%Psi = this%Upsilon%exp(this%Psi_n,dt=dt,cutoff_frequency=this%cutoff_frequency,fix_idx=fix_idx,&
        !    max_itr=this%max_itr)
        
        if(this%cutoff_frequency==0.0d0)then
            ! disable cutoff frequency
            this%Psi = this%Upsilon%exp(dt*this%Psi_n,fix_idx=fix_idx,&
                max_itr=this%max_itr)
        else
            this%Psi = this%Upsilon%exp(this%Psi_n,dt=dt,cutoff_frequency=this%cutoff_frequency,fix_idx=fix_idx,&
                max_itr=this%max_itr)
        endif
    elseif(this%TimeIntegrationScheme==TI_FORWARDEULER)then
        !! Forward Euler method
        !this%Psi = this%Upsilon%exp(this%Psi_n,dt=dt,cutoff_frequency=this%cutoff_frequency,fix_idx=fix_idx,&
        !    max_itr=1)
        if(this%cutoff_frequency==0.0d0)then
            ! disable cutoff frequency
            this%Psi = this%Upsilon%exp(dt*this%Psi_n,fix_idx=fix_idx,&
                max_itr=1)
        else
            this%Psi = this%Upsilon%exp(this%Psi_n,dt=dt,cutoff_frequency=this%cutoff_frequency,fix_idx=fix_idx,&
                max_itr=1)
        endif
    elseif(this%TimeIntegrationScheme==TI_RungeKutta4)then
        !! 4-th order Runge-Kutta method
        k_1 = Upsilon_n%matmul(this%Psi_n)
        k_1(fix_idx) = 0.0d0
        
        ! 現配置(x)で係数マトリクスを計算
        M_inv_P = this%globalStressDivMatrix(Psi=this%Psi_n + dt/2.0d0*k_1)
        call M_inv_P%divide_by_vector(this%LumpedMassDiag())
        
        K = this%globalStiffnessMatrix(   Psi=this%Psi_n + dt/2.0d0*k_1)        
        W = this%globalStressRatiolMatrix(Psi=this%Psi_n + dt/2.0d0*k_1) 
        call Upsilon_mid%set([1,2],(-1.0d0)*M_inv_P)
        call Upsilon_mid%set([2,1],K)
        call Upsilon_mid%set([2,2],(-1.0d0)*W)
        k_2 = Upsilon_mid%matmul(this%Psi_n + dt/2.0d0*k_1 )
        k_2(fix_idx) = 0.0d0

        M_inv_P = this%globalStressDivMatrix(Psi=this%Psi_n + dt/2.0d0*k_2)
        call M_inv_P%divide_by_vector(this%LumpedMassDiag())
        
        K = this%globalStiffnessMatrix(   Psi=this%Psi_n + dt/2.0d0*k_2)        
        W = this%globalStressRatiolMatrix(Psi=this%Psi_n + dt/2.0d0*k_2) 
        call Upsilon_mid%set([1,2],(-1.0d0)*M_inv_P)
        call Upsilon_mid%set([2,1],K)
        call Upsilon_mid%set([2,2],(-1.0d0)*W)
        k_3 = Upsilon_mid%matmul(this%Psi_n + dt/2.0d0*k_2 )
        k_3(fix_idx) = 0.0d0

        M_inv_P = this%globalStressDivMatrix(Psi=this%Psi_n + dt/2.0d0*k_3)
        call M_inv_P%divide_by_vector(this%LumpedMassDiag())
        this%force_elastic = .true.
        K = this%globalStiffnessMatrix(   Psi=this%Psi_n + dt/2.0d0*k_3)        
        W = this%globalStressRatiolMatrix(Psi=this%Psi_n + dt/2.0d0*k_3) 
        call Upsilon_mid%set([1,2],(-1.0d0)*M_inv_P)
        call Upsilon_mid%set([2,1],K)
        call Upsilon_mid%set([2,2],(-1.0d0)*W)
        k_4 = this%Upsilon%matmul(this%Psi_n + dt*k_3 )
        k_4(fix_idx) = 0.0d0

        this%Psi = this%Psi_n &
            + dt/6.0d0*(k_1 + 2.0d0*k_2 + 2.0d0*k_3 + k_4)
        this%Psi(fix_idx) = fix_value(:)

    elseif(this%TimeIntegrationScheme==TI_HEUN)then
        !! Heun method
        k_1 = this%Upsilon%matmul(this%Psi_n)
        k_1(fix_idx) = 0.0d0
        !k_2 = this%Upsilon%matmul(this%Psi_n + dt*k_1)

        M_inv_P = this%globalStressDivMatrix(Psi=this%Psi_n + dt/2.0d0*k_1)
        call M_inv_P%divide_by_vector(this%LumpedMassDiag())
        this%force_elastic = .true.
        K = this%globalStiffnessMatrix(   Psi=this%Psi_n + dt/2.0d0*k_1)        
        W = this%globalStressRatiolMatrix(Psi=this%Psi_n + dt/2.0d0*k_1) 
        call Upsilon_mid%set([1,2],(-1.0d0)*M_inv_P)
        call Upsilon_mid%set([2,1],K)
        call Upsilon_mid%set([2,2],(-1.0d0)*W)
        k_2 = this%Upsilon%matmul(this%Psi_n + dt*k_1)
        k_2(fix_idx) = 0.0d0

        this%Psi = this%Psi_n &
            + dt/2.0d0*(k_1 + k_2)
        this%Psi(fix_idx) = fix_value(:)
    else
        call print("[ERROR] >> update_DynamicEP >> no such TimeIntegrationSceme :: "&
            +str(this%TimeIntegrationScheme))
    endif
    
!    if(present(fixNodeList_x) .and. present(fixValueList_x) )then
!        this%Psi(fixNodeList_x(:)*3-2) = fixValueList_x(:)
!    endif
!    if(present(fixNodeList_y) .and. present(fixValueList_y) )then
!        this%Psi(fixNodeList_y(:)*3-1) = fixValueList_y(:)
!    endif
!    if(present(fixNodeList_z) .and. present(fixValueList_z) )then
!        this%Psi(fixNodeList_z(:)*3-0) = fixValueList_z(:)
!    endif
    call print("debug >> 9")
    call time%show()
    call time%start()
    this%v = transpose(&
        reshape(this%Psi(1:this%femdomain%nd()*this%femdomain%nn()),&
        [this%femdomain%nd(),this%femdomain%nn()]))
    
    ! Crank-Nicolson
    this%u = this%u_n + dt/2.0d0 * (this%v + this%v_n)

! !    ! 最後に，x を X に戻しておく
! !    this%femdomain%mesh%nodcoord(:,:) = X(:,:)
!    elseif(this%TimeIntegrationScheme == TI_NEWMARK_BETA)then
!        ! Time-integration scheme is Newmark's beta method.
!        ! Stress-update scheme is Return-mapping.
!        ! Solution algorithm is Newton's method.
!
!        
!        fix_idx   = int(zeros(0))
!        fix_value = zeros(0)
!        if(present(fixNodeList_x))then
!            fix_idx   = fix_idx   // fixNodeList_x(:)*this%femdomain%nd()-2
!            fix_value = fix_value // fixValueList_x(:)*this%femdomain%nd()-2
!        endif
!        if(present(fixNodeList_y))then
!            fix_idx   = fix_idx // fixNodeList_y(:)*this%femdomain%nd()-1
!            fix_value = fix_value // fixValueList_y(:)*this%femdomain%nd()-1
!        endif
!        if(present(fixNodeList_z))then
!            fix_idx   = fix_idx // fixNodeList_z(:)*this%femdomain%nd()-0
!            fix_value = fix_value // fixValueList_z(:)*this%femdomain%nd()-0
!        endif
!        
!        if(allocated(this%Upsilon%CRS)) then
!            deallocate(this%Upsilon%CRS)
!        endif
!        allocate(this%Upsilon%CRS(1,1))
!        this%force_elastic = .true.
!        this%Upsilon%CRS(1,1) = this%globalStiffnessMatrix()
!        
!        ! 当該ステップにおけるtraction + body force からinternal forceを引いて初期残差力を出す
!        !  [new!]   && [new!] 
!        RHS = this%globalExternalForceVector() - this%globalInternalForceVector()
!        !  [!] bicgstab for BCRS format
!        call this%Upsilon%bicgstab(x=du,b=RHS,itrmax=this%max_bicgstab_itr, &
!            er=this%bicgstab_er) !fix_idx=fix_idx,,fix_value=fix_value
!        
!        do itr_Newton = 1,this%max_Newton_itr
!            du = du - ddu
!
!            this%force_elastic = .false.
!            ! [new!] update stress
!            call this%returnMapping(du) !Store [du, stress tensor] in this%Psi
!            this%Upsilon%CRS(1,1) = this%globalStiffnessMatrix()
!            !> Solve K(ddu) = f
!            RHS = RHS - this%globalInternalForceVector()
!            call this%Upsilon%bicgstab(x=ddu,b=RHS,itrmax=this%max_bicgstab_itr,&
!                er=this%bicgstab_er) !fix_idx=fix_idx,fix_value=0.0d0*fix_value
!            
!        enddo
!
!    else
!        call print("[ERROR] >> update_DynamicEP >> no such TimeIntegrationSceme :: "+str(this%TimeIntegrationScheme))
!    endif
end subroutine
! ###############################################################


! ###############################################################
function globalStressDivMatrix_DynamicEP(this,Psi) result(ret)
    class(DynamicEP_),intent(inout) :: this
    real(real64),optional,intent(in) :: Psi(:)
    
    integer(int32) :: elem_idx,gp_idx,i,j,nd,nne,row_idx,col_idx,nsig,k,l,nn,&
        nne_idx,sig_idx,dim_idx,ngp,node_idx,row,col
    real(real64),allocatable :: dNdxi(:,:),P_e(:,:),Jin(:,:),dNdx(:,:),val(:), Psi_buf(:)
    integer(int32),allocatable ::  stress_i(:),stress_j(:)
    real(real64) :: integral_val,detJ
    type(Shapefunction_) :: shapefunc
    type(COO_) :: ret_COO
    type(CRS_) :: ret

    if(present(Psi))then
        Psi_buf  = this%Psi
        this%Psi = Psi
    endif

    nd   = this%femdomain%nd()
    nne  = this%femdomain%nne()
    nn  = this%femdomain%nn()
    ngp  = this%femdomain%ngp()

    if (nd /= 3)then
        stop "globalStressDivMatrix_DynamicEP >> only for 3-D problems."
    endif

    nsig = ((nd)*(nd-1))/2+nd
    call ret_COO%init(nd*nn)

    ! s(1,1),s(2,2),s(3,3)
    if(nd==1)then
        !stress_i = [1]
        stress_j = [1]
    endif
    if(nd==2)then
        !stress_i = [1,2,1]
        stress_j = [1,2,2]
    endif
    if(nd==3)then
        !stress_i = [1,2,3,1,2,3]
        stress_j = [1,2,3,2,3,1]
    endif

    ! create coo matrix
    if(allocated(this%Upsilon%CRS) )then
        ret = this%Upsilon%CRS(1,2)
        ret%val(:) = 0.0d0
        val = ret%val(:)
        deallocate(ret%val)
    else
        do elem_idx = 1, this%femdomain%ne()
            do gp_idx = 1, this%femdomain%ngp()
                do nne_idx=1,this%femdomain%nne()
                    node_idx = this%femdomain%mesh%elemnod(elem_idx,nne_idx)
                    do dim_idx=1,this%femdomain%nd()
                        do sig_idx = 1,nsig
                            call ret_COO%add(&
                                nd*(node_idx-1)+dim_idx,&
                                ngp*nsig*(elem_idx-1) + nsig*(gp_idx-1) + sig_idx,&
                                0.0d0 )
                        enddo
                    enddo
                enddo
            enddo
        enddo

        ret = ret_COO%to_CRS()
        call ret_COO%remove()
        val = ret%val
        val(:) = 0.0d0
        deallocate(ret%val)
    endif

    P_e = zeros(nd*nne,nsig)

    call shapefunc%setType(NumOfDim=this%femdomain%nd(), &
        NumOfNodePerElem=this%femdomain%nne(), NumOfGp=this%femdomain%ngp())
    !$OMP parallel do private(gp_idx,dNdxi,Jin,P_e,nne_idx,node_idx,dim_idx,sig_idx,dNdx,j,row,col) &
    !$OMP  shared(this,nsig,ret) firstprivate(shapefunc) reduction(+:val)
    do elem_idx = 1, this%femdomain%ne()
        !call shapefunc%setType(NumOfDim=this%femdomain%nd(), &
        !    NumOfNodePerElem=this%femdomain%nne(), NumOfGp=this%femdomain%ngp())
        do gp_idx = 1, this%femdomain%ngp()
            call getAllShapeFunc(shapefunc, elem_id=elem_idx, nod_coord=this%femdomain%Mesh%NodCoord, &
                elem_nod=this%femdomain%Mesh%ElemNod, OptionalGpID=gp_idx)

            
            if (det_mat(shapefunc%Jmat, this%femdomain%nd()) == 0.0d0) stop "globalStressDivMatrix_DynamicEP,detJ=0"
            call inverse_rank_2(shapefunc%Jmat, Jin)
            dNdx = matmul(transpose(shapefunc%dNdgzi), Jin) !dNdgzi* dgzidx

            do nne_idx=1,this%femdomain%nne()
                P_e(3*(nne_idx-1)+1,1:nsig) = [dNdx(nne_idx,1),0.0d0,0.0d0,dNdx(nne_idx,2),0.0d0,dNdx(nne_idx,3)]
                P_e(3*(nne_idx-1)+2,1:nsig) = [0.0d0,dNdx(nne_idx,2),0.0d0,dNdx(nne_idx,1),dNdx(nne_idx,3),0.0d0]
                P_e(3*(nne_idx-1)+3,1:nsig) = [0.0d0,0.0d0,dNdx(nne_idx,3),0.0d0,dNdx(nne_idx,2),dNdx(nne_idx,1)]
            enddo

            do nne_idx=1,this%femdomain%nne()
                node_idx = this%femdomain%mesh%elemnod(elem_idx,nne_idx)
                do dim_idx=1,this%femdomain%nd()
                    do sig_idx = 1,nsig

                        ! Rewritten for parallelization
                        row = this%femdomain%nd()*(node_idx-1)+dim_idx
                        col = this%femdomain%ngp()*nsig*(elem_idx-1) + nsig*(gp_idx-1) + sig_idx
                        do j = ret%row_ptr(row), ret%row_ptr(row + 1) - 1
                            if (ret%col_idx(j) == col) then
                                val(j) = val(j) + P_e(this%femdomain%nd()*(nne_idx-1)+dim_idx,sig_idx )
                                exit
                            end if
                        end do
                        !call ret_COO%add(&
                        !    nd*(node_idx-1)+dim_idx,&
                        !    ngp*nsig*(elem_idx-1) + nsig*(gp_idx-1) + sig_idx,&
                        !    P_e(nd*(nne_idx-1)+dim_idx,sig_idx ) )
                        
                    enddo
                enddo
            enddo

        enddo
    enddo
    !$OMP end parallel do
    ret%val = val


    if(present(Psi))then
        this%Psi = Psi_buf
    endif
!    ret = ret_COO%to_CRS()

    

end function
! ###############################################################

! ####################################################


! ####################################################
function LumpedMassDiag_DynamicEP(this) result(ret)
    class(DynamicEP_),intent(in) :: this
    real(real64),allocatable :: ret(:)
    type(CRS_) :: M

    M = this%femdomain%MassMatrix( &
            Density=this%Density, &
            DOF=this%femdomain%nd())

    ret = M%lumped()

end function
! ####################################################

function get_yield_function_values_DynamicEP(this) result(ret)
    class(DynamicEP_),intent(in) :: this
    real(real64),allocatable :: ret(:,:)
    complex(real64),allocatable :: CauchyStress(:,:),PlasticStrain(:,:)
    integer(int32) :: elem_idx, gp_idx
    
    ret = zeros(this%femdomain%ne(),this%femdomain%ngp())
    do elem_idx = 1, this%femdomain%ne()
        do gp_idx = 1, this%femdomain%ngp()
            CauchyStress = this%getCauchyStress(ElementID=elem_idx,GaussPointID=gp_idx)
            PlasticStrain = zeros(3,3)
            ret(elem_idx,gp_idx) = dble(&
                this%EP_Models(this%ElemEP_ModelIdx(elem_idx))&
                    %YieldFunction(CauchyStress, PlasticStrain, this%PlasticParams(elem_idx,:) ))
        enddo
    enddo

end function

! ####################################################

function get_yield_function_value_DynamicEP(this,ElementID,GaussPointID,last_step) result(ret)
    class(DynamicEP_),intent(in) :: this
    integer(int32),intent(in) :: ElementID,GaussPointID
    logical,optional,intent(in) :: last_step
    real(real64),allocatable :: ret
    complex(real64),allocatable :: CauchyStress(:,:),PlasticStrain(:,:)
    integer(int32) :: elem_idx, gp_idx
    
    
    CauchyStress = this%getCauchyStress(ElementID=ElementID,GaussPointID=GaussPointID,last_step=last_step)
    PlasticStrain = zeros(3,3)
    ret = dble(&
        this%EP_Models(this%ElemEP_ModelIdx(ElementID)) &
            %YieldFunction(CauchyStress, PlasticStrain, this%PlasticParams(ElementID,:) ))
    
end function

! ####################################################
! ここを高速化したい．
function globalStiffnessMatrix_DynamicEP(this,auto_tuning,Psi) result(ret)
    class(DynamicEP_),intent(inout) :: this
    logical,optional,intent(in)  :: auto_tuning
    real(real64),optional,intent(in) :: Psi(:)

    ! under implementation 
    integer(int32) :: elem_idx,gp_idx,i,j,nd,nne,row_idx,col_idx,nsig,k,l,ne,&
        node_idx_1,node_idx_2,nn,theta,n,m,row,col
    real(real64),allocatable :: dNdxi(:,:),C_mat(:,:),B_mat(:,:),ElasticStrain(:,:),&
        U(:),ElasticStrain_vec(:),U_e(:),C_mat_n(:,:),ElasticStrain_n(:,:),val(:),Psi_buf(:)
        
    integer(int32),allocatable ::  stress_i(:),stress_j(:)
    real(real64) :: integral_val,fval,fval_n
    real(real64),allocatable :: K_e(:,:),K_e_n(:,:)
    integer(int32) :: ngp
    
    type(Shapefunction_) :: shapefunc
    type(COO_) :: ret_COO
    type(CRS_) :: ret
    type(IO_)  :: debug
    logical :: execute_auto_tuning 



    if(present(Psi))then
        Psi_buf  = this%Psi
        this%Psi = Psi
    endif

    execute_auto_tuning = input(default=.false.,option=auto_tuning)
    ! Validated >> 2025/04/24
    ! 亜弾性構成則の全体要素剛性行列　
    ! C B v
    

    nd   = this%femdomain%nd()
    nn   = this%femdomain%nn()
    nne  = this%femdomain%nne()
    ne  = this%femdomain%ne()
    ngp  = this%femdomain%ngp()
    nsig = ((nd)*(nd-1))/2 + nd

    ! for 3-D case, with single element
    ! and NNE = 8, NN=8, nGP = 9
    ! matrix size = ( nsig * ngp * ne ,ND * NN )

    ! use zero matrix
    if(allocated(this%Upsilon%CRS))then
        ! if K is already allocated, use the CRS format.
        ret = this%Upsilon%CRS(2,1)
        ret%val(:) = 0.0d0
        val = ret%val(:)
        deallocate(ret%val)
    else
        call ret_COO%init(nsig * ngp * ne)
        do elem_idx = 1, this%femdomain%ne()
            do gp_idx = 1, this%femdomain%ngp()
                do i=1,nne
                    node_idx_1 = this%femdomain%mesh%elemnod(elem_idx,i)
                    do k=1,nd
                        do l=1,nsig
                            call ret_COO%set( &
                                nsig * ngp * (elem_idx-1) + nsig * (gp_idx-1) +  l, & ! gauss-point wise
                                nd * (node_idx_1 - 1) + k, & ! node-wise
                                0.0d0 &
                            )
                        enddo
                    enddo
                enddo
            enddo
        enddo
        ret = ret_COO%to_CRS()
        call ret_COO%remove()
        val = ret%val
        deallocate(ret%val)
        val(:)=0.0d0
    endif



    ! s(1,1),s(2,2),s(3,3)
    if(nd==1)then
        !stress_i = [1]
        stress_j = [1]
    endif
    if(nd==2)then
        !stress_i = [1,2,1]
        stress_j = [1,2,2]
    endif
    if(nd==3)then
        !stress_i = [1,2,3,1,2,3]
        stress_j = [1,2,3,2,3,1]
    endif

    ! ONLY FOR UNIFORM MESH which uses only a single type of element
    
    
    if(.not. execute_auto_tuning)then
!$OMP parallel do private(gp_idx,C_mat,ElasticStrain,K_e,node_idx_1,i,j,k,l,n,m,row,col,nsig) &
!$OMP   shared(this,ret)  firstprivate(shapefunc) reduction(+:val)
        do elem_idx = 1, this%femdomain%ne()

    call shapefunc%setType(NumOfDim=this%femdomain%nd(), &
        NumOfNodePerElem=this%femdomain%nne(), NumOfGp=this%femdomain%ngp())


            nsig = ((this%femdomain%nd())*(this%femdomain%nd()-1))/2 + this%femdomain%nd()
            do gp_idx = 1, this%femdomain%ngp()                
                call getAllShapeFunc(shapefunc,elem_id=elem_idx, nod_coord=this%femdomain%Mesh%NodCoord, &
                    elem_nod=this%femdomain%Mesh%ElemNod, OptionalGpID=gp_idx)
                ! element-wise matrices >> 
                
                ElasticStrain = this%femdomain%getStrainTensor(&
                    displacement=this%u,&
                    ElementID=elem_idx, &
                    GaussPointID=gp_idx)

                ! 論点:
                    ! small strainのBmatでよいのか．
                    ! 超弾性と亜弾性の関係を深堀する必要あり
                ! Compute stiffness matrix C

                C_mat = this%EP_Models( this%ElemEP_ModelIdx(elem_idx) )%StiffnessMatrix(&
                    CauchyStress = this%getCauchyStress(ElementID=elem_idx,GaussPointID=gp_idx),&
                    ElasticParams=this%ElasticParams(elem_idx,:),&
                    PlasticParams=this%PlasticParams(elem_idx,:),&
                    ElasticStrain=ElasticStrain,&
                    nDim=this%femdomain%nd(),&
                    force_elastic=this%force_elastic)

                ! K = C B
                ! K_e : (  NSIG, ND*NNE)
                K_e = matmul(C_mat,this%femdomain%Bmatrix(shapefunc))

                do i=1,this%femdomain%nne()
                    node_idx_1 = this%femdomain%mesh%elemnod(elem_idx,i)
                    do k=1,this%femdomain%nd()
                        do l=1,nsig
                            row = nsig * this%femdomain%ngp() * (elem_idx-1) + nsig * (gp_idx-1) +  l
                            col = this%femdomain%nd() * (node_idx_1 - 1) + k
                        
                            !ret%add
                            do j = ret%row_ptr(row), ret%row_ptr(row + 1) - 1
                                if (ret%col_idx(j) == col) then
                                    
                                    val(j) = val(j) + K_e( l , nd*(i-1)+k )   
                                    exit
                                end if
                            end do

!                            call ret_COO%add( &
!                                nsig * ngp * (elem_idx-1) + nsig * (gp_idx-1) +  l, & ! gauss-point wise
!                                nd * (node_idx_1 - 1) + k, & ! node-wise
!                                K_e( l , nd*(i-1)+k ) &
!                            )
                        enddo
                    enddo
                enddo
            enddo
        enddo        
        !$OMP end parallel do
        
!        ret = ret_COO%to_CRS()
        ret%val = val
    else

        call shapefunc%setType(NumOfDim=this%femdomain%nd(), &
            NumOfNodePerElem=this%femdomain%nne(), NumOfGp=this%femdomain%ngp())
 !$OMP parallel do private(gp_idx,C_mat_n,C_mat,B_mat,ElasticStrain,ElasticStrain_n,&
        !$OMP K_e,K_e_n,node_idx_1,i,j,k,l,fval,fval_n,theta,row,col,nsig) firstprivate(shapefunc) shared(this,ret) reduction(+:val)
        do elem_idx = 1, this%femdomain%ne()
            
            nsig = ((this%femdomain%nd())*(this%femdomain%nd()-1))/2 + this%femdomain%nd()    
                
            do gp_idx = 1, this%femdomain%ngp()
                ! 時間積分においてUpsilonの線形仮定を無視し，
                ! 塑性構成則をみたすように時間積分点をずらす．
                ! 説明は追記予定
                
                ! 原因はBマトリクス
                ! さらにshapefuncの何が原因かを絞り込む
                !> shapefunc%dNdgziが原因!
                
                
                
                !(1) 形状関数を計算(共通)ここに原因
                
                call getAllShapeFunc(shapefunc,elem_id=elem_idx, nod_coord=this%femdomain%Mesh%NodCoord, &
                    elem_nod=this%femdomain%Mesh%ElemNod, OptionalGpID=gp_idx)
                
                !(2) 弾性ひずみ@(n), (n+1)ステップを計算(共通)
                ![ok]
                ElasticStrain_n = this%femdomain%getStrainTensor(&
                    displacement=this%u_n,&
                    ElementID=elem_idx, &
                    GaussPointID=gp_idx)
                ![ok]
                ElasticStrain = this%femdomain%getStrainTensor(&
                    displacement=this%u,&
                    ElementID=elem_idx, &
                    GaussPointID=gp_idx)
                ! 論点:
                    ! small strainのBmatでよいのか．
                    ! 超弾性と亜弾性の関係を深堀する必要あり

                ! Compute stiffness matrix C for n-th step
                ![OK]
                C_mat_n = this%EP_Models( this%ElemEP_ModelIdx(elem_idx) )%StiffnessMatrix(&
                    CauchyStress = this%getCauchyStress(ElementID=elem_idx,GaussPointID=gp_idx,last_step=.true.),&
                    ElasticParams=this%ElasticParams(elem_idx,:),&
                    PlasticParams=this%PlasticParams(elem_idx,:),&
                    ElasticStrain=ElasticStrain_n,&
                    nDim=this%femdomain%nd(),&
                    force_elastic=.false.)
                ![OK]
                ! Compute stiffness matrix C for n-th step
                C_mat = this%EP_Models( this%ElemEP_ModelIdx(elem_idx) )%StiffnessMatrix(&
                    CauchyStress = this%getCauchyStress(ElementID=elem_idx,GaussPointID=gp_idx,last_step=.false.),&
                    ElasticParams=this%ElasticParams(elem_idx,:),&
                    PlasticParams=this%PlasticParams(elem_idx,:),&
                    ElasticStrain=ElasticStrain,&
                    nDim=this%femdomain%nd(),&
                    force_elastic=.false.)
                
                ![ok]
                fval = this%get_yield_function_value(ElementID=elem_idx,GaussPointID=gp_idx)
                fval_n = this%get_yield_function_value(ElementID=elem_idx,GaussPointID=gp_idx,last_step=.true.)
            
                theta = 0.50d0
                if(fval_n - fval==0.0d0)then
                    theta = 0.50d0
                elseif ( (fval < 0  ) .and. (fval_n <= 0  ))then
                    ! e -> e
                    theta = 0.50d0
                elseif( (fval == 0  ) .and. (fval_n <= 0  ))then
                    theta        = fval_n/(fval_n - fval)    
                elseif( (fval <= 0  ) .and. (fval_n >= 0  ))then
                    ! p -> e
                    theta        = fval_n/(fval_n - fval)
                elseif( (fval >= 0  ) .and. (fval_n <= 0  ))then
                    ! e -> p 
                    theta        = fval_n/(fval_n - fval)
                elseif( (fval >= 0  ) .and. (fval_n <= 0  ))then
                    ! p -> p
                    theta        = fval_n/(fval_n - fval)
                endif
                if(theta < 0.0d0)then
                    theta = 0.0d0
                elseif(theta > 1.0d0)then
                    theta = 1.0d0
                endif
                

                B_mat = this%femdomain%Bmatrix(shapefunc)

                ! stiffness matrix for current and last stress conditions
                K_e = matmul(C_mat,B_mat)
                K_e_n = matmul(C_mat_n,B_mat)

                K_e = (1.0d0 - theta) * K_e + (theta) * K_e_n
                

                do i=1,this%femdomain%nne()
                    ![OK]
                    node_idx_1 = this%femdomain%mesh%elemnod(elem_idx,i)
                    
                    do k=1,this%femdomain%nd()
                        do l=1,nsig
                            ![OK]
                            row = nsig * this%femdomain%ngp() * (elem_idx-1) + nsig * (gp_idx-1) +  l
                            col = this%femdomain%nd() * (node_idx_1 - 1) + k
                            do j = ret%row_ptr(row), ret%row_ptr(row + 1) - 1
                                if (ret%col_idx(j) == col) then
                                    val(j) = val(j) + K_e( l , this%femdomain%nd()*(i-1) + k )   
                                    exit
                                end if
                            end do

                            !call ret_COO%add( &
                            !    nsig * ngp * (elem_idx-1) + nsig * (gp_idx-1) +  l, & ! gauss-point wise
                            !    nd * (node_idx_1 - 1) + k, & ! node-wise
                            !    K_e( l , nd*(i-1)+k ) &
                            !)

                        enddo
                    enddo
                enddo
            enddo
        enddo
        !$OMP end parallel do
!        ret = ret_COO%to_CRS()
        ret%val = val
        
    endif
    

    if(present(Psi))then
        this%Psi = Psi_buf 
    endif

! ! memo

                ! 2025/04/15 
                ! どこのStrain?
                ! StressとStrainを同じ場所で定義していないと，
                ! 構成則を厳密に満たさないおそれがある．
                ! 平均化するか?(=要素内で一定値とするか)

                ! とりあえず，節点で応力を定義している以上は，
                ! それと整合するように，節点でひずみをとる．
                ! そのため，
                ! e = Bu
                ! とするための，Bmatrixは，それぞれの節点
                ! に対応する局所座標により定義する．
                ! そのため，getStrainTensor関数で
                ! (変位，節点番号)
                ! を引数にとれるようにする．

                ! >> どのように?
                ! その節点が含まれる要素を探索し，局所座標を特定したうえで
                ! Bmatrixを計算し，e = Bu によりひずみを計算

                ! <課題>
                ! 1つの節点が複数の要素に属していた場合，
                ! e = Buは一意か?

                ! <eの一意性についての考察>
                ! uが連続であるが，uは節点およびエッジにおいて不連続となりうる．
                ! そのとき，du/dxは不定となりうる．
                ! よって，一意とは限らない．
                
                ! <ではどうするか>
                ! <1> 全要素で平均化して一意性をみたすようにする．
                ! <2> 要素内で一定値をとるようにする．

                ! <test>
                ! 1D-linear elementでやってみる．
                ! 線形要素の場合，ひずみは要素内で一定
                ! 2D-linear elemetの場合
                ! ひずみテンソルは要素内の至るところで異なる．
                ! ひずみテンソル = f(dN/dxi)*g(dN/d\xi) 
                ! では，連続性はどうか？ →　当然，節点で非連続
                
                ! <Conclusion>
                ! 応力とひずみはGauss pointで定義すべき．
                ! 当然，応力評価点はメッシュ読み込み後に決定．
                
                ! Gauss-point wise
                !ElasticStrain = this%femdomain%getStrainTensor(&
                !    displacement=this%u,&
                !    ElementID=elem_idx, &
                !    GaussPointID=gp_idx)


    

end function
! ####################################################






! ####################################################
function globalStressRatiolMatrix_SymEP(this,Psi) result(ret)
    class(DynamicEP_),intent(inout) :: this
    real(real64),optional,intent(in) :: Psi(:)

    ! under implementation 
    integer(int32) :: elem_idx,gp_idx,i,j,nd,nne,row_idx,col_idx,nsig,k,l,ne,&
        node_idx_1,node_idx_2,nn
    real(real64),allocatable :: dNdxi(:,:),N(:),C_mat(:,:),B_mat(:,:),ElasticStrain(:,:),&
        U(:),ElasticStrain_vec(:),U_e(:),W_tensor(:),val(:),Psi_buf(:)
    integer(int32),allocatable ::  stress_i(:),stress_j(:)
    real(real64) :: integral_val
    real(real64),allocatable :: W_e(:,:)
    integer(int32) :: ngp,row,col
    type(Shapefunction_) :: shapefunc
    type(COO_) :: ret_COO
    type(CRS_) :: ret
    type(IO_)  :: debug

    ! Validated >> 2025/04/24
    ! 亜弾性構成則の全体要素剛性行列　
    ! C B v
    if(present(Psi))then
        Psi_buf  = this%Psi 
        this%Psi = Psi 
    endif
    

    nd   = this%femdomain%nd()
    nn   = this%femdomain%nn()
    nne  = this%femdomain%nne()
    ne  = this%femdomain%ne()
    ngp  = this%femdomain%ngp()
    nsig = ((nd)*(nd-1))/2 + nd

    ! for 3-D case, with single element
    ! and NNE = 8, NN=8, nGP = 9
    ! matrix size = ( nsig * ngp * ne ,ND * NN )

    call ret_COO%init(nsig * ngp * ne)

    do elem_idx = 1, this%femdomain%ne()
        do gp_idx = 1, this%femdomain%ngp()
            do i=1,nsig
                do j=1,nsig
                    call ret_COO%add( &
                        nsig * ngp * (elem_idx-1) + nsig * (gp_idx-1) +  i, & ! gauss-point wise
                        nsig * ngp * (elem_idx-1) + nsig * (gp_idx-1) +  j, & ! gauss-point wise
                        0.0d0 &
                    )
                enddo
            enddo
        enddo
    enddo
    ret = ret_COO%to_CRS()
    call ret_COO%remove()
    val = ret%val
    deallocate(ret%val)
    val(:) = 0.0d0


    ! s(1,1),s(2,2),s(3,3)
    if(nd==1)then
        !stress_i = [1]
        stress_j = [1]
    endif
    if(nd==2)then
        !stress_i = [1,2,1]
        stress_j = [1,2,2]
    endif
    if(nd==3)then
        !stress_i = [1,2,3,1,2,3]
        stress_j = [1,2,3,2,3,1]
    endif

    ! ONLY FOR UNIFORM MESH which uses only a single type of element
    

    !$OMP parallel do private(gp_idx,W_e,i,j,k,row,col) &
    !$OMP  shared(this,nsig,ret) firstprivate(shapefunc) reduction(+:val)
    do elem_idx = 1, this%femdomain%ne()
        !call shapefunc%setType(NumOfDim=this%femdomain%nd(), &
        !    NumOfNodePerElem=this%femdomain%nne(), NumOfGp=this%femdomain%ngp())
        do gp_idx = 1, this%femdomain%ngp()
            call getAllShapeFunc(shapefunc, elem_id=elem_idx, nod_coord=this%femdomain%Mesh%NodCoord, &
                elem_nod=this%femdomain%Mesh%ElemNod, OptionalGpID=gp_idx)
            !dNdxi = shapefunc%dNdgzi
            !N = shapefunc%Nmat

            W_e = this%StressRatioMatrix(ElementID=elem_idx,GaussPointID=gp_idx)

            ! compute spin tensor
            do i=1,nsig
                do j=1,nsig
                    row = nsig * this%femdomain%ngp() * (elem_idx-1) + nsig * (gp_idx-1) +  i
                    col = nsig * this%femdomain%ngp() * (elem_idx-1) + nsig * (gp_idx-1) +  j
                    !ret%add
                    do k = ret%row_ptr(row), ret%row_ptr(row + 1) - 1
                        if (ret%col_idx(k) == col) then
                            val(k) = val(k) + W_e( i, j )
                            exit
                        end if
                    end do

                    !call ret_COO%add( &
                    !    nsig * ngp * (elem_idx-1) + nsig * (gp_idx-1) +  i, & ! gauss-point wise
                    !    nsig * ngp * (elem_idx-1) + nsig * (gp_idx-1) +  j, & ! gauss-point wise
                    !    W_e( i, j ) &
                    !)
                enddo
            enddo
        enddo
    enddo
    !$OMP end parallel do
    ret%val = val
!    ret = ret_COO%to_CRS()

    if(present(Psi))then
        this%Psi  = Psi_buf 
    endif

end function
! ####################################################

! ####################################################
function StressRatioMatrix_SymEP(this,ElementID,GaussPointID) result(ret)
    class(DynamicEP_),intent(in) :: this
    integer(int32),intent(in) :: ElementID,GaussPointID
    real(real64),allocatable :: ret(:,:),A(:,:),B(:,:),W_tensor(:,:),L_tensor(:,:)
    integer(int32) :: nd
    type(ShapeFunction_) :: shapefunc

    
    call getAllShapeFunc(shapefunc, elem_id=ElementID, nod_coord=this%femdomain%Mesh%NodCoord, &
        elem_nod=this%femdomain%Mesh%ElemNod, OptionalGpID=GaussPointID)
    
    
    L_tensor = this%femdomain%getVelocityGradient(&
        velocity=this%v, ElementID=ElementID, GaussPointID=GaussPointID)

    W_tensor = this%femdomain%getSpinTensor(&
        velocity=this%v, ElementID=ElementID, GaussPointID=GaussPointID)
    
    nd = size(L_tensor,1)

    if(this%stress_ratio == NONE)then
        A = 0.0d0*W_tensor(:,:)
        B = 0.0d0*W_tensor(:,:)
    elseif(this%stress_ratio == JAUMANN_STRESS_RATIO)then
        !  - w_{ik} s_{kj} + s_{ik} w_{kj}
        A =  - W_tensor(:,:)
        B =    W_tensor(:,:)
    elseif(this%stress_ratio == TRUESDELL_STRESS_RATIO)then
        ! - l_{ik} s_{kj} - s_{ik} l_{kj} + tr(l) s_{ij}
        A =  - L_tensor(:,:) + trace(L_tensor)*eyes(nd,nd)
        B =  - L_tensor(:,:)
    elseif(this%stress_ratio == OLDROYD_STRESS_RATIO)then
        ! - l_{ik} s_{kj} - s_{ik} l_{kj} 
        A =  - L_tensor(:,:)
        B =  - L_tensor(:,:)
    elseif(this%stress_ratio == CONVECTIVE_STRESS_RATIO)then
        !   l_{ik} s_{kj} + s_{ik} l_{kj} 
        A =    L_tensor(:,:)
        B =    L_tensor(:,:)
    else
        stop "THIS STRESS RATIO IS NOT IMPLEMENTED."
    endif

    ret = A_ik_sigma_kj_to_matrix(A=(A))
    ret = ret + sigma_ik_B_kj_to_matrix(B=(B))

end function
! ####################################################


! ################################################################
function A_ik_sigma_kj_to_matrix(A) result(ret)
    real(real64),intent(in)  :: A(:,:)
    real(real64),allocatable :: ret(:,:)
 
    ret = zeros(6,6)
    ret(1,:) = [ A(1,1), 0.00d0, 0.00d0,  A(1,2), 0.00d0, A(1,3)]
    ret(2,:) = [ 0.00d0, A(2,2), 0.00d0,  A(2,1), A(2,3), 0.00d0]
    ret(3,:) = [ 0.00d0, 0.00d0, A(3,3),  0.00d0, A(3,2), A(3,1)]
    ret(4,:) = [ 0.00d0, A(1,2), 0.00d0,  A(1,1), A(1,3), 0.00d0]
    ret(5,:) = [ 0.00d0, 0.00d0, A(2,3),  0.00d0, A(2,2), A(2,1)]
    ret(6,:) = [ A(3,1), 0.00d0, 0.00d0,  A(3,2), 0.00d0, A(3,3)]
 
 end function
 ! ################################################################
 

! ################################################################
 function sigma_ik_B_kj_to_matrix(B) result(ret)
    real(real64),intent(in)  :: B(:,:)
    real(real64),allocatable :: ret(:,:)
 
    ret = zeros(6,6)
    ret(1,:) = [ B(1,1), 0.00d0, 0.00d0,  B(2,1), 0.00d0, B(3,1)]
    ret(2,:) = [ 0.00d0, B(2,2), 0.00d0,  B(1,2), B(3,2), 0.00d0]
    ret(3,:) = [ 0.00d0, 0.00d0, B(3,3),  0.00d0, B(2,3), B(1,3)]
    ret(4,:) = [ B(1,2), 0.00d0, 0.00d0,  B(2,2), 0.00d0, B(3,2)]
    ret(5,:) = [ 0.00d0, B(2,3), 0.00d0,  B(1,3), B(3,3), 0.00d0]
    ret(6,:) = [ 0.00d0, 0.00d0, B(3,1),  0.00d0, B(2,1), B(1,1)]
    
 end function
 ! ################################################################
 
subroutine assign_vector_from_array(vec,mat) 
    real(real64),allocatable,intent(out) :: vec(:)
    real(real64),intent(in) :: mat(:,:)
    integer(int32) :: i,nc

    nc  = size(mat,2)
    vec = zeros(size(mat,1)*size(mat,2))
    do i=1,size(mat,1)
        vec( nc*(i-1)+1:nc*(i-1)+nc ) = mat( i,1:nc )
    enddo

end subroutine
 
! ################################################################
!> Get Cauchy's stress tensor from the state vector Psi.
function getCauchyStress_SymEP(this,ElementID, GaussPointID,last_step) result(ret)
    class(DynamicEP_),intent(in) :: this
    integer(int32),intent(in) :: ElementID, GaussPointID
    logical,optional,intent(in) :: last_step
    real(real64),allocatable :: ret(:,:),sigma_vec(:)
    integer(int32) :: nSigma, num_offset
    logical :: compute_stress_of_last_step

    compute_stress_of_last_step = input(default=.false.,option=last_step)

    nSigma = (this%femdomain%nd()+1)*this%femdomain%nd()/2
    !> skip velocity field
    num_offset = this%femdomain%nd()*this%femdomain%nn()
    !> consider offsets for stress
    num_offset = num_offset + nSigma*this%femdomain%ngp()*(ElementID-1) + nSigma*(GaussPointID-1)
    
    if(compute_stress_of_last_step)then
        sigma_vec = this%Psi_n(num_offset + 1: num_offset + nSigma )
    else
        sigma_vec = this%Psi(num_offset + 1: num_offset + nSigma )
    endif

    ret = zeros(this%femdomain%nd(),this%femdomain%nd())
    if(this%femdomain%nd()==1)then
        ret = sigma_vec(1)*ones(1)
    elseif(this%femdomain%nd()==2)then
        ret = zeros(2,2)
        ret(1,1) = sigma_vec(1)
        ret(2,2) = sigma_vec(2)
        ret(2,1) = sigma_vec(3)
        ret(1,2) = sigma_vec(3)
    elseif(this%femdomain%nd()==3)then
        ret = zeros(3,3)
        ret(1,1) = sigma_vec(1)
        ret(2,2) = sigma_vec(2)
        ret(3,3) = sigma_vec(3)
        ret(1,2) = sigma_vec(4)
        ret(2,3) = sigma_vec(5)
        ret(3,1) = sigma_vec(6)

        ret(2,1) = sigma_vec(4)
        ret(3,2) = sigma_vec(5)
        ret(1,3) = sigma_vec(6)
    endif
    
end function
! ################################################################


! ################################################################
function getStrainField_SymEP(this,invariant_type) result(ret)
    class(DynamicEP_),intent(in) :: this
    real(real64),allocatable :: ret(:),StrainTensor(:,:)
    character(*),intent(in) :: invariant_type
    integer(int32) :: elem_idx, gp_idx,nsig

    ret = zeros(this%femdomain%ne())
    do elem_idx=1,this%femdomain%ne()
        StrainTensor = zeros(this%femdomain%nd(),this%femdomain%nd())
        do gp_idx=1,this%femdomain%ngp()
            StrainTensor = StrainTensor + this%femdomain%getStrainTensor(&
                displacement=this%u,ElementID=elem_idx,GaussPointID=gp_idx)
        enddo
        ! Averaging
        StrainTensor = StrainTensor/dble(this%femdomain%ngp())
        if("J2" .in. invariant_type)then
            ! J2
            ret(elem_idx) = to_J2(StrainTensor)
        elseif("I1" .in. invariant_type)then
            ret(elem_idx) = to_I1(StrainTensor)
        else
            call print("[ERROR] getStrainField_SymEP >> argument invariant_type should be ")
            call print("I1 or J2")
        endif
    enddo


end function
! ################################################################


! ################################################################
function getStressField_SymEP(this,invariant_type) result(ret)
    class(DynamicEP_),intent(in) :: this
    real(real64),allocatable :: ret(:),StressTensor(:,:)
    character(*),intent(in) :: invariant_type
    integer(int32) :: elem_idx, gp_idx,nsig

    ret = zeros(this%femdomain%ne())
    do elem_idx=1,this%femdomain%ne()
        StressTensor = zeros(this%femdomain%nd(),this%femdomain%nd())
        do gp_idx=1,this%femdomain%ngp()
            StressTensor = StressTensor + this%getCauchyStress(&
                ElementID=elem_idx,GaussPointID=gp_idx)
        enddo
        ! Averaging
        StressTensor = StressTensor/dble(this%femdomain%ngp())

        if("J2" .in. invariant_type)then
            ! J2
            ret(elem_idx) = to_J2(StressTensor)
        elseif("I1" .in. invariant_type)then
            ret(elem_idx) = to_I1(StressTensor)
        else
            call print("[ERROR] getStrainField_SymEP >> argument invariant_type should be ")
            call print("I1 or J2")
        endif
    enddo


end function
! ################################################################


! ################################################################
!> compute element-wise f_ext
function localInternalForceVector_SymEP(this,ElementID) result(ret)
    class(DynamicEP_),intent(in) :: this
    real(real64),allocatable :: ret(:)
    integer(int32) ,intent(in) :: ElementID
    integer(int32) :: gp_idx
    type(Shapefunction_) :: shapefunc

    ! using following values
    ret = zeros(this%femdomain%nne()*this%femdomain%nd())

    call shapefunc%SetType(NumOfDim=this%femdomain%nd(),&
        NumOfNodePerElem=this%femdomain%nne(),&
        NumOfGp=this%femdomain%mesh%getNumOfGp())
    do gp_idx=1,this%femdomain%ngp()

        call shapefunc%getAll( elem_id=ElementID, &
                              nod_coord=this%femdomain%Mesh%NodCoord, &
                              elem_nod=this%femdomain%Mesh%ElemNod, OptionalGpID=gp_idx)
        
        
        ret = ret + matmul(transpose(this%femdomain%Bmatrix(shapefunction=shapefunc)),&
            to_stress_vector(this%getCauchyStress(ElementID=ElementID,GaussPointID=gp_idx) ))&
            *shapefunc%GaussIntegWei(gp_idx)*shapefunc%detJ
    enddo
    
    
end function
! ################################################################


function to_stress_vector(stress_tensor) result(ret)
    real(real64),intent(in) :: stress_tensor(:,:)
    real(real64),allocatable :: ret(:)

    if(size(stress_tensor,1)==1)then
        ret = stress_tensor(1,1)*ones(1)
    elseif(size(stress_tensor,1)==2)then
        allocate(ret(3))
        ret(1) = stress_tensor(1,1)
        ret(2) = stress_tensor(2,2)
        ret(3) = stress_tensor(1,2)
    elseif(size(stress_tensor,1)==3)then
        allocate(ret(6))
        ret(1) = stress_tensor(1,1)
        ret(2) = stress_tensor(2,2)
        ret(3) = stress_tensor(3,3)
        ret(4) = stress_tensor(1,2)
        ret(5) = stress_tensor(2,3)
        ret(6) = stress_tensor(3,1)
    else
        ret = zeros(0)
    endif

end function 
! ################################################################
!> compute global f_ext
function globalInternalForceVector_SymEP(this,range) result(ret)
    class(DynamicEP_),intent(in) :: this
    type(Range_),optional,intent(in) :: range
    real(real64),allocatable :: ret(:),vec(:)
    integer(int32),allocatable :: elemlist(:)
    integer(int32) :: elem_idx,nne_idx,idx

    ! using following values

    if (present(range)) then
        ret = zeros(this%femdomain%nn()*this%femdomain%nd())
        elemlist = this%femdomain%getElementList(range=range)
        do idx = 1,size(elemlist)
            elem_idx = elemlist(idx)
            vec = this%localInternalForceVector(ElementID=elem_idx)
            do nne_idx = 1, this%femdomain%nne()
                ret(this%femdomain%nd()*(this%femdomain%mesh%elemnod(elem_idx,nne_idx)-1)+1: &
                    this%femdomain%nd()*(this%femdomain%mesh%elemnod(elem_idx,nne_idx)) )  = &
                ret(this%femdomain%nd()*(this%femdomain%mesh%elemnod(elem_idx,nne_idx)-1)+1: &
                    this%femdomain%nd()*(this%femdomain%mesh%elemnod(elem_idx,nne_idx)) )  + &
                    vec(this%femdomain%nd()*(nne_idx-1)+1:&
                        this%femdomain%nd()*(nne_idx  ) )
            enddo
        enddo
    else
        ret = zeros(this%femdomain%nn()*this%femdomain%nd())
        do elem_idx = 1,this%femdomain%ne()
            vec = this%localInternalForceVector(ElementID=elem_idx)
            do nne_idx = 1, this%femdomain%nne()
                ret(this%femdomain%nd()*(this%femdomain%mesh%elemnod(elem_idx,nne_idx)-1)+1: &
                    this%femdomain%nd()*(this%femdomain%mesh%elemnod(elem_idx,nne_idx)) )  = &
                ret(this%femdomain%nd()*(this%femdomain%mesh%elemnod(elem_idx,nne_idx)-1)+1: &
                    this%femdomain%nd()*(this%femdomain%mesh%elemnod(elem_idx,nne_idx)) )  + &
                    vec(this%femdomain%nd()*(nne_idx-1)+1:&
                        this%femdomain%nd()*(nne_idx  ) )
            enddo
        enddo
    endif
end function
! ################################################################


! ################################################################
!> compute element-wise f_ext
function localExternalForceVector_SymEP(this,ElementID) result(ret)
    class(DynamicEP_),intent(in) :: this
    real(real64),allocatable :: ret(:)
    integer(int32) ,intent(in) :: ElementID
    integer(int32) :: gp_idx
    real(real64),allocatable :: Nmat(:,:),gravity_vector(:)
    type(Shapefunction_) :: shapefunc
    type(PhysicalConstants_) :: p_const

    ! using following values
    ret = zeros(this%femdomain%nne()*this%femdomain%nd())

    call shapefunc%SetType(NumOfDim=this%femdomain%nd(),&
        NumOfNodePerElem=this%femdomain%nne(),&
        NumOfGp=this%femdomain%mesh%getNumOfGp())
    gravity_vector = zeros(this%femdomain%nd())
    gravity_vector(size(gravity_vector)) = p_const%g

    do gp_idx=1,this%femdomain%ngp()

        call getAllShapeFunc(shapefunc, elem_id=ElementID, &
                              nod_coord=this%femdomain%Mesh%NodCoord, &
                              elem_nod=this%femdomain%Mesh%ElemNod, OptionalGpID=gp_idx)
        
        
        ret = ret + matmul(&
            to_stripe_repeat_matrix(shapefunc%Nmat,this%femdomain%nd()),&
                gravity_vector)*shapefunc%detJ*shapefunc%GaussIntegWei(gp_idx)
    enddo
    
    
end function
! ################################################################


! ################################################################
function to_stripe_repeat_matrix(vec,num_col) result(ret)
    real(real64),intent(in) :: vec(:)
    integer(int32),intent(in) :: num_col
    real(real64),allocatable :: ret(:,:)
    integer(int32) :: i,j

    allocate(ret(size(vec)*num_col,num_col))
    ret(:,:) = 0.0d0
    !(A)
    !(B)
    !(C)
    !(D)

    ! => 

    !(A, 0, 0)
    !(0, A, 0)
    !(0, 0, A)
    !(B, 0, 0)
    !(0, B, 0)
    !(0, 0, B)
    !(C, 0, 0)
    !(0, C, 0)
    !(0, 0, C)
    !(D, 0, 0)
    !(0, D, 0)
    !(0, 0, D)
    
    do i=1,size(vec)
        do j=1,num_col
            ret( num_col*(i-1) + j, j)  = vec(i)
        enddo
    enddo

end function 

! ################################################################
function globalExternalForceVector_SymEP(this) result(ret)
    class(DynamicEP_),intent(in) :: this
    real(real64),allocatable :: ret(:),vec(:)
    integer(int32) :: elem_idx,nne_idx

    ret = zeros(this%femdomain%nd()*this%femdomain%nn())
    if(maxval(this%density)==0.0d0)then
        return
    else
        ! only for body-force
        do elem_idx = 1,this%femdomain%ne()
        vec = this%localExternalForceVector(ElementID=elem_idx)
        do nne_idx = 1, this%femdomain%nne()
            ret(this%femdomain%nd()*(this%femdomain%mesh%elemnod(elem_idx,nne_idx)-1)+1: &
                this%femdomain%nd()*(this%femdomain%mesh%elemnod(elem_idx,nne_idx)  ) )  = &
                ret(this%femdomain%nd()*(this%femdomain%mesh%elemnod(elem_idx,nne_idx)-1)+1: &
                this%femdomain%nd()*(this%femdomain%mesh%elemnod(elem_idx,nne_idx)) )  + &
                vec(this%femdomain%nd()*(nne_idx-1)+1:&
                    this%femdomain%nd()*(nne_idx  ) )
        enddo

        ! add traction

    enddo
    
    endif

end function
! ################################################################

subroutine returnMapping_SymEP(this,du)
    class(DynamicEP_),intent(inout) :: this
    real(real64),intent(in) :: du(:)
    real(real64),allocatable :: CauchyStress(:,:),StrainTensor(:,:),d_StrainTensor(:,:),&
        CauchyStress_n(:,:)
    real(real64) :: epsilon
    integer(int32) :: elem_idx, gp_idx

!    ! return-mapping algorithm 
!    do elem_idx=1,this%femdomain%ne()
!        do gp_idx=1,this%femdomain%ngp()
!            ! 
!            StrainTensor   = 
!            d_StrainTensor = 
!            PlasticStrain  =
!            CauchyStress = to_StressTensor(&
!                    YieldFunction=this%YieldFunction,&
!                    PlasticPotential=this%PlasticPotential,&
!                    Strain=StrainTensor,&
!                    dStrain=d_StrainTensor,&
!                    CauchyStress=CauchyStress_n,&
!                    PlasticStrain=PlasticStrain,&
!                    YieldParams=this%YieldParams,&
!                    PlasticParams=this%PlasticParams,&
!                    ElasticParams=this%ElasticParams,&
!                    epsilon=epsilon &
!                )
!        enddo
!    enddo

end subroutine

! ################################################################

! destructor
subroutine remove_DynamicEP(this)
    class(DynamicEP_),intent(inout) :: this
    
    if(allocated(this%EP_Models))  deallocate (this%EP_Models)

    this%femdomain => null()
        

    if(allocated(this%Density)) deallocate(this%density)
    if(allocated(this%ElasticParams)) deallocate(this%ElasticParams)
    if(allocated(this%YieldParams)) deallocate(this%YieldParams)
    if(allocated(this%PlasticParams)) deallocate(this%PlasticParams)
    if(allocated(this%ElemEP_ModelIdx)) deallocate(this%ElemEP_ModelIdx)
    if(allocated(this%u)) deallocate(this%u)
    if(allocated(this%u_n)) deallocate(this%u_n)
    if(allocated(this%v)) deallocate(this%v)
    if(allocated(this%v_n)) deallocate(this%v_n)
    if(allocated(this%v)) deallocate(this%TractionBoundaryIdx)
    if(allocated(this%v_n)) deallocate(this%TractionBoundaryVal)
        

    this%force_elastic = .false.

    ! control parameters
    this% stress_ratio = 2 ! TRUESDELL_STRESS_RATIO
    !real(real64) :: time_integration_factor = 0.50d0 ! time-integration factor (theta \in [0,1])

        ! >> for computation
    call this % Upsilon% remove()
    if(allocated(this%Psi)) deallocate(this%Psi)
    if(allocated(this%Psi_n)) deallocate(this%Psi_n)
    if(allocated(this%Psi_tr)) deallocate(this%Psi_tr)
    if(allocated(this%ElasticStrain)) deallocate(this%ElasticStrain)
    if(allocated(this%PlasticStrain)) deallocate(this%PlasticStrain)
    if(allocated(this%yield_function_values_n)) deallocate(this%yield_function_values_n)
        

    this % cutoff_frequency=0.0d0

    this % max_itr = 100
    this % max_Newton_itr = 1000
    this % bicgstab_er = dble(1.0e-14)
    this % max_bicgstab_itr = 10000


    this % TimeIntegrationScheme = 0 !


end subroutine

end module