module GeneticControlClass
    use ODEClass
    use IOClass
    implicit none

    type :: Gene_
        character(:),allocatable :: name
        real(real64) :: expression ! \in [0,max_expression]
        real(real64) :: max_expression=1.0d0 ! default value is 1.0
        
    end type

    type :: Gene_info_
        type(Gene_) :: wild
        type(Gene_),allocatable :: mutants(:)
        real(real64) :: current_value=0.0d0 !default value is zero
    end type

    type :: input_signal_    
        character(:),allocatable :: name ! e.g. "LD :: Long-daylight"
        real(real64) :: current_value
        type(Gene_info_),pointer :: target_gene_info
        real(real64) :: threshold  ! 10 hour
        real(real64) :: gain       ! 1.0
        real(real64) :: weight     ! multiplier
        logical :: is_activator 
!    contains
!        procedure,public :: add_target_gene_info => add_target_gene_info_GC
    end type

    
    type :: output_signal_
        character(:),allocatable :: name ! e.g. "FT"
        real(real64) :: current_value
        type(Gene_info_),pointer :: related_gene_info ! e.g. E9 and GmFT5a
        real(real64) :: max_expression=1.0d0 ! default value is 1.0
        real(real64) :: weight   ! E9: 0.5 & GmFT5a: 0.5
        logical :: is_activator 
    end type

    

    type :: GeneticControl_
        type(ODE_) :: ode
        type(Gene_info_),allocatable :: gene_info(:)
        integer(int32),allocatable :: genotype(:)   
        type(input_signal_),allocatable :: input_signal(:)
        type(output_signal_),allocatable :: output_signal(:)
        real(real64),allocatable :: relation_matrix(:,:)
        real(real64) :: t = 0.0d0
        real(real64) :: dt = 0.0d0

        ! 発現量の半減期
        real(real64) :: half_life = 60.0d0*60.0d0*24.0d0 ! sec. = 1 day
    contains
        procedure,public :: define_Allele => define_Allele_GeneticControl
        procedure,public :: show_Allele_Database => show_Allele_Database_GC
        procedure,public :: show_input_signal_Database => show_input_signal_Database_GC
        procedure,public :: show_output_signal_Database => show_output_signal_Database_GC
        procedure,public :: get_gene_id => get_gene_id_GC
        procedure,public :: add_relation => add_relation_GC
        procedure,public :: add_input_signal => add_input_signal_GC
        procedure,public :: add_output_signal => add_output_signal_GC
        procedure,public :: get_gene_info => get_gene_info_GC
        procedure,public :: set_genotype => set_genotype_GC
        procedure,public :: get_genotype_and_allele_id => get_genotype_and_allele_id_GC
        procedure,public :: set_input_signal_value => set_input_signal_value_GC
        procedure,public :: update_time => update_time_GC
        procedure,public :: get_max_expression_value => get_max_expression_value_GC

        procedure,public :: num_gene          => num_gene_GC
        procedure,public :: num_input_signal  => num_input_signal_GC
        procedure,public :: num_output_signal => num_output_signal_GC
        procedure,public :: num_unique_output_signal => num_unique_output_signal_GC

        procedure,public :: write => write_GC
    end type


    interface operator(.in.)
        module procedure in_gene_info_GCClass, in_input_signal_GCClass
    end interface

contains

!> returns gene idx
function get_gene_id_GC(this,name) result(ret)
    class(GeneticControl_),intent(in) :: this
    character(*),intent(in) :: name
    integer(int32) :: ret
    integer(int32) :: itr,j

    ret = -1
    do itr = 1,size(this%gene_info)
        if(this%gene_info(itr)%wild%name == name)then
            ret = itr
            return
        endif
        do j=1,size(this%gene_info(itr)%mutants)
            if(this%gene_info(itr)%mutants(j)%name == name )then
                ret = itr
                return
            endif
        enddo
    enddo
    
    print *, "[ERROR] GeneticControlClass >> get_gene_id_GC >> invalid {gene_info_} name",name
    stop

end function

! search gene name (wildtype)
function in_gene_info_GCClass(name,gene_info) result(ret)
    character(*),intent(in) :: name
    type(gene_info_),intent(in) :: gene_info(:)
    integer(int32) :: i
    logical :: ret

    ret = .False.
    do i=1,size(gene_info)
        if(gene_info(i)%wild%name == name)then
            ret = .True.
        endif
    enddo

end function

! search signal name
function in_input_signal_GCClass(name,input_signal) result(ret)
    character(*),intent(in) :: name
    type(input_signal_),intent(in) :: input_signal(:)
    integer(int32) :: i
    logical :: ret

    ret = .False.
    do i=1,size(input_signal)
        if(input_signal(i)%name == name)then
            ret = .True.
        endif
    enddo

end function


subroutine define_Allele_GeneticControl(this,wild,mutant,max_expression)
    class(GeneticControl_),intent(inout) :: this
    type(Gene_info_),allocatable :: gene_info_buf(:)
    type(Gene_),allocatable :: mutants_buf(:)
    character(*),intent(in) :: wild
    character(*),intent(in) :: mutant
    real(real64),intent(in) :: max_expression
    integer(int32) :: i,j,n

    if(.not.allocated(this%gene_info))then
        allocate(this%gene_info(1))
        allocate(this%gene_info(1)%mutants(1))
        this%gene_info(1)%wild%name = wild
        this%gene_info(1)%wild%expression = 0.0d0
        this%gene_info(1)%wild%max_expression = 1.0d0
        
        this%gene_info(1)%mutants(1)%name = mutant
        this%gene_info(1)%mutants(1)%expression = 0.0d0
        this%gene_info(1)%mutants(1)%max_expression = max_expression

        
    elseif(wild .in. this%gene_info(:)) then
        do i=1,size(this%gene_info)
            if(this%gene_info(i)%wild%name == wild)then
                mutants_buf = this%gene_info(i)%mutants
                deallocate(this%gene_info(i)%mutants)
                allocate(this%gene_info(i)%mutants(size(mutants_buf)+1))
                do j=1,size(mutants_buf)
                    this%gene_info(i)%mutants(j) = mutants_buf(j)
                enddo
                n = size(mutants_buf)+1
                this%gene_info(i)%mutants(n)%name = mutant
                this%gene_info(i)%mutants(n)%expression = 0.0d0
                this%gene_info(i)%mutants(n)%max_expression = max_expression
            endif
        enddo
    else
        !> extend this%gene_info array
        gene_info_buf = this%gene_info
        deallocate(this%gene_info)
        n = size(gene_info_buf)+1
        allocate(this%gene_info(n))
        allocate(this%gene_info(n)%mutants(1))

        do i=1,size(gene_info_buf)
            this%gene_info(i) = gene_info_buf(i)
        enddo
        !> ... and pop.
        
        this%gene_info(n)%wild%name = wild
        this%gene_info(n)%wild%expression = 0.0d0
        this%gene_info(n)%wild%max_expression = 1.0d0

        this%gene_info(n)%mutants(1)%name = mutant
        this%gene_info(n)%mutants(1)%expression = 0.0d0
        this%gene_info(n)%mutants(1)%max_expression = max_expression
    endif

end subroutine
! #############################################

!> show allele database
subroutine show_Allele_Database_GC(this)
    class(GeneticControl_),intent(in) :: this
    integer(int32) :: i,j

    do i=1,size(this%gene_info)
        print *, "ID :: ",i
        print *, "  Wildtype :: "
        print *, "      ",this%gene_info(i)%wild%name,this%gene_info(i)%wild%max_expression
        print *, "  Mutants  :: "
        do j=1,size(this%gene_info(i)%mutants)
            print *, "    ",this%gene_info(i)%mutants(j)%name,this%gene_info(i)%mutants(j)%max_expression
        enddo
    enddo
    

end subroutine


!> show input signal Database 
subroutine show_input_signal_Database_GC(this)
    class(GeneticControl_),intent(in) :: this
    integer(int32) :: i,j

    do i=1,size(this%input_signal)
        print *, ""
        print *, "ID :: ",i
        print *, "  Input signal name :: "
        print *, "      ",this%input_signal(i)%name
        print *, "  Target gene (WT)  :: "
        print *, "    ",this%input_signal(i)%target_gene_info%wild%name
        
        print *, "  Sigmoid function parameters  :: (threshold, gain)"
        print *, "      ",this%input_signal(i)%threshold,this%input_signal(i)%gain
    enddo
end subroutine

!> show output signal Database 
subroutine show_output_signal_Database_GC(this)
    class(GeneticControl_),intent(in) :: this
    integer(int32) :: i,j

    do i=1,size(this%output_signal)
        print *, ""
        print *, "ID :: ",i
        print *, "  output signal name :: "
        print *, "      ",this%output_signal(i)%name
        print *, "  Related gene (WT)  :: "
        print *, "    ",this%output_signal(i)%related_gene_info%wild%name
        
        print *, "  Sigmoid function parameters  :: (weight)"
        print *, "      ",this%output_signal(i)%weight
    enddo
end subroutine


!> add relation between two genes (activation or supression)
subroutine add_relation_GC(this,factor1,act_or_sup,factor2,coeff)
    class(GeneticControl_),intent(inout) :: this
    character(*),intent(in) :: factor1, factor2, act_or_sup
    real(real64),intent(in) :: coeff
    integer(int32) :: idx_1, idx_2

    this%ode%real_positive_value = True
    if(.not.allocated(this%relation_matrix))then
        allocate(this%relation_matrix(this%num_gene(),this%num_gene())) ! number of gene
    endif

    idx_1 = this%get_gene_id(name=factor1)
    idx_2 = this%get_gene_id(name=factor2)

    if( ("-|" .in. act_or_sup) .or. ("=|" .in. act_or_sup) )then
        this%relation_matrix(idx_2,idx_1) = - coeff
    else
        this%relation_matrix(idx_2,idx_1) =   coeff
    endif

end subroutine


!> registering input signal

!! in/outシグナルはin/outシグナルのリストが必要
!! It does not check duplication.
subroutine add_input_signal_GC(this,signal,act_or_sup,target_gene,weight,threshold,gain)
    class(GeneticControl_),target,intent(inout) :: this
    character(*),intent(in) :: signal, target_gene, act_or_sup
    real(real64),intent(in) :: threshold,gain,weight
    real(real64) :: coeff
    integer(int32) :: gene_idx,n,i
    type(input_signal_),allocatable :: input_signal_buf(:)

    if(.not.allocated(this%input_signal))then
        allocate(this%input_signal(1) )
        
        this%input_signal(1)%name = signal
        this%input_signal(1)%target_gene_info => this%get_gene_info(name=target_gene)
        this%input_signal(1)%threshold = threshold
        this%input_signal(1)%gain = gain
        this%input_signal(1)%weight = weight
        this%input_signal(1)%current_value = 0.0d0
        
        if( ("-|" .in. act_or_sup) .or. ("=|" .in. act_or_sup) )then
            this%input_signal(1)%is_activator = .false.
        else
            this%input_signal(1)%is_activator = .true.
        endif
    else
        ! This signal is not registrated.
        input_signal_buf = this%input_signal
        deallocate(this%input_signal)
        n = size(input_signal_buf)+1
        allocate(this%input_signal(n))
        
        do i=1,size(input_signal_buf)
            this%input_signal(i) = input_signal_buf(i)
        enddo
        this%input_signal(n)%name = signal
        this%input_signal(n)%target_gene_info => this%get_gene_info(name=target_gene)
        this%input_signal(n)%threshold = threshold
        this%input_signal(n)%gain = gain
        this%input_signal(n)%weight = weight
        this%input_signal(n)%current_value = 0.0d0
        
        if( ("-|" .in. act_or_sup) .or. ("=|" .in. act_or_sup) )then
            this%input_signal(n)%is_activator = .false.
        else
            this%input_signal(n)%is_activator = .true.
        endif
    endif

end subroutine

!> 
function get_gene_info_GC(this,name) result(ret_gene_info)
    class(GeneticControl_),target,intent(in) :: this
    character(*),intent(in) :: name
    type(Gene_info_),pointer :: ret_gene_info
    integer(int32) :: i,n

    n = size(this%gene_info)
    do i=1,n
        if(this%gene_info(i)%wild%name == name) then
            ret_gene_info => this%gene_info(i)
            return
        endif
    enddo

    print *, "[ERROR] GeneticControlClass >> get_gene_info_GC >> invalid {gene_info_} name",name
    stop

end function


!> add output signal
subroutine add_output_signal_GC(this,related_gene,act_or_sup,output_signal,weight)
    class(GeneticControl_),target,intent(inout) :: this
    character(*),intent(in) :: output_signal, related_gene, act_or_sup
    real(real64),intent(in) :: weight

    real(real64) :: coeff
    integer(int32) :: gene_idx,n,i
    type(output_signal_),allocatable :: output_signal_buf(:)

    if(.not.allocated(this%output_signal))then
        allocate(this%output_signal(1) )
        
        this%output_signal(1)%name = output_signal
        this%output_signal(1)%related_gene_info => this%get_gene_info(name=related_gene)
        this%output_signal(1)%weight = weight
        this%output_signal(1)%current_value = 0.0d0
       
        
        if( ("-|" .in. act_or_sup) .or. ("=|" .in. act_or_sup) )then
            this%output_signal(1)%is_activator = .false.
        else
            this%output_signal(1)%is_activator = .true.
        endif
    else
        ! This signal is not registrated.
        output_signal_buf = this%output_signal
        deallocate(this%output_signal)
        n = size(output_signal_buf)+1
        allocate(this%output_signal(n))
        
        do i=1,size(output_signal_buf)
            this%output_signal(i) = output_signal_buf(i)
        enddo
        this%output_signal(n)%name = output_signal
        this%output_signal(n)%related_gene_info => this%get_gene_info(name=related_gene)
        this%output_signal(n)%weight = weight
        this%output_signal(n)%current_value = 0.0d0
        
        
        if( ("-|" .in. act_or_sup) .or. ("=|" .in. act_or_sup) )then
            this%output_signal(n)%is_activator = .false.
        else
            this%output_signal(n)%is_activator = .true.
        endif
    endif

end subroutine


!> set genotype of this cultivar
!> genotype=0: wildtype, genotype>1: corresponding mutants
subroutine set_genotype_GC(this,genotype_name)
    class(GeneticControl_),intent(inout) :: this
    character(*),intent(in) :: genotype_name
    integer(int32) :: i, idx(1:2)


    if(.not.allocated(this%genotype))then
        this%genotype = int(zeros(size(this%gene_info))) 
    endif

    idx = this%get_genotype_and_allele_id(genotype_name=genotype_name)
    this%genotype(idx(1)) = idx(2)

end subroutine

!> genotype_idx and allele idx(0: wildtype, >1: mutants)
function get_genotype_and_allele_id_GC(this,genotype_name) result(ret_idx)
    class(GeneticControl_),intent(in) :: this
    character(*),intent(in) :: genotype_name
    integer(int32) :: i, j
    integer(int32) :: ret_idx(1:2)

    ret_idx(1:2) = [-1,-1]
    do i=1,size(this%gene_info)
        if(this%gene_info(i)%wild%name == genotype_name)then
            ret_idx(1:2) = [i,0]
            return
        endif
        do j=1,size(this%gene_info(i)%mutants)
            if(this%gene_info(i)%mutants(j)%name == genotype_name)then
                ret_idx(1:2) = [i,j]
                return
            endif
        enddo
        
    enddo

    print *, "[ERROR] GeneticControlClass >> get_genotype_and_allele_id_GC >> invalid {gene_info_} name",genotype_name
    stop

    
end function

!> set input signal value 
subroutine set_input_signal_value_GC(this,input_signal_name,current_value)
    class(GeneticControl_),intent(inout) :: this
    character(*),intent(in) :: input_signal_name
    real(real64),intent(in) :: current_value
    integer(int32) :: i,n

    n = 0
    do i=1,size(this%input_signal)
        if(this%input_signal(i)%name == input_signal_name)then
            this%input_signal(i)%current_value = current_value
            n =n + 1   
        endif
    enddo
    
    if(n==0)then
        print *, "[ERROR] GeneticControlClass >> set_input_signal_value_GC >> invalid {input_signal_} name",&
            input_signal_name
        stop
    endif
    

end subroutine


!> update_time
subroutine update_time_GC(this,dt)
    class(GeneticControl_),intent(inout) :: this
    real(real64),intent(in) :: dt
    integer(int32) :: i, j, n, idx, gene_idx, this_idx
    real(real64) :: val, lambda
    type(List_) :: output_signal_list

    this%t  = this%t + dt
    this%dt = dt

    n = this%num_gene() + this%num_unique_output_signal()

    !> [1]following parts setup ODE
    call this%ode%init(n)
    
    !> [1-1]set coefficient matrix
    do i=1,this%num_gene()
        do j=1,this%num_gene()
            if(i==j)then
                ! 現在の発現量は放っておくと指数関数的に減少
                ! 半減期からlambdaを計算
                lambda = log(2.0d0)/this%half_life 
                this%ode%A(i,j) = - lambda
                    ! x = x_0 * exp(-labmda*t)
            endif
            if(this%genotype(i)>=1)then
                ! mutant
                !> 不等式制約のときのEIって...?
                !> 要検討
                !> とりあえず，=0制約ならマトリクスをゼロにすればよい．
                if(this%get_max_expression_value(i)==0.0d0)then
                    this%ode%A(i,j) = 0.0d0
                else
                    !> 一旦，0でなければ係数行列は不変でいく．
                    this%ode%A(i,j) = this%relation_matrix(i,j)
                endif
            else
                ! wildtype
                this%ode%A(i,j) = this%relation_matrix(i,j)
            endif
        enddo
    enddo

    !> [1-2] constant value vector (input signal)
    !> 入力シグナルの強さは，それを受容し活性化する遺伝子の発現量の時間微分に比例させる．
    !> [要検討] シグナルが増加後減少してゼロになった場合には，
    !> 発現量を低減させる? または増加させる？
    do i=1,size(this%input_signal)
        gene_idx = this%get_gene_id(this%input_signal(i)%target_gene_info%wild%name)
        val = this%input_signal(i)%weight*sigmoid(&
                x=this%input_signal(i)%current_value,&
                params=[this%input_signal(i)%gain,this%input_signal(i)%threshold] &
            )
        this%ode%const(gene_idx) = this%ode%const(gene_idx) + val 
    enddo
    
    ! [1-3] constant value vector (output signal)
    !> 出力シグナルの強さの時間微分は，それを受容し活性化する遺伝子の発現量に比例させる．
    do i=1,this%num_output_signal()
        ! 既に登録済みのシグナルがある場合，かぶせる
        if(this%output_signal(i)%name .in. output_signal_list)then
            this_idx = output_signal_list%getIdx(this%output_signal(i)%name)
        else
            call output_signal_list%append(this%output_signal(i)%name)
            this_idx = output_signal_list%size()
        endif

        gene_idx = this%get_gene_id(this%output_signal(i)%related_gene_info%wild%name)
        !> [weight] * [expression amount]
        val = this%output_signal(i)%weight*this%output_signal(i)%related_gene_info%current_value
        if(.not.(this%output_signal(i)%is_activator))then
            val = -val
        endif
        this%ode%A(this%num_gene()+this_idx,gene_idx) =  val
    enddo
    
    !> [1-2]set initial condition
    idx = 0
    do i=1,this%num_gene()
        idx = idx + 1
        this%ode%x(idx) = this%gene_info(i)%current_value
    enddo
    
    do j=1,output_signal_list%size()
        ! "OUTPUT#1", "OUTPUT#2", "OUTPUT#3", ...
        do i=1,this%num_output_signal()    
            if(output_signal_list%get(j)==this%output_signal(i)%name)then
                idx = idx + 1
                this%ode%x(idx) = this%output_signal(i)%current_value
                exit
            endif
        enddo
    enddo

    !> [1-3]all quantities are to be non-negative 
    this%ode%real_positive_value = .true. 



    !> [2] solve ODE
    this%ode%x =  this%ode%solve(dt=dt)



    !> [3]update current values
    idx = 0
    do i=1,this%num_gene()
        idx = idx + 1
        !> if this trial value is greater than the max. value, limit it.
        !> x \in [0, max_value]
        this%gene_info(i)%current_value = this%ode%x(idx)
        if(this%gene_info(i)%current_value > this%get_max_expression_value(i))then
            this%gene_info(i)%current_value = this%get_max_expression_value(i)
        elseif(this%gene_info(i)%current_value < 0.0d0)then
            this%gene_info(i)%current_value = 0.0d0
        else
            this%gene_info(i)%current_value = this%ode%x(idx)
        endif
        this%ode%x(idx) = this%gene_info(i)%current_value 

        if(this%gene_info(i)%current_value > 1.0d0)then
            print *, "debug",this%gene_info(i)%current_value,this%get_max_expression_value(i)
            stop
        endif
    enddo

    do j=1,output_signal_list%size()
        ! "OUTPUT#1", "OUTPUT#2", "OUTPUT#3", ...
        idx = idx + 1
        do i=1,this%num_output_signal()    
            if(output_signal_list%get(j)==this%output_signal(i)%name)then
                this%output_signal(i)%current_value = this%ode%x(idx)

                if(this%output_signal(i)%current_value > this%get_max_expression_value(i))then
                    this%output_signal(i)%current_value = this%get_max_expression_value(i)
                elseif(this%output_signal(i)%current_value < 0.0d0)then
                    this%output_signal(i)%current_value = 0.0d0
                else
                    this%output_signal(i)%current_value = this%ode%x(idx)
                endif
                
            endif
        enddo
    enddo

    
    

end subroutine

function get_max_expression_value_GC(this,idx) result(ret)
    class(GeneticControl_),intent(in) :: this
    integer(int32),intent(in) :: idx
    real(real64) :: ret

    if(this%genotype(idx)==0)then
        ! wildtype
        ret = this%gene_info(idx)%wild%max_expression
    else
        ! mutant
        ret = this%gene_info(idx)%mutants(this%genotype(idx))%max_expression
    endif

end function


!> return a number of gene
function num_gene_GC(this) result(ret)
    class(GeneticControl_),intent(in) :: this
    integer(int32) :: ret

    ret = size(this%gene_info)

end function


!> return a number of input signal
function num_input_signal_GC(this) result(ret)
    class(GeneticControl_),intent(in) :: this
    integer(int32) :: ret

    ret = size(this%input_signal)

end function



!> return a number of output signal
function num_output_signal_GC(this) result(ret)
    class(GeneticControl_),intent(in) :: this
    integer(int32) :: ret

    ret = size(this%output_signal)

end function

function num_unique_output_signal_GC(this) result(ret)
    class(GeneticControl_),intent(in) :: this
    integer(int32) :: ret,i
    type(List_) :: output_signal_list
    
    do i=1,size(this%output_signal)
        if(this%output_signal(i)%name .in. output_signal_list)then
            cycle
        else
            call output_signal_list%append(this%output_signal(i)%name)
        endif
    enddo

    ret = output_signal_list%size()

end function


!> write data
subroutine write_GC(this,name,time_unit)
    class(GeneticControl_),intent(in) :: this
    character(*),intent(in) :: name
    character(*),optional,intent(in) :: time_unit
    
    type(IO_) :: f
    integer(int32) :: i,j
    real(real64) :: time_unit_val

    time_unit_val = 1.0d0 ! default 1 sec.
    if(present(time_unit))then
        if(time_unit(1:1)=="m" .or.time_unit(1:1)=="M" )then
            time_unit_val = 60.0d0
        endif
        if(time_unit(1:1)=="h" .or.time_unit(1:1)=="H" )then
            time_unit_val = 60.0d0*60.0d0
        endif
        if(time_unit(1:1)=="d" .or.time_unit(1:1)=="D" )then
            time_unit_val = 60.0d0*60.0d0*24.0d0
        endif
    endif
    
    do i = 1,size(this%gene_info)
        call f%open(name+"-"+this%gene_info(i)%wild%name+".txt","a")
            write(f%fh,*) this%t/time_unit_val, this%gene_info(i)%current_value
        call f%close()
    enddo
    

    do i = 1,size(this%input_signal)
        call f%open(name+"-"+this%input_signal(i)%name+".txt","a")
            write(f%fh,*) this%t/time_unit_val, this%input_signal(i)%current_value
        call f%close()
    enddo
    

    do i = 1,size(this%output_signal)
        call f%open(name+"-"+this%output_signal(i)%name+".txt","a")
            write(f%fh,*) this%t/time_unit_val, this%output_signal(i)%current_value
        call f%close()
    enddo
    
end subroutine

end module 
