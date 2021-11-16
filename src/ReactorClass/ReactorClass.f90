! #############################
module ReactorClass
    use MathClass
    implicit none
  
    integer(int32),parameter :: PF_MAX_REACTION_NUM = 20
    integer(int32),parameter :: PF_MAX_INPUT_NUM = 5
    integer(int32),parameter :: PF_MAX_OUTPUT_NUM = 5
  
    type :: Reactor_
      ! substances
      type(Real64Ptr_), allocatable :: substances(:)
      type(String_),    allocatable :: substance_names(:)
      integer :: n_substances=0
      
      ! reactions
      integer :: n_reaction=0
      integer(int32):: input_list(PF_MAX_REACTION_NUM,PF_MAX_INPUT_NUM) = 0
      integer(int32):: output_list(PF_MAX_REACTION_NUM,PF_MAX_OUTPUT_NUM) = 0
      real(real64)  :: reaction_order(PF_MAX_REACTION_NUM,PF_MAX_INPUT_NUM+PF_MAX_OUTPUT_NUM) = 0
      real(real64)  :: mol_rate(PF_MAX_REACTION_NUM,PF_MAX_INPUT_NUM+PF_MAX_OUTPUT_NUM) = 0
      real(real64)  :: constants(PF_MAX_REACTION_NUM) = 0
  
    contains
      procedure :: Init => InitReactor
      procedure :: put => putReactor
      procedure :: define => defineReactor
      procedure :: SearchSubstanceID=>SearchSubstanceIDReactor
      procedure :: run => runReactor
    end type
  contains
  
  ! ---------------------------
  subroutine InitReactor(this, max_num_substances)
    class(Reactor_), intent(inout) :: this
    integer(int32),optional, intent(in) :: max_num_substances
    integer(int32) :: n
  
    if(present(max_num_substances) )then
      n = max_num_substances
    else
      n = 100
    endif
    this%n_substances = 0
  
    if(allocated(this%substances) )then
      deallocate(this%substances)
    end if
    if(allocated(this%substance_names) )then
      deallocate(this%substance_names)
    end if
  
    allocate(this%substances(n))
    allocate(this%substance_names(n))
    
  end subroutine InitReactor
  ! ---------------------------
  
  
  ! ---------------------------
  subroutine putReactor(this, substance_name, substance)
    class(Reactor_), intent(inout) :: this
    character(*), intent(in) :: substance_name
    real(real64),target,intent(in) :: substance
    integer(int32) :: i,num
    num = this%n_substances + 1
  
    if(num>size(this%substances ) )then
      print *, "ERROR :: putReactor num>size(this%substances ) "
    else
      this%substances(num)%ptr => substance
      this%substance_names(num) = substance_name
    endif  
    this%n_substances =this%n_substances + 1
  
  end subroutine putReactor
  ! ---------------------------
  
  pure function SearchSubstanceIDReactor(this,name) result(ret)
    class(Reactor_), intent(in) :: this
    character(*),intent(in) :: name
    integer(int32) :: i, ret
  
    ret = 0
    do i=1,this%n_substances
      if(trim(this%substance_names(i)%all)==trim(name) )then
        ret = i
        return
      endif
    enddo
  
  end function
  
  ! ---------------------------
  subroutine defineReactor(this, input_list,output_list,constant,mol_rate,reaction_order)
    class(Reactor_), intent(inout) :: this
    type(String_),intent(in) :: input_list(:)
    type(String_),intent(in) :: output_list(:)
    real(real64),intent(in) :: constant ! reacton constant k
    real(real64),optional,intent(in) :: reaction_order(:),mol_rate(:)
    real(real64),allocatable :: mrate(:),rrate(:)
    integer(int32) :: i,j,n
    
    this%n_reaction = this%n_reaction + 1
  
  
    if(present(mol_rate)  )then
      mrate = mol_rate
    else
      n = size(input_list) + size(output_list)
      allocate(mrate(n) )
      mrate(:) = 1.0d0
    endif
  
    if(present(reaction_order)  )then
      rrate = reaction_order
    else
      n = size(input_list) + size(output_list)
      allocate(rrate(n) )
      rrate(:) = 1.0d0
    endif
  
    if(size(input_list) > PF_MAX_INPUT_NUM .or. size(output_list) > PF_MAX_OUTPUT_NUM )then
      print *, "ERROR :: defineReactor size(mrate) > PF_MAX_INPUT_NUM+PF_MAX_OUTPUT_NUM"
      stop
    endif
    ! register reaction
    ! INPUT
    do i=1,size(input_list)
      ! search substance id
      this%input_list(this%n_reaction,i) = this%SearchSubstanceID( trim(input_list(i)%all))
    enddo
  
  
    do i=1,size(output_list)
      ! search substance id
      this%output_list(this%n_reaction,i) = this%SearchSubstanceID( trim(output_list(i)%all ))
    enddo
  
    this%reaction_order(this%n_reaction,1:size(mrate) ) = mrate(1:size(mrate) )
    this%mol_rate(this%n_reaction,1:size(mrate) ) = mrate(1:size(mrate) )
  
    this%constants(this%n_reaction) = constant
  
  end subroutine
  ! ---------------------------
  
  subroutine runReactor(this, dt)
    class(Reactor_),intent(inout) :: this
    real(real64),intent(in) :: dt
    integer(int32) :: i,j,num_input,num_output
    real(real64) :: A_B_ratio,xi,xi_n,coef,A0,B0,nA,nB
    real(real64) :: k1,k2, k3, k4
  
    ! run simulation
    if(this%n_reaction==0)then
      print *, "ERROR :: runReactor this%n_reaction==0"
      stop
    elseif(this%n_reaction==1)then
      ! single reaction equation
      num_input = 0
      do i=1,size(this%input_list,2)
        if(this%input_list(1,i)/=0)then
          num_input = num_input + 1
        endif
      enddo
      num_output = 0
      do i=1,size(this%output_list,2)
        if(this%output_list(1,i)/=0)then
          num_output = num_output + 1
        endif
      enddo
      if(num_input==1 .and. num_output==1)then
        ! x*A -> y*B reaction equation
        print *, "[ok] Reactor Started >> "
        A_B_ratio = this%reaction_order(1,2) / this%reaction_order(1,1)
        this%substances(this%input_list(1,1))%ptr = this%substances(this%input_list(1,1))%ptr *exp(this%constants(1)*dt)
        this%substances(this%output_list(1,1))%ptr = this%substances(this%output_list(1,1))%ptr *exp(A_B_ratio*this%constants(1)*dt)
      elseif(num_input==2 .and. num_output==1)then
        print *, "[ok] Reactor Started >> "
        ! x*A + y*B -> z*C reaction equation
        ! A + y/x*B -> z/x*C reaction equation
        ! by Runge-Kutta mehod
        ! Reacted A = A0 - xi
        ! d xi/dt = k*(A0-xi)**(nA) * (B0 - xi)**(nB)
        ! 反応比率も必要！！
        ! 反応速度式の次数は反応比率によらないが，
        ! 反応量は反応比率によるため．
        coef = this%constants(1)
        
        A0 = this%substances(this%input_list(1,1))%ptr
        B0 = this%substances(this%input_list(1,2))%ptr
        nA = this%reaction_order(1,1)
        nB = this%reaction_order(1,2)
        xi_n = 0.0d0
        k1 = coef*((A0-xi_n)**(nA)) *( (B0 - xi_n)**(nB))
        k2 = coef*((A0- (xi_n + dt*0.50d0*k1 ) )**(nA)) &
            * ( (B0 -  (xi_n + dt*0.50d0*k1 ) )**(nB))
        k3 = coef*((A0- (xi_n + dt*0.50d0*k2 ) )**(nA)) &
            * ( (B0 -  (xi_n + dt*0.50d0*k2 ) )**(nB))
        k4 = coef*((A0- (xi_n + dt*k3 ) )**(nA)) &
            * ( (B0 -  (xi_n + dt*k3 ) )**(nB))
        xi = xi_n + 1.0d0/6.0d0*dt*(k1+k2+k3+k4)
  
        this%substances(this%input_list(1,1))%ptr = A0 - xi
        this%substances(this%input_list(1,2))%ptr = &
          B0 - dble(this%mol_rate(1,2)/this%mol_rate(1,1))* xi
        
        this%substances(this%output_list(1,1))%ptr = &
          this%substances(this%output_list(1,1))%ptr &
          + dble(this%mol_rate(1,3)/this%mol_rate(1,1))* xi
      else
        print *, "ERROR :: runReactor num_input/=1 .or. num_output/=1, now being implemented."
        stop  
      endif
  
  
    else
      print *, "ERROR :: runReactor this%n_reaction>=2, now being implemented."
      stop
    endif
  end subroutine
  
  
  end module ReactorClass
  ! #############################
  
  
  
  
  