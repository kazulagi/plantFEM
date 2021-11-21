module LoggerClass
    use MathClass
    use StringClass
    use IOClass
    use ArrayClass
    implicit none
  
    integer(int32) :: PF_MAX_CHANNEL_NUM=100
  
    type :: Logger_
      type(String_),allocatable     :: channel_name(:)
      type(Real64Ptr_) ,allocatable :: channel_value(:)
      integer(int32)   ,allocatable :: channel_id(:)
      logical   ,allocatable :: channel_active(:)
      logical :: initialized = .False.
      integer(int32)::counter=0
    contains
      procedure :: init => initLogger
      procedure :: numchannel =>numchannelLogger
      procedure :: set => setLogger
      procedure :: start => startLogger
      procedure :: save => saveLogger
      procedure :: reset => resetLogger
      
    end type
  contains
  
  !------------------------------------------------------
  subroutine initLogger(this,MAX_CHANNEL_NUM)
    class(Logger_),intent(inout) :: this
    integer(int32),optional,intent(in) :: MAX_CHANNEL_NUM
    integer(int32) :: n
  
    if(allocated(this%channel_name) ) then
      deallocate(this%channel_name)
    endif
    if(allocated(this%channel_value) ) then
      deallocate(this%channel_value)
    endif
    if(allocated(this%channel_id) ) then
      deallocate(this%channel_id)
    endif
    if(allocated(this%channel_active) ) then
      deallocate(this%channel_active)
    endif
  
    n = input(default=PF_MAX_CHANNEL_NUM,option=MAX_CHANNEL_NUM)
  
    allocate(this%channel_name(n) )
    allocate(this%channel_value(n) )
    allocate(this%channel_id(n) )
    allocate(this%channel_active(n) )
  
    this%channel_id(:) = 0
    this%channel_active(:) = .False.
    this%initialized = .True.
  
  end subroutine
  !------------------------------------------------------
  
  
  !------------------------------------------------------
  function numchannelLogger(this) result(ret)
    class(Logger_),intent(inout) :: this
    integer(int32) :: ret,i,n
  
    ret = 0
    do i=1,size(this%channel_active)
      if(this%channel_active(i) )then
        ret = ret + 1
      else
        cycle
      endif
    enddo
  end function
  !------------------------------------------------------
  
  
  !------------------------------------------------------
  subroutine setLogger(this,channel_name,channel_value,channel_id)
    class(Logger_),intent(inout) :: this
    character(*),intent(in) :: channel_name
    real(real64),target,intent(in) :: channel_value
    integer(int32),optional,intent(in) :: channel_id  
    integer(int32) :: n
    
    if(.not. this%initialized) then
      call this%init()
    endif
  
    if(present(channel_id)) then
      n = channel_id
    else
      n = this%numchannel() + 1
    endif
  
    this%channel_name(n) =  channel_name
  
    this%channel_value(n)%ptr => channel_value
    this%channel_id(n)   = n
    this%channel_active(n)=.true.
  
  end subroutine
  !------------------------------------------------------
  
  
  
  !------------------------------------------------------
  subroutine startLogger(this)
    class(Logger_),intent(inout) :: this
    integer(int32) :: i,n
    type(IO_) :: f
    
    n=0
    this%counter=0
    do i=1,size(this%channel_active)
      if(this%channel_active(i) )then
        call f%open(trim(this%channel_name(i)%all )//".txt","w") 
        call f%close()
        n = n + 1
      endif
      if(n==this%numchannel() ) return
    enddo
  
  end subroutine
  !------------------------------------------------------
  
  
  !------------------------------------------------------
  subroutine saveLogger(this,t)
    class(Logger_),intent(inout) :: this
    integer(int32) :: i,n
    real(real64),optional,intent(in) :: t
    type(IO_) :: f
    
    n=0
    this%counter=this%counter+1
    do i=1,size(this%channel_active)
      if(this%channel_active(i) )then
        call f%open(trim(this%channel_name(i)%all )//".txt","a")
        if(present(t) )then
            call f%write(t, this%channel_value(i)%ptr )
        else
            call f%write(this%counter,this%channel_value(i)%ptr )
        endif
        
        call f%close()
        n = n + 1
      endif
      if(n==this%numchannel() ) return
    enddo
  
  
  end subroutine
  !------------------------------------------------------
  
  
  
  !------------------------------------------------------
  subroutine resetLogger(this)
    class(Logger_),intent(inout) :: this
    integer(int32) :: i,n
    type(IO_) :: f
    
    n=0
    this%counter = 0
    do i=1,size(this%channel_active)
      if(this%channel_active(i) )then
        call f%open(trim(this%channel_name(i)%all )//".txt","w") 
        call f%close("delete")
        n = n + 1
      endif
      if(n==this%numchannel() ) return
    enddo
  
  end subroutine
  !------------------------------------------------------
  
  
  end module LoggerClass
  
  