
module RangeClass
    use MathClass
    use ArrayClass
    implicit none
    
    real(real64),parameter :: PF_RANGE_INFTY = dble(1.0e+14)
    
    type :: Range_
        real(real64) :: x_range(1:2)=[-PF_RANGE_INFTY , PF_RANGE_INFTY]
        real(real64) :: y_range(1:2)=[-PF_RANGE_INFTY , PF_RANGE_INFTY]
        real(real64) :: z_range(1:2)=[-PF_RANGE_INFTY , PF_RANGE_INFTY]
        real(real64) :: t_range(1:2)=[-PF_RANGE_INFTY , PF_RANGE_INFTY]
    contains 
        procedure :: init => initRange
        procedure :: set => setRange
        procedure :: get => getRange
        
        procedure :: set_x    =>    set_xRange
        procedure :: set_y    =>    set_yRange
        procedure :: set_z    =>    set_zRange
        procedure :: set_t    =>    set_tRange

        procedure :: inside   =>    insideRange
    end type
contains

! #########################################################
subroutine initRange(this,MaxRange) 
    class(Range_),intent(inout) :: this
    real(real64),optional,intent(in) :: MaxRange

    if(present(MaxRange) )then
        this%x_range=[-MaxRange , MaxRange]
        this%y_range=[-MaxRange , MaxRange]
        this%z_range=[-MaxRange , MaxRange]
        this%t_range=[-MaxRange , MaxRange]
    else
        this%x_range=[-PF_RANGE_INFTY , PF_RANGE_INFTY]
        this%y_range=[-PF_RANGE_INFTY , PF_RANGE_INFTY]
        this%z_range=[-PF_RANGE_INFTY , PF_RANGE_INFTY]
        this%t_range=[-PF_RANGE_INFTY , PF_RANGE_INFTY]
    endif

end subroutine
! #########################################################

subroutine setRange(this,x_min,x_max,y_min,y_max,z_min,z_max)
    class(Range_),intent(inout) :: this
    real(real64),optional,intent(in) :: x_min, x_max
    real(real64),optional,intent(in) :: y_min, y_max
    real(real64),optional,intent(in) :: z_min, z_max

    call this%set_x(x_min=x_min,x_max=x_max)
    call this%set_y(y_min=y_min,y_max=y_max)
    call this%set_z(z_min=z_min,z_max=z_max)

end subroutine

! #########################################################
subroutine set_xRange(this,x_min,x_max)
    class(Range_),intent(inout) :: this
    real(real64),optional,intent(in) :: x_min, x_max

    if(present(x_min) )then 
        this%x_range(1) = x_min
    endif

    if(present(x_max) )then 
        this%x_range(2) = x_max
    endif

end subroutine
! #########################################################


! #########################################################
subroutine set_yRange(this,y_min,y_max)
    class(Range_),intent(inout) :: this
    real(real64),optional,intent(in) :: y_min, y_max

    if(present(y_min) )then 
        this%y_range(1) = y_min
    endif

    if(present(y_max) )then 
        this%y_range(2) = y_max
    endif

end subroutine
! #########################################################


! #########################################################
subroutine set_zRange(this,z_min,z_max)
    class(Range_),intent(inout) :: this
    real(real64),optional,intent(in) :: z_min, z_max

    if(present(z_min) )then 
        this%z_range(1) = z_min
    endif

    if(present(z_max) )then 
        this%z_range(2) = z_max
    endif

end subroutine
! #########################################################


! #########################################################
subroutine set_tRange(this,t_min,t_max)
    class(Range_),intent(inout) :: this
    real(real64),optional,intent(in) :: t_min, t_max

    if(present(t_min) )then 
        this%t_range(1) = t_min
    endif

    if(present(t_max) )then 
        this%t_range(2) = t_max
    endif

end subroutine 
! #########################################################

! #########################################################
function getRange(this,range_type)  result(min_and_max)
    class(Range_),intent(inout) :: this
    character(1),intent(in) :: range_type
    real(real64) :: min_and_max(2)

    if(range_type=="x" .or. range_type=="X")then
        min_and_max=this%x_range
    endif


    if(range_type=="y" .or. range_type=="Y")then
        min_and_max=this%y_range
    endif


    if(range_type=="z" .or. range_type=="Z")then
        min_and_max=this%z_range
    endif


    if(range_type=="t" .or. range_type=="T")then
        min_and_max=this%t_range
    endif


end function
! #########################################################

pure function insideRange(this,point) result(inside_is_true)
    class(Range_),intent(in) :: this
    real(real64),intent(in) :: point(:)
    integer(int32) :: i
    logical :: inside_is_true

    inside_is_true = .true.
    do i=1,size(point)
        if(i==1)then
            if( this%x_range(1) > point(i) .or. this%x_range(2) < point(i))then
                inside_is_true = .false.
                return
            endif
        elseif(i==2)then
            if( this%y_range(1) > point(i) .or. this%y_range(2) < point(i))then
                inside_is_true = .false.
                return
            endif
        elseif(i==3)then
            if( this%z_range(1) > point(i) .or. this%z_range(2) < point(i))then
                inside_is_true = .false.
                return
            endif
        elseif(i==4)then
            if( this%t_range(1) > point(i) .or. this%t_range(2) < point(i))then
                inside_is_true = .false.
                return
            endif
        endif
    enddo 

end function


end module RangeClass