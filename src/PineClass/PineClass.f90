module PineClass
    !use LeafClass
    use ArrayClass
    use IOClass
    use StemClass
    implicit none
    
    type :: Pine_
        type(Stem_),allocatable :: stem(:) 

        real(real64) :: mainstem_length
        real(real64),allocatable :: mainstem_diameters(:)
        real(real64),allocatable :: mainstem_diameters_h(:)
        integer(int32) :: divisions(1:2)

    contains
        procedure,public :: init => initPine
    end type

contains

subroutine initPine(this,config)
    class(Pine_),intent(inout) :: this
    character(*),intent(in) :: config
    type(IO_) :: f
    integer(int32) :: r_num,l_num
    real(real64) :: z,r_max,resize_ratio
    real(real64),allocatable :: x(:)
    integer(int32) :: i

    this%mainstem_length = freal(f%parse_json(config,to_list("Mainstem","Length")))
    this%mainstem_diameters = &
        f%parse_json(config,to_list("Mainstem","Diameters")) .as. real64_vector()
    this%mainstem_diameters_h = &
        f%parse_json(config,to_list("Mainstem","Diameter_heights")) .as. real64_vector()
    this%divisions = &
        f%parse_json(config,to_list("Mainstem","Divisions")) .as. int32_vector()

    if(allocated(this%stem) ) then
        deallocate(this%stem)
    endif

    allocate(this%stem(1) )
    r_num = this%divisions(1)
    l_num = this%divisions(2)
    print *, l_num
    call this%stem(1)%femdomain%to_cylinder(x_num=r_num,y_num=r_num,z_num=l_num)
    call this%stem(1)%femdomain%resize(&
        x=1.0d0,&
        y=1.0d0,&
        z=this%mainstem_length)

    do i=1,this%stem(1)%femdomain%nn() 
        x = this%stem(1)%femdomain%mesh%nodcoord(i,:) 
        z = x(3)
        r_max = 0.50d0* &
            linear_interpolate_pine(x=this%mainstem_diameters_h,f=this%mainstem_diameters,x0=z)
        resize_ratio = r_max
        this%stem(1)%femdomain%mesh%nodcoord(i,1:2) &
            = resize_ratio*this%stem(1)%femdomain%mesh%nodcoord(i,1:2)
    enddo
    
end subroutine

function linear_interpolate_pine(x,f,x0) result(ret)
    real(real64),intent(in) :: x(:),f(:),x0
    real(real64) :: ret,theta
    integer(int32) :: i

    if(x0 < x(1))then
        ret = f(1)
        return
    endif


    if(x0 > x(size(x)))then
        ret = f(size(f))
        return
    endif

    do i=1,size(x)-1
        if(x(i) <= x0 .and. x0 <= x(i+1))then
            theta = (x0-x(i))/(x(i+1)-x(i))
            ! theta = 0 => ret = f(i)
            ! theta = 1 => ret = f(i+1)
            ret = (1.0d0-theta)*f(i) + theta*f(i+1)
            return
        endif
    enddo

end function

end module PineClass