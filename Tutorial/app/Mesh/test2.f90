program main
    use iso_fortran_env
    implicit none
    integer(int32) :: x,y
    integer(int32),allocatable :: c(:)
    x=2
    y=3
    if(allocated(c))then
      deallocate(c)
    end if
    c=hoge(a=x,b=y)
    write(output_unit,*) "#1回目---"
    write(output_unit,*) "x=",x
    write(output_unit,*) "y=",y
    write(output_unit,*) "c=",c
    if(allocated(c))then
      deallocate(c)
    end if
    c=hoge(a=y,b=x)
    write(output_unit,*) "#2回目---"
    write(output_unit,*) "x=",x
    write(output_unit,*) "y=",y
    write(output_unit,*) "c=",c
    contains
      function hoge(a,b) result (c)
        integer(int32),intent(in) :: a,b
        integer(int32),allocatable :: c(:)
        allocate(c(b))
        c(:)=a
      end function hoge
  end program