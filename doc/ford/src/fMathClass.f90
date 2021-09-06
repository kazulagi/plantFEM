module fMathClass
  use iso_c_binding

  implicit none

  !type(c_ptr) :: pa
  !integer(c_int)::n

  interface
    subroutine testc(pa) bind(c)
      use iso_c_binding
      type(c_ptr):: pa
    end subroutine testc
  end interface


  interface
    subroutine addValVec(pa,n,i,val_in) bind(c)
      use iso_c_binding
      type(c_ptr):: pa
      integer(c_int),value:: i
      integer(c_int),value:: n
      real(c_double),value::val_in
  
    end subroutine addValVec
  end interface

  interface
    subroutine putValVec(pa,n,i,val_in) bind(c)
      use iso_c_binding
      type(c_ptr):: pa
      integer(c_int),value:: i
      integer(c_int),value:: n
      real(c_double),value::val_in
  
    end subroutine putValVec
  end interface

  interface
    subroutine setZeroVec(pa,n) bind(c)
      use iso_c_binding
      type(c_ptr):: pa
      integer(c_int),value:: n
  
    end subroutine setZeroVec
  end interface

  interface
    subroutine initVec(pa,n) bind(c)
      use iso_c_binding
      type(c_ptr):: pa
      integer(c_int),value:: n
  
    end subroutine initVec
  end interface

  interface
    subroutine c_allocatev(pa,n) bind(c)
      use iso_c_binding
      type(c_ptr):: pa
      integer(c_int),value:: n
  
    end subroutine c_allocatev
  end interface

  interface
    function dotproduct(pa,pa2,n) result(val) bind(c)
      use iso_c_binding
      type(c_ptr):: pa,pa2
      integer(c_int),value:: n
      real(c_double):: val
    end function dotproduct
  end interface
  

  interface
    function opencl_dotproduct(pa,pa2,n) result(val) bind(c)
      use iso_c_binding
      type(c_ptr):: pa,pa2
      integer(c_int),value:: n
      real(c_double):: val
    end function opencl_dotproduct
  end interface

  interface
    function opencl_dotproduct_f(pa,pa2,n) result(val) bind(c)
      use iso_c_binding
      type(c_ptr):: pa,pa2
      integer(c_int),value:: n
      real(c_float):: val
    end function opencl_dotproduct_f
  end interface

  
contains

  subroutine showValue()
    type(c_ptr) :: pa
    real(c_double),pointer::fpa(:)
    real(c_double)::val
    double precision, allocatable,target :: vec(:),vec2(:)
    integer(c_int)::n
    integer :: i
  

    n=10000
    allocate(vec(n) )
    
    do i=1,n
      val=dble(i)
      call putValVec(pa,n,i,val)
    enddo

    call c_f_pointer(pa, fpa, [n])
    
    vec(:)= fpa(:)

    print *, vec(:)
  end subroutine


  function c_dot_product(a,b,nf) result(dp)
    use iso_c_binding
    integer,intent(in)::nf
    real(8),intent(in),target::a(nf),b(nf)
    real(4),target::a_f(nf),b_f(nf)
    double precision, pointer ::fpa(:),fpb(:)
    real(4), pointer ::fpa_f(:),fpb_f(:)
    
    real(8) :: dp
    real(4) :: dp_f

    type(c_ptr) :: pa
    type(c_ptr) :: pa2
    
    type(c_ptr) :: pa_f
    type(c_ptr) :: pa2_f

    real(c_double)::val
    real(c_float)::val_f
    integer(c_int)::n
    integer :: i
  
    do i=1,nf
      a_f(i)=real(a(i) )
      b_f(i)=real(b(i) )
    enddo

    fpa => a
    fpb => b

    
    n=nf
    !call c_f_pointer(fpa, a, [n])
    !call c_f_pointer(fpb, b, [n])
    call c_allocatev(pa,n)
    call c_f_pointer(pa, fpa, [n])
    fpa(:)=a(:)
    call c_allocatev(pa2,n)
    call c_f_pointer(pa2, fpb, [n])
    fpb(:)=b(:)

    dp_f= opencl_dotproduct(pa,pa2,n)
    print *, dp


  

  end function c_dot_product






  function c_dot_product_f(a,b,nf) result(dp)
    use iso_c_binding
    integer,intent(in)::nf
    real(8),intent(in),target::a(nf),b(nf)
    real(4),target::a_f(nf),b_f(nf)
    double precision, pointer ::fpa(:),fpb(:)
    real(4), pointer ::fpa_f(:),fpb_f(:)
    
    real(8) :: dp
    real(4) :: dp_f

    type(c_ptr) :: pa
    type(c_ptr) :: pa2
    
    type(c_ptr) :: pa_f
    type(c_ptr) :: pa2_f

    real(c_double)::val
    real(c_float)::val_f
    integer(c_int)::n
    integer :: i
  
    do i=1,nf
      a_f(i)=real(a(i) )
      b_f(i)=real(b(i) )
    enddo

    !fpa => a
    !fpb => b

    
    n=nf
    !call c_f_pointer(fpa, a, [n])
    !call c_f_pointer(fpb, b, [n])
    !call c_allocatev(pa,n)
    !call c_f_pointer(pa, fpa, [n])
    !fpa(:)=a(:)
    !call c_allocatev(pa2,n)
    !call c_f_pointer(pa2, fpb, [n])
    !fpb(:)=b(:)
    

    ! pointer
    fpa_f => a_f
    fpb_f => b_f

    call c_allocatev(pa_f,n)
    call c_f_pointer(pa_f, fpa_f, [n])
    fpa_f(:)=a_f(:)
    call c_allocatev(pa2_f,n)
    call c_f_pointer(pa2_f, fpb_f, [n])
    fpb_f(:)=b_f(:)

    dp_f= opencl_dotproduct_f(pa_f,pa2_f,n)
    print *, dp


  

  end function c_dot_product_f


end module fMathClass

