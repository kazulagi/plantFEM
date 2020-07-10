module LinearSolverClass
  use, intrinsic :: iso_fortran_env
  use MathClass
  use MPIClass
  implicit none

  type :: LinearSolver_
    ! non-Element-by-element
    real(real64),allocatable :: a(:,:)
    real(real64),allocatable :: b(:)
    real(real64),allocatable :: x(:)
    ! Element-by-element
    real(real64),allocatable :: a_e(:,:,:)
    real(real64),allocatable :: b_e(:,:)
    real(real64),allocatable :: x_e(:,:)

    real(real64),allocatable :: val(:)
    integer(int32),allocatable :: index_I(:)
    integer(int32),allocatable :: index_J(:)

    integer(int32),allocatable :: connectivity(:,:)
    integer(int32) :: itrmax=1000000
    integer(int32) :: currentID=1
    real(real64) :: er0=dble(1.0e-08)
  contains
    procedure, public :: set => setLinearSolver
    procedure, public :: import => importLinearSolver
    procedure, public :: fix => fixLinearSolver
    procedure, public :: solve => solveLinearSolver
  end type
contains

!====================================================================================
subroutine fixLinearSolver(obj,nodeid,entryvalue)
  class(LinearSolver_),intent(inout) :: obj
  integer(int32),intent(in) :: nodeid
  real(real64),intent(in) :: entryvalue
  integer(int32) :: i,j

  ! only for CRS-format
  if(.not. allocated(obj%val) .or. .not.allocated(obj%b))then
    print *, "ERROR >> fixLinearSolver .not. allocated(val) "
    stop
  endif

  do i=1,size(obj%val)

    if(obj%index_J(i)==nodeid)then
      obj%b(obj%index_I(i) ) = obj%b(obj%index_I(i) )- obj%val(i) * entryvalue
      obj%val(i)=0.0d0
    endif

  enddo

  do i=1,size(obj%index_I)
    
    if(obj%index_I(i)==nodeid)then
      if(obj%index_J(i) ==nodeid)then
        obj%val(i)=1.0d0
      else
        obj%val(i)=0.0d0
      endif
      obj%b(obj%index_I(i) ) = entryvalue
    endif
    
    if(obj%index_I(i)==nodeid)then
      if(obj%index_J(i)==nodeid)then
        obj%val(i)=1.0d0
      endif
    endif
  enddo
  
  


end subroutine
!====================================================================================


!====================================================================================
subroutine setLinearSolver(obj,low,column,entryvalue,init)
  class(LinearSolver_),intent(inout) :: obj
  integer(int32),optional,intent(in) :: low, column
  real(real64),optional,intent(in) :: entryvalue
  logical,optional,intent(in) :: init
  integer(int32) :: i

  if(present(init) )then
    if(init .eqv. .true.)then
      if(allocated(obj%val) )then
        obj%val(:)=0.0d0
      endif
      if(allocated(obj%b) )then
        obj%b(:)=0.0d0
      endif
      obj%currentID=1
    endif
  endif

  if(present(low) .and. present(column))then
    if(.not. allocated(obj%val) )then
      allocate(obj%val(1) )
      obj%val(1)=input(default=0.0d0, option=entryvalue)
    endif
    if(.not. allocated(obj%index_I) )then
      allocate(obj%index_I(1) )
      obj%index_I(1)=low
    endif
    if(.not. allocated(obj%index_J) )then
      allocate(obj%index_J(1) )
      obj%index_J(1)=column
      return
    endif
    ! if already exists, add.
    do i=1,size(obj%index_I)
      if(obj%index_I(i) == low )then
        if(obj%index_J(i) == column )then
          obj%val(i) = obj%val(i) + entryvalue
          return
        endif
      endif
    enddo

    if(obj%currentID < size(obj%val) )then
      obj%val(obj%currentID)=entryvalue
      obj%index_I(obj%currentID)=low
      obj%index_J(obj%currentID)=column
    else
      call  extendArray(obj%val,entryvalue)
      call  extendArray(obj%index_I,low)
      call  extendArray(obj%index_J,column)
    endif
    obj%currentID=obj%currentID+1
  elseif(present(low) )then
    if(.not. allocated(obj%b) )then
      allocate(obj%b(1) )
      obj%b(1)=input(default=0.0d0, option=entryvalue)
      return
    else
      if(low > size(obj%b) )then
        if(obj%currentID < size(obj%val) )then
          obj%b(low)=entryvalue
        else
          call extendArray(obj%b,0.0d0,low-size(obj%b) )
          obj%b(low)=obj%b(low)+entryvalue
        endif
      endif
    endif
  else
    return
  endif

end subroutine
!====================================================================================

!====================================================================================
subroutine importLinearSolver(obj,a,x,b,a_e,b_e,x_e,connectivity,val,index_I,index_J)
  class(LinearSolver_),intent(inout) :: obj
  real(8),optional,intent(in) :: a(:,:),b(:),x(:),a_e(:,:,:),b_e(:,:),x_e(:,:)
  real(8),optional,intent(in) :: val(:)
  integer(int32),optional,intent(in) :: index_I(:),index_J(:)
  integer(int32),optional,intent(in) :: connectivity(:,:)
  integer(int32) :: k,l,m

  if(present(val) )then
    if(.not. allocated(obj%val) )then
      allocate(obj%val(size(val) ))
      obj%val=val
    endif
  endif
  
  if(present(index_i) )then
    if(.not. allocated(obj%index_i) )then
      allocate(obj%index_i(size(index_i) ))
      obj%index_i=index_i
    endif
  endif

  
  if(present(index_j) )then
    if(.not. allocated(obj%index_j) )then
      allocate(obj%index_j(size(index_j) ))
      obj%index_j=index_j
    endif
  endif
  
  ! in case of non element-by-element
  if(present(a) )then
    ! Set Ax=b
    k=size(a,1)
    l=size(a,2)
    if(.not. allocated(obj%a) )then
      allocate(obj%a(k,l) )
    elseif(size(obj%a,1)/=k .or. size(obj%a,2)/=l )then
      deallocate(obj%a)
      allocate(obj%a(k,l) )
    endif
    obj%a(:,:)=a(:,:)
  endif

  if(present(b) )then
    k=size(b,1)
    if(.not. allocated(obj%b) )then
      allocate(obj%b(k) )
    elseif(size(obj%b,1)/=k)then
      deallocate(obj%b)
      allocate(obj%b(k) )
    endif
    obj%b(:)=b(:)
  endif

  if(present(x) )then
    k=size(x,1)
    if(.not. allocated(obj%x) )then
      allocate(obj%x(k) )
    elseif(size(obj%x,1)/=k)then
      deallocate(obj%x)
      allocate(obj%x(k) )
    endif
    obj%x(:)=x(:)
  endif

  if(present(a_e) )then
    ! Set A_e x_e =b_e
    k=size(a_e,1)
    l=size(a_e,2)
    m=size(a_e,3)
    if(.not. allocated(obj%a_e) )then
      allocate(obj%a_e(k,l,m) )
    endif
    obj%a_e(:,:,:)=a_e(:,:,:)
  endif

  if(present(b_e) )then
    ! Set Ax=b
    k=size(b_e,1)
    l=size(b_e,2)
    if(.not. allocated(obj%b_e) )then
      allocate(obj%b_e(k,l) )
    elseif(size(obj%b_e,1)/=k .or. size(obj%b_e,2)/=l )then
      deallocate(obj%b_e)
      allocate(obj%b_e(k,l) )
    endif
    obj%b_e(:,:)=b_e(:,:)
  endif

  if(present(x_e) )then
    ! Set Ax=b
    k=size(x_e,1)
    l=size(x_e,2)
    if(.not. allocated(obj%x_e) )then
      allocate(obj%x_e(k,l) )
    elseif(size(obj%x_e,1)/=k .or. size(obj%x_e,2)/=l )then
      deallocate(obj%x_e)
      allocate(obj%x_e(k,l) )
    endif
    obj%x_e(:,:)=x_e(:,:)
  endif

end subroutine importLinearSolver
!====================================================================================


!====================================================================================
subroutine solveLinearSolver(obj,Solver,MPI,OpenCL,CUDAC,preconditioning,CRS)
  class(LinearSolver_),intent(inout) :: obj
  character(*),intent(in) :: Solver
  logical,optional,intent(in) :: MPI, OpenCL, CUDAC,preconditioning,CRS

  if(.not. allocated(obj%a) .and. .not. allocated(obj%val) )then
    print *, "solveLinearSolver >> ERROR :: .not. allocated(obj%b) "
    stop
  endif


  if(.not. allocated(obj%b) )then
    print *, "solveLinearSolver >> ERROR :: .not. allocated(obj%b) "
    stop
  endif


  if(.not. allocated(obj%x) )then
    allocate(obj%x( size(obj%b) ) )
    obj%x(:)=0.0d0
  endif

  
  if(size(obj%x) /=size(obj%b) )then
    deallocate(obj%x)
    allocate(obj%x( size(obj%b) ) )
    obj%x(:)=0.0d0
  endif
  
  ! No MPI, No OpenCl and No CUDAC
  if(allocated(obj%a) )then
    if(allocated(obj%b) )then
      if(allocated(obj%x))then
        ! run as non EBE-mode
        if(trim(Solver) == "GaussSeidel" )then
          call gauss_seidel(obj%a, obj%b, obj%x, size(obj%a,1), obj%itrmax, obj%er0 )
        elseif(trim(Solver) == "GaussJordanPV" .or. trim(Solver) == "GaussJordan" )then
          call gauss_jordan_pv(obj%a, obj%x, obj%b, size(obj%a,1) )
        elseif(trim(Solver) == "BiCGSTAB" )then
          if(present(CRS) )then
            if(CRS .eqv. .true.)then
              call bicgstab_CRS(obj%val, obj%index_I, obj%index_J, obj%x, obj%b, obj%itrmax, obj%er0)
            else
              call bicgstab1d(obj%a, obj%b, obj%x, size(obj%a,1), obj%itrmax, obj%er0)  
            endif
          else
            call bicgstab1d(obj%a, obj%b, obj%x, size(obj%a,1), obj%itrmax, obj%er0)
          endif
        elseif(trim(Solver) == "GPBiCG" )then
          if(present(preconditioning) )then
            if(preconditioning .eqv. .true.)then
              call preconditioned_GPBiCG(obj%a, obj%b, obj%x, size(obj%a,1), obj%itrmax, obj%er0)
            else
              call GPBiCG(obj%a, obj%b, obj%x, size(obj%a,1), obj%itrmax, obj%er0)
            endif
          else
            call GPBiCG(obj%a, obj%b, obj%x, size(obj%a,1), obj%itrmax, obj%er0)
          endif
          

        else
          print *, "LinearSolver_ ERROR:: no such solver as :: ",trim(Solver)
        endif
        return
      endif
    endif
  else
    call bicgstab_CRS(obj%val, obj%index_I, obj%index_J, obj%x, obj%b, obj%itrmax, obj%er0)
    return
  endif

  print *, "LinearSolver_ ERROR:: EBE-mode is not implemented yet."
  stop 
end subroutine solveLinearSolver
!====================================================================================


!====================================================================================
subroutine gauss_seidel(a, b, x, n, itrmax, er0)
  integer(int32), intent(in) :: n, itrmax
  real(real64), intent(in)  :: a(n, n), b(n), er0
  real(real64), intent(out) :: x(n)
  real(real64) s, er, rd(n), r(n)
  integer(int32) i, itr
  do i = 1, n
    if (a(i, i) == 0.0d0)  stop  'a(i, i) == 0.0d0'
    rd(i) = 1.0d0 / a(i, i)
  enddo
  x(1:n) = 0.0d0
  do itr = 1, itrmax
    do i = 1,n
      s = dot_product(a(i, 1 :i-1), x(1: i-1))
      s = s + dot_product(a(i, i + 1:n), x(i+1:n))
      x(i) = rd(i) * (b(i) - s)
    enddo
    r(1:n) = b(1:n) - matmul(a,x)
    er = dot_product(r, r)
    if(er <= er0) then
      write(20,*) '# converged#'
      exit
    endif
  enddo
 end subroutine gauss_seidel
!===================================================================================
subroutine gauss_jordan_pv(a0, x, b, n)
  integer(int32), intent(in) :: n
  real(real64), intent(in) :: a0(n,n), b(n)
  real(real64), intent(out) :: x(n)
  integer(int32) i, j, k, m,nn, mm
  real(real64) ar, am, t, a(n,n), w(n)
  nn = size(a0,1)

  a(:,:)= a0(:,:)
  x(:) = b(:)
  do k = 1, n
     m = k
     am = abs(a(k,k))
     do i = k+1, n
        if (abs(a(i,k)) > am) then
          am = abs(a(i,k))
          m = i
        endif
     enddo
     if (am == 0.0d0)   stop  ' A is singular '
     if ( k /= m) then
       w(k:n) = a(k, k:n)
       a(k,k:n) = a(m, k:n)
       a(m, k:n) =w(k:n)
       t = x(k)
       x(k) = x(m)
       x(m) = t
     endif
     ! �ȉ��A�ʏ��gauss_jordan
     if (a(k, k) == 0.0d0)  stop  'devide by zero3gauss_jordan_pv'
	 ar = 1.0d0 / a(k,k)
     a(k,k) = 1.0d0
     a(k,k+1:n) = ar * a(k, k+1:n)
     x(k) = ar * x(k)
     do i= 1, n
       if (i /= k) then
         a(i, k+1:n) = a(i, K+1:n) - a(i,k) * a(k, k+1:n)
         x(i) = x(i) - a(i,k) * x(k)
         a(i,k) = 0.0d0
       endif
     enddo
  enddo
  
 end subroutine gauss_jordan_pv
!===========================================================================

 subroutine bicgstab_diffusion(a, b, x, n, itrmax, er,DBC,DBCVal)
  integer(int32), intent(in) :: n, itrmax,DBC(:,:)
  real(real64), intent(in) :: a(n,n), b(n), er,DBCVal(:,:)
  real(real64), intent(inout) :: x(n)
     integer(int32) itr,i,j
     real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(real64) r(n), r0(n), p(n), y(n), e(n), v(n)
     er0=1.00e-8
   r(:) = b - matmul(a,x)
   ! if DBC => reset residual
   do i=1,size(DBC,1)
    do j=1,size(DBC,2)
      if(DBC(i,j)<1 )then
        cycle
      else
        r( DBC(i,j) )=0.0d0
        x(DBC(i,j)  )=DBCVal(i,j)
      endif
    enddo
   enddo
   
   !
   c1 = dot_product(r,r)
	 init_rr=c1
     if (c1 ==0.0d0) then
      print *, "Caution :: Initial residual is zero"
      return
     endif
     p(:) = r(:)
     r0(:) = r(:)
     do itr = 1, itrmax
        !c1 = dot_product(r0,r)
        y(:) = matmul(a,p)
        c2 = dot_product(r0,y)
        alp = c1/c2
        e(:) = r(:) - alp * y(:)
        v(:) = matmul(a,e)
        ev = dot_product(e,v)
        vv = dot_product(v,v)
        if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
		    c3 = ev / vv
        x(:) = x(:) + alp * p(:) + c3 * e(:)
        r(:) = e(:) - c3 * v(:)
        
        ! if DBC => reset residual
        do i=1,size(DBC,1)
         do j=1,size(DBC,2)
           if(DBC(i,j)<1 )then
             cycle
           else
             r( DBC(i,j) )=0.0d0
             x(DBC(i,j)  )=DBCVal(i,j)
           endif
         enddo
        enddo
        rr = dot_product(r,r)
    !    write(*,*) 'itr, er =', itr,rr
        if (rr/init_rr < er0) then
          
          print *, "[ok] :: BICGSTAB is converged in ",i," steps."
          exit
        endif

        c1 = dot_product(r0,r)
        bet = c1 / (c2 * c3)
		if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
		
        p(:) = r(:) + bet * (p(:) -c3*y(:) )
        if(itrmax==itr)then
          print *, "ERROR :: BICGSTAB did not converge"
          return
        endif
     enddo

 end subroutine bicgstab_diffusion
!===============================================================

subroutine bicgstab_CRS(a, index_i, index_j, x, b, itrmax, er)
  integer(int32), intent(inout) :: index_i(:),index_j(:), itrmax
  real(real64), intent(inout) :: a(:), b(:), er
  real(real64), intent(inout) :: x(:)
  integer(int32) itr,i,j,n
  real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
  real(real64),allocatable:: r(:), r0(:), p(:), y(:), e(:), v(:)

  n=size(b)
  allocate(r(n), r0(n), p(n), y(n), e(n), v(n))
  er0=dble(1.00e-14)
  r(:) = b(:)
  do i=1,size(a)
    r( index_i(i) ) = r( index_i(i) ) - a(i)*x( index_j(i) ) 
  enddo

  !r(:) = b - matmul(a,x)
  
  c1 = dot_product(r,r)
	init_rr=c1
  if (c1 < er0) return
  p(:) = r(:)
  r0(:) = r(:)
  do itr = 1, itrmax   
    c1 = dot_product(r0,r)
    
    !y(:) = matmul(a,p)
    y(:)=0.0d0
    do i=1,size(a)
      y( index_i(i) ) = y( index_i(i) ) + a(i)*p( index_j(i) ) 
    enddo

    c2 = dot_product(r0,y)
    alp = c1/c2
    e(:) = r(:) - alp * y(:)
    !v(:) = matmul(a,e)
    v(:)=0.0d0
    do i=1,size(a)
      v( index_i(i) ) = v( index_i(i) ) + a(i)*e( index_j(i) ) 
    enddo
    ev = dot_product(e,v)
    vv = dot_product(v,v)
    if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
		c3 = ev / vv
    x(:) = x(:) + alp * p(:) + c3 * e(:)
    r(:) = e(:) - c3 * v(:)
    rr = dot_product(r,r)
    
    
    !    write(*,*) 'itr, er =', itr,rr
    if (rr/init_rr < er0) exit
    c1 = dot_product(r0,r)
    bet = c1 / (c2 * c3)
		if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
    p(:) = r(:) + bet * (p(:) -c3*y(:) )
  enddo
 end subroutine 
!===============================================================

!===============================================================

subroutine bicgstab1d(a, b, x, n, itrmax, er)
  integer(int32), intent(in) :: n, itrmax
  real(real64), intent(in) :: a(n,n), b(n), er
  real(real64), intent(inout) :: x(n)
     integer(int32) itr
     real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(real64) r(n), r0(n), p(n), y(n), e(n), v(n)
     er0=dble(1.00e-14)

	 r(:) = b - matmul(a,x)
     c1 = dot_product(r,r)
	 init_rr=c1
     if (c1 < er0) return
     p(:) = r(:)
     r0(:) = r(:)
     do itr = 1, itrmax
        c1 = dot_product(r0,r)


        y(:) = matmul(a,p)

        c2 = dot_product(r0,y)
        alp = c1/c2
        e(:) = r(:) - alp * y(:)
        v(:) = matmul(a,e)
        ev = dot_product(e,v)
        vv = dot_product(v,v)
        if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
	    	c3 = ev / vv
        x(:) = x(:) + alp * p(:) + c3 * e(:)
        r(:) = e(:) - c3 * v(:)
        rr = dot_product(r,r)

        
    
    
    !    write(*,*) 'itr, er =', itr,rr
        if (rr/init_rr < er0) exit
        c1 = dot_product(r0,r)
        bet = c1 / (c2 * c3)
		if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
		
        p(:) = r(:) + bet * (p(:) -c3*y(:) )
     enddo
 end subroutine bicgstab1d
!===============================================================

subroutine bicgstab_nr(a, b, x, n, itrmax, er,u_nod_x, u_nod_y)
  integer(int32), intent(in) :: n, itrmax,u_nod_x(:),u_nod_y(:)
  real(real64), intent(in) :: a(n,n),b(n), er
  real(real64), intent(inout) :: x(n)
  integer(int32) itr
     real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(real64) r(n), r0(n), p(n), y(n), e(n), v(n)
     er0=1.00e-14
	 
	 r(:) = b - matmul(a,x)
	 
	 call modify_residual(r, u_nod_x, u_nod_y)
     
	 c1 = dot_product(r,r)
	 
	 init_rr=c1
     
	 if (c1 < er0) return
     
	 p(:) = r(:)
     
	 r0(:) = r(:)
     
	 do itr = 1, itrmax
        y(:) = matmul(a,p)
        c2 = dot_product(r0,y)
        alp = c1/c2
        e(:) = r(:) - alp * y(:)
        v(:) = matmul(a,e)
        ev = dot_product(e,v)
        vv = dot_product(v,v)
        if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
		c3 = ev / vv
        x(:) = x(:) + alp * p(:) + c3 * e(:)
        r(:) = e(:) - c3 * v(:)
		call modify_residual(r, u_nod_x, u_nod_y)
        rr = dot_product(r,r)
    !    write(*,*) 'itr, er =', itr,rr
        if (rr/init_rr < er0) exit
        c1 = dot_product(r0,r)
        bet = c1 / (c2 * c3)
		if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
		
        p(:) = r(:) + bet * (p(:) -c3*y(:) )
     enddo
 end subroutine bicgstab_nr
!====================================================================================
subroutine bicgstab_nr1(a, b, x, n, itrmax, er,u_nod_x, u_nod_y,u_nod_dis_x,u_nod_dis_y)
  integer(int32), intent(in) :: n, itrmax,u_nod_x(:),u_nod_y(:)
  real(real64), intent(in) :: a(n,n),b(n), er,u_nod_dis_x(:),u_nod_dis_y(:)
  real(real64), intent(inout) :: x(n)
     integer(int32) itr
     real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(real64) r(n), r0(n), p(n), y(n), e(n), v(n)
     er0=1.00e-14
	 r(:) = b - matmul(a,x)
	 call modify_residual_1(r,x, u_nod_x, u_nod_y,u_nod_dis_x,u_nod_dis_y)
     c1 = dot_product(r,r)
	 init_rr=c1
     if (c1 < er0) return
     p(:) = r(:)
     r0(:) = r(:)
     do itr = 1, itrmax
        y(:) = matmul(a,p)
        c2 = dot_product(r0,y)
        alp = c1/c2
        e(:) = r(:) - alp * y(:)
        v(:) = matmul(a,e)
        ev = dot_product(e,v)
        vv = dot_product(v,v)
        if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
		c3 = ev / vv
        x(:) = x(:) + alp * p(:) + c3 * e(:)
        r(:) = e(:) - c3 * v(:)
		call modify_residual_1(r,x, u_nod_x, u_nod_y,u_nod_dis_x,u_nod_dis_y)
        rr = dot_product(r,r)
    !    write(*,*) 'itr, er =', itr,rr
        if (rr/init_rr < er0) exit
        c1 = dot_product(r0,r)
        bet = c1 / (c2 * c3)
		if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
		
        p(:) = r(:) + bet * (p(:) -c3*y(:) )
     enddo
 end subroutine bicgstab_nr1
!====================================================================================

subroutine bicgstab_dirichlet(a, b, x, n, itrmax, er,DBoundNodID, DBoundVal,SetBC)
  integer(int32), intent(in) :: n, itrmax,DBoundNodID(:,:),SetBC
  real(real64), intent(in) :: a(n,n),b(n), er,DBoundVal(:,:)
  real(real64), intent(inout) :: x(n)
     integer(int32) itr
     real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(real64) r(n), r0(n), p(n), y(n), e(n), v(n)
     er0=1.00e-14
	 
	 r(:) = b - matmul(a,x)
	 
	 call modify_residual_dirichlet(r,x, DBoundNodID, DBoundVal,SetBC)
     
	 c1 = dot_product(r,r)
	 
	 init_rr=c1
     
	 if (c1 < er0) return
     
	 p(:) = r(:)
     
	 r0(:) = r(:)
     
	 do itr = 1, itrmax
        y(:) = matmul(a,p)
        c2 = dot_product(r0,y)
        alp = c1/c2
        e(:) = r(:) - alp * y(:)
        v(:) = matmul(a,e)
        ev = dot_product(e,v)
        vv = dot_product(v,v)
        if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
		c3 = ev / vv
        x(:) = x(:) + alp * p(:) + c3 * e(:)
        r(:) = e(:) - c3 * v(:)
		call modify_residual_dirichlet(r,x, DBoundNodID, DBoundVal,SetBC)
        rr = dot_product(r,r)
    !    write(*,*) 'itr, er =', itr,rr
        if (rr/init_rr < er0) exit
        c1 = dot_product(r0,r)
        bet = c1 / (c2 * c3)
		if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
		
        p(:) = r(:) + bet * (p(:) -c3*y(:) )
     enddo
 end subroutine bicgstab_dirichlet
!====================================================================================


subroutine modify_residual_1(r,x, u_nod_x, u_nod_y,u_nod_dis_x,u_nod_dis_y)
	integer(int32),intent(in)::u_nod_x(:),u_nod_y(:)
	real(real64), intent(in) :: u_nod_dis_x(:),u_nod_dis_y(:)
	real(real64),intent(inout)::r(:),x(:)
	 integer(int32) i
	 
	 do i=1,size(u_nod_x)

		r( 2*u_nod_x(i)-1 )=0.0d0
		x( 2*u_nod_x(i)-1 )=u_nod_dis_x(i)
	 enddo
	 
	 do i=1,size(u_nod_y)

		r( 2*u_nod_y(i) )=0.0d0
		x( 2*u_nod_y(i) )=u_nod_dis_y(i)
	 enddo
	
  end subroutine modify_residual_1
!====================================================================================
subroutine modify_residual(r, u_nod_x, u_nod_y)
	integer(int32),intent(in)::u_nod_x(:),u_nod_y(:)
	real(real64),intent(inout)::r(:)
	 integer(int32) i
	 
	 do i=1,size(u_nod_x)

		r( 2*u_nod_x(i)-1 )=0.0d0
	 enddo
	 
	 do i=1,size(u_nod_y)

		r( 2*u_nod_y(i) )=0.0d0
	 enddo
	
  end subroutine modify_residual
!====================================================================================
subroutine modify_residual_dirichlet(r,x, DBoundNodID, DBoundVal,SetBC)
  integer(int32),intent(in)::DBoundNodID(:,:),SetBC
  real(real64),intent(in)::DBoundVal(:,:)
	real(real64),intent(inout)::r(:),x(:)

  
	integer(int32) :: i,j,k,dim_num,dbc_num
	real(real64) :: val

  if(SetBC==1)then

	  dim_num=size(DBoundNodID,2)
	  dbc_num=size(DBoundNodID,1)
    
	  do i=1,dim_num
	  	do j=1,dbc_num
	  		k=DBoundNodID(j,i)
	  		val=DBoundVal(j,i)
	  		if(k<1)then
	  			cycle
	  		elseif(k>=1)then
          x(dim_num*(k-1)+i)=val*dble(SetBC)
          r(dim_num*(k-1)+i)=0.0d0
	  		else
	  			cycle
	  		endif
	  	enddo
    enddo
  else
    
	  dim_num=size(DBoundNodID,2)
	  dbc_num=size(DBoundNodID,1)
    
	  do i=1,dim_num
	  	do j=1,dbc_num
	  		k=DBoundNodID(j,i)
	  		val=DBoundVal(j,i)
	  		if(k<1)then
	  			cycle
	  		elseif(k>=1)then
          r(dim_num*(k-1)+i)=0.0d0
          x(dim_num*(k-1)+i)=0.0d0
          
	  		else
	  			cycle
	  		endif
	  	enddo
    enddo
  endif
   

end subroutine
!====================================================================================

!===========================================================================
subroutine GPBiCG(a, b, x, n, itrmax, er)
  integer(int32), intent(in) :: n
  integer(int32),optional,intent(in) :: itrmax
  real(real64), intent(in) :: a(n,n), b(n)
  real(real64), optional,intent(in)::er
  real(real64), intent(inout) :: x(n)
     integer(int32) itr,itrmax_
     real(real64) alp,c1, rr,er0,init_rr,beta
     real(real64) gzi,nu,val1,val2,r0rk,eps
     real(real64) r(n), r0(n), p(n), y(n),ap(n),q(n)
     real(real64) u(n),w(n),t(n),t_(n),z(n)
     eps=1.0e-18
     er0=input(default=eps,option=er)

    r(:) = b - matmul(a,x)
    c1 = dot_product(r,r)
    init_rr=c1
    if (c1 < er0) return
    beta=0.0d0
    p(:) = 0.0d0
    r0(:) = r(:)
    w(:) = 0.0d0
    u(:) = 0.0d0
    t(:) = 0.0d0
    t_(:)= 0.0d0
    z(:) = 0.0d0
    itrmax_=input(default=1000,option=itrmax)
    do itr = 1, itrmax_
      p(:) = r(:)+beta*(p(:)-u(:) )  ! triple checked.

      ap(:)   = matmul(a,p) ! triple checked. ap=v,
      alp  = dot_product(r0,r )/dot_product(r0,ap )! triple checked.
      y(:) = t(:) - r(:) - alp*w(:) + alp*ap(:) !triple checked.
      
      t(:) = r(:) - alp*ap(:) ! triple checked.

      q(:) = matmul(a,t) ! triple checked. s=q=at=c
      
      r0rk=dot_product(r0,r)
      if(itr==1)then
        gzi  = dot_product(q,t)/dot_product(q,q)! double checked.
        nu   = 0.0d0 ! double checked.
      else
        val1 = dot_product(y,y)*dot_product(q,t) - dot_product(y,t)*dot_product(q,y)
        val2 = dot_product(q,q)*dot_product(y,y) - dot_product(y,q)*dot_product(q,y)
        
        if(  val2==0.0d0 ) then
          !print *, itr
          !print *,  r(:), alp*ap(:) 
          stop "GPBiCG devide by zero"
        endif
        gzi  = val1/val2
        val1 = dot_product(q,q)*dot_product(y,t) - dot_product(y,q)*dot_product(q,t)
        nu  = val1/val2  !triple checked.
      endif
      
      u(:) = gzi*ap + nu*(t_(:) -r(:) + beta*u(:)  ) !double checked.
      z(:) = gzi*r(:) + nu*z(:) - alp*u(:)!double checked.
      x(:) = x(:) +alp*p(:) +z(:) !double checked.

      r(:) = t(:) -nu*y(:) -gzi*q(:)!double checked.
      t_(:)=t(:)
      rr = dot_product(r,r)
      !print *, 'itr, er =', itr,sqrt(rr),sqrt(rr)/sqrt(init_rr)
      !print *, "ans = ",x(:)
      if (sqrt(rr)/sqrt(init_rr) < er0 ) exit
      r0rk=dot_product(r0,r)
      beta=alp/gzi*dot_product(r0,r)/r0rk
      w(:) = q(:) + beta * ap(:)
      if(itr==itrmax_) then
        print *, "ERROR :: GPBiCG did not converge."
      endif
    enddo
 end subroutine 
!===============================================================


 
!===========================================================================
subroutine preconditioned_GPBiCG(a, b, x, n, itrmax, er)
  integer(int32), intent(in) :: n
  integer(int32),optional,intent(in) :: itrmax
  real(real64), intent(in) :: a(1:n,1:n), b(1:n)
  real(real64), optional,intent(in)::er
  real(real64), intent(inout) :: x(1:n)
  real(real64) :: L(1:n,1:n),d(1:n)
    ! presented by Moe Thuthu et al., 2009, algorithm #3
    integer(int32) itr,itrmax_,i,j,k
    real(real64) alp,c1, rr,er0,init_rr,beta,lld,ld
    real(real64) gzi,nu,val1,val2,r0rk,eps
    real(real64) r(n),rk(n),r_(n),r_k(n), r0(n), p(n), y(n),ap(n),q(n)
    real(real64) u(n),w(n),t(n),t_(n),z(n)

    print *, "<<< Under implementation >>>"
    
    ! Incomplete Cholosky decomposition.
    ! http://www.slis.tsukuba.ac.jp/~fujisawa.makoto.fu/cgi-bin/wiki/index.php?%A5%B3%A5%EC%A5%B9%A5%AD%A1%BC%CA%AC%B2%F2
    ! >>>>>>>>>>
    L(:,:) = 0.0d0
    d(:) = 0.0d0
    d(1) = a(1,1)
    L(1,1) = 1.0d0
    L(:,:) = imcompleteCholosky(a)
    call showArray(L)
    
    !do i=2, n
    !  ! i < kの場合
    !  do j=1, i
    !    if( abs(a(i,j)) < dble(1.0e-10)  )then
    !      cycle
    !    else
    !      lld = a(i,j)
    !      do k=1, j
    !        lld = lld - L(i,k)*L(j,k)*d(k)
    !      enddo
    !      if(d(j)==0.0d0)then
    !        stop "Error :: d(j)==0.0d0"
    !      endif
    !      L(i,j) = 1.0d0/d(j)*lld
    !    endif
    !    ld = a(i,i)
    !    do k=1, i
    !      ld = ld - L(i,k)*L(i,k)*d(k)
    !    enddo
    !    d(i) = ld
    !    L(i,i) = 1.0d0
    !  enddo
    !  
    !enddo
!
    print *, d

    ! <<<<<<<<<<

    eps=dble(1.00e-14)
    er0=input(default=eps,option=er)

    r(:) = b - matmul(a,x)
    call icres(L, d, r, p, n)
    


    c1 = dot_product(r,r)
    init_rr=c1
    if (c1 < er0) return
    beta=0.0d0
    !p(:) = 0.0d0
    r0(:) = r(:)
    w(:) = 0.0d0
    u(:) = 0.0d0
    t(:) = 0.0d0
    t_(:)= 0.0d0
    z(:) = 0.0d0
    itrmax_=input(default=100000,option=itrmax)
    itrmax_=10
    do itr = 1, itrmax_ 
      call icres(L, d, r, r_k, n)

      !p(:) = r(:)+beta*(p(:)-u(:) )  ! triple checked.
      !r0rk=dot_product(r0,r)
      ap   = matmul(a,p) ! triple checked. ap=v,
      alp  = dot_product(r,r_k )/dot_product(p,ap )! triple checked.
      x(:) = x(:) + alp*ap(:)
      rk(:)=r(:)
      r(:) = r(:) - alp*ap(:)
      !y(:) = t(:) - r(:) - alp*w(:) + alp*ap(:) !triple checked.
      !t(:) = r(:) - alp*ap(:) ! triple checked.

      !q(:) = matmul(a,t) ! triple checked. s=q=at=c
      !if(itr==1)then
      !  gzi  = dot_product(q,t)/dot_product(q,q)! double checked.
      !  nu   = 0.0d0 ! double checked.
      !else
      !  val1 = dot_product(y,y)*dot_product(q,t) - dot_product(y,t)*dot_product(q,y)
      !  val2 = dot_product(q,q)*dot_product(y,y) - dot_product(y,q)*dot_product(q,y)
      !  
      !  if(  val2==0.0d0 ) stop "Bicgstab devide by zero"
      !  gzi  = val1/val2
      !  val1 = dot_product(q,q)*dot_product(y,t) - dot_product(y,q)*dot_product(q,t)
      !  nu  = val1/val2  !triple checked.
      !endif

      !u(:) = gzi*ap + nu*(t_(:) -r(:) + beta*u(:)  ) !double checked.
      !z(:) = gzi*r(:) + nu*z(:) - alp*u(:)!double checked.
      !x(:) = x(:) +alp*p(:) +z(:) !double checked.
      !r(:) = t(:) -nu*y(:) -gzi*q(:)!double checked.
      !t_(:)=t(:)
      rr = dot_product(r,r)
      print *, rr
      !print *, 'itr, er =', itr,rr,sqrt(rr)/sqrt(init_rr)
      !print *, "ans = ",x(:)
      if (sqrt(rr)/sqrt(init_rr) < er0 ) exit
      call icres(L, d, r, r_, n)
      beta = dot_product(r,r_)/dot_product(rk,r_k)
      p(:)=r_(:)+beta*p(:)
      !beta=alp/gzi*dot_product(r0,r)/r0rk
      !w(:) = q(:) + beta * ap(:)
      if(itr==itrmax_) then
        print *, "ERROR :: preconditioned-CG did not converge."
      endif
    enddo
 end subroutine 
!===============================================================

subroutine icres(L, d, r, u, n)
  real(real64) :: y(n), rly,lu
  real(real64),intent(inout) :: L(1:n,1:n), d(1:n),r(1:n),u(1:n)
  integer(int32),intent(in)  :: n
  integer(int32) :: i,j
  do i=1, n
    rly = r(i)
    do j=1, i
      rly = rly - L(i,j)*y(j)
    enddo
    y(i) = rly/L(i,i)
  enddo

  do i=n, 1, -1
    lu=0.0d0
    do j=i+1, n
      lu = lu + L(j,i)*u(j)
    enddo
    u(i)= y(i)-d(i)*lu
  enddo


end subroutine icres

end module
!====================================================================================