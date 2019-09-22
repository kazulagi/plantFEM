module LinearSolverClass
  use MathClass
  implicit none
contains
!====================================================================================
subroutine gauss_seidel(a, b, x, n, itrmax, er0)
  integer, intent(in) :: n, itrmax
  real(8), intent(in)  :: a(n, n), b(n), er0
  real(8), intent(out) :: x(n)
  real(8) s, er, rd(n), r(n)
  integer i, itr
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
  integer, intent(in) :: n
  real(8), intent(in) :: a0(n,n), b(n)
  real(8), intent(out) :: x(n)
  integer i, j, k, m,nn, mm
  real(8) ar, am, t, a(n,n), w(n)
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
  integer, intent(in) :: n, itrmax,DBC(:,:)
  real(8), intent(in) :: a(n,n), b(n), er,DBCVal(:,:)
  real(8), intent(inout) :: x(n)
     integer itr,i,j
     real(8) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(8) r(n), r0(n), p(n), y(n), e(n), v(n)
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

subroutine bicgstab1d(a, b, x, n, itrmax, er)
  integer, intent(in) :: n, itrmax
  real(8), intent(in) :: a(n,n), b(n), er
  real(8), intent(inout) :: x(n)
     integer itr
     real(8) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(8) r(n), r0(n), p(n), y(n), e(n), v(n)
     er0=1.00e-14
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
  integer, intent(in) :: n, itrmax,u_nod_x(:),u_nod_y(:)
  real(8), intent(in) :: a(n,n),b(n), er
  real(8), intent(inout) :: x(n)
  integer itr
     real(8) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(8) r(n), r0(n), p(n), y(n), e(n), v(n)
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
  integer, intent(in) :: n, itrmax,u_nod_x(:),u_nod_y(:)
  real(8), intent(in) :: a(n,n),b(n), er,u_nod_dis_x(:),u_nod_dis_y(:)
  real(8), intent(inout) :: x(n)
     integer itr
     real(8) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(8) r(n), r0(n), p(n), y(n), e(n), v(n)
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
  integer, intent(in) :: n, itrmax,DBoundNodID(:,:),SetBC
  real(8), intent(in) :: a(n,n),b(n), er,DBoundVal(:,:)
  real(8), intent(inout) :: x(n)
     integer itr
     real(8) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(8) r(n), r0(n), p(n), y(n), e(n), v(n)
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
	integer,intent(in)::u_nod_x(:),u_nod_y(:)
	real(8), intent(in) :: u_nod_dis_x(:),u_nod_dis_y(:)
	real(8),intent(inout)::r(:),x(:)
	 integer i
	 
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
	integer,intent(in)::u_nod_x(:),u_nod_y(:)
	real(8),intent(inout)::r(:)
	 integer i
	 
	 do i=1,size(u_nod_x)

		r( 2*u_nod_x(i)-1 )=0.0d0
	 enddo
	 
	 do i=1,size(u_nod_y)

		r( 2*u_nod_y(i) )=0.0d0
	 enddo
	
  end subroutine modify_residual
!====================================================================================
subroutine modify_residual_dirichlet(r,x, DBoundNodID, DBoundVal,SetBC)
  integer,intent(in)::DBoundNodID(:,:),SetBC
  real(8),intent(in)::DBoundVal(:,:)
	real(8),intent(inout)::r(:),x(:)

  
	integer :: i,j,k,dim_num,dbc_num
	real(8) :: val

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
  integer, intent(in) :: n
  integer,optional,intent(in) :: itrmax
  real(8), intent(in) :: a(n,n), b(n)
  real(8), optional,intent(in)::er
  real(8), intent(inout) :: x(n)
     integer itr,itrmax_
     real(8) alp,c1, rr,er0,init_rr,beta
     real(8) gzi,nu,val1,val2,r0rk,eps
     real(8) r(n), r0(n), p(n), y(n),ap(n),q(n)
     real(8) u(n),w(n),t(n),t_(n),z(n)
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
      r0rk=dot_product(r0,r)
      p(:) = r(:)+beta*(p(:)-u(:) )  ! triple checked.
      ap   = matmul(a,p) ! triple checked. ap=v,
      alp  = dot_product(r0,r )/dot_product(r0,ap )! triple checked.
      y(:) = t(:) - r(:) - alp*w(:) + alp*ap(:) !triple checked.
      t(:) = r(:) - alp*ap(:) ! triple checked.
      q(:) = matmul(a,t) ! triple checked. s=q=at=c
      if(itr==1)then
        gzi  = dot_product(q,t)/dot_product(q,q)! double checked.
        nu   = 0.0d0 ! double checked.
      else
        val1 = dot_product(y,y)*dot_product(q,t) - dot_product(y,t)*dot_product(q,y)
        val2 = dot_product(q,q)*dot_product(y,y) - dot_product(y,q)*dot_product(q,y)
        
        if(  val2==0.0d0 ) then
          print *, itr
          print *,  r(:), alp*ap(:) 
          stop "GPBiCG devide by zero"
        endif
        gzi  = val1/val2
        val1 = dot_product(q,q)*dot_product(y,t) - dot_product(y,q)*dot_product(q,t)
        nu  = val1/val2  !triple checked.
      endif

      u(:) = gzi*ap + nu*(t_(:) -r(:) + beta*u(:)  ) !double checked.
      z(:) = gzi*r(:) + nu*z(:) - alp*u(:)!double checked.
      x(:) = x(:) +alp*ap(:) +z(:) !double checked.
      r(:) = t(:) -nu*y(:) -gzi*q(:)!double checked.
      t_(:)=t(:)
      rr = dot_product(r,r)
      print *, 'itr, er =', itr,sqrt(rr),sqrt(rr)/sqrt(init_rr)
      !print *, "ans = ",x(:)
      if (sqrt(rr)/sqrt(init_rr) < er0.and. itr ==5) exit
      beta=alp/gzi*dot_product(r0,r)/r0rk
      w(:) = q(:) + beta * ap(:)
      if(itr==itrmax_) then
        print *, "ERROR :: GPBiCG did not converge."
      endif
    enddo
 end subroutine 
!===============================================================

 
!===========================================================================
subroutine pre_processed_GPBiCG(a, b, x, n, itrmax, er)
  integer, intent(in) :: n
  integer,optional,intent(in) :: itrmax
  real(8), intent(in) :: a(n,n), b(n)
  real(8), optional,intent(in)::er
  real(8), intent(inout) :: x(n)
    ! presented by Moe Thuthu et al., 2009, algorithm #3
     integer itr,itrmax_
     real(8) alp,c1, rr,er0,init_rr,beta
     real(8) gzi,nu,val1,val2,r0rk,eps
     real(8) r(n), r0(n), p(n), y(n),ap(n),q(n)
     real(8) u(n),w(n),t(n),t_(n),z(n)
     eps=1.00e-14
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
      r0rk=dot_product(r0,r)
      p(:) = r(:)+beta*(p(:)-u(:) )  ! triple checked.
      ap   = matmul(a,p) ! triple checked. ap=v,
      alp  = dot_product(r0,r )/dot_product(r0,ap )! triple checked.
      y(:) = t(:) - r(:) - alp*w(:) + alp*ap(:) !triple checked.
      t(:) = r(:) - alp*ap(:) ! triple checked.
      q(:) = matmul(a,t) ! triple checked. s=q=at=c
      if(itr==1)then
        gzi  = dot_product(q,t)/dot_product(q,q)! double checked.
        nu   = 0.0d0 ! double checked.
      else
        val1 = dot_product(y,y)*dot_product(q,t) - dot_product(y,t)*dot_product(q,y)
        val2 = dot_product(q,q)*dot_product(y,y) - dot_product(y,q)*dot_product(q,y)
        
        if(  val2==0.0d0 ) stop "Bicgstab devide by zero"
        gzi  = val1/val2
        val1 = dot_product(q,q)*dot_product(y,t) - dot_product(y,q)*dot_product(q,t)
        nu  = val1/val2  !triple checked.
      endif

      u(:) = gzi*ap + nu*(t_(:) -r(:) + beta*u(:)  ) !double checked.
      z(:) = gzi*r(:) + nu*z(:) - alp*u(:)!double checked.
      x(:) = x(:) +alp*ap(:) +z(:) !double checked.
      r(:) = t(:) -nu*y(:) -gzi*q(:)!double checked.
      t_(:)=t(:)
      rr = dot_product(r,r)
      print *, 'itr, er =', itr,rr,sqrt(rr)/sqrt(init_rr)
      !print *, "ans = ",x(:)
      if (sqrt(rr)/sqrt(init_rr) < er0 .and. itr ==5) exit
      beta=alp/gzi*dot_product(r0,r)/r0rk
      w(:) = q(:) + beta * ap(:)
      if(itr==itrmax_) then
        print *, "ERROR :: GPBiCG did not converge."
      endif
    enddo
 end subroutine 
!===============================================================

end module
!====================================================================================