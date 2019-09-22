module MathClass
	implicit none
	

    interface fstring
        module procedure fstring_Int, fstring_Real, fstring_Int_len, fstring_Real_len
	end interface fstring

	interface input
		module procedure input_Int,input_Real,input_IntVec,input_RealVec,input_IntArray,input_RealArray,input_String,input_logical
	end interface input
	
	interface zeroif
		module procedure zeroif_Int,zeroif_Real
	end interface zeroif

	interface removeWord
		module procedure removeWord_String 
	end interface 

contains


!########################################
function norm(vec) result(a)
	real(8),intent(in)::vec(:)
	integer :: n
	real(8) :: a

	n=size(vec)
	a=dsqrt(dot_product(vec,vec) )

end function
!########################################



!########################################
function SearchNearestCoord(Array,x)  result(id)
	real(8),intent(in) :: Array(:,:)
	real(8),intent(in) :: x(:)
	integer,allocatable::xr(:)

	integer :: i,id,n,m,norm,tr_norm

	n=size(Array,1)
	m=size(Array,2)
	if(m/=size(x) )then
		stop "ERROR :: SearchNearestCoord >> size(Array,2) should be =size(x)"
	endif

	allocate(xr(m) )
	do i=1,n
		xr(:)=Array(i,:)
		tr_norm=dot_product(xr-x,xr-x)
		if(i==1)then
			norm=tr_norm
			id  =i
		else
			if(norm > tr_norm)then
				norm=tr_norm
				id  =i
			else
				cycle
			endif
		endif
	enddo


	
end function
!########################################

!##################################################
function SearchIDIntVec(Vec,val) result(id_)
	integer,intent(in) :: Vec(:)
	integer,intent(in) :: val

	integer :: i,id_

	do i=1,size(Vec)
		if(Vec(i)==val )then
			id_=i
			return
		endif
	enddo

end function
!##################################################
subroutine heapsort(n,array)
  	integer,intent(in) :: n
  	integer,intent(inout) :: array(1:n)
  
  	integer ::i,k,j,l
  	integer :: t
  
  	if(n.le.0)then
  		write(6,*)"Error, at heapsort"; stop
  	endif
  	if(n.eq.1)return

	l=n/2+1
	k=n
	do while(k.ne.1)
	    if(l.gt.1)then
	        l=l-1
	        t=array(L)
	    else
	        t=array(k)
	        array(k)=array(1)
	        k=k-1
	        if(k.eq.1) then
	           	array(1)=t
	        	exit
	        endif
	    endif
	    i=l
	    j=l+l
	    do while(j.le.k)
	        if(j.lt.k)then
	           	if(array(j).lt.array(j+1))j=j+1
	        endif
	        if (t.lt.array(j))then
	        	array(i)=array(j)
	        	i=j
	        	j=j+j
	        else
	        	j=k+1
	        endif
	    enddo
	 	array(i)=t
  	enddo

	return
end subroutine heapsort


!==========================================================
!calculate cross product
!---------------------------
function cross_product(a,b) result (c)
	real(8), intent(in) :: a(:),b(:)
	real(8), allocatable :: c(:)
  
    if(size(a) /= size(b)) then
        stop  "wrong number on size a, b"
    endif
 
    allocate(c(size(a,1)))
  	if(size(c,1)==3) then
     	c(1) = a(2)*b(3) - a(3)*b(2)
     	c(2) = a(3)*b(1) - a(1)*b(3)
     	c(3) = a(1)*b(2) - a(2)*b(1)
  	else
      	stop "wrong number at cross_product"
  	endif
  
end function cross_product
!=========================================================
!calculate diadic
!----------------------
function diadic(a,b) result(c)
  real(8), intent(in) :: a(:), b(:)
	real(8), allocatable :: c(:,:)
	
	integer n,i,j
	   
	allocate(c(size(a),size(b) ) )
	do i=1,size(a)
		do j=1,size(b)
			c(i,j)=a(i)*b(j)		
		enddo
	enddo

end function diadic	
!==========================================================
!calculate gz
!--------------
subroutine calcgz(x2,x11,x12,nod_coord,gzi)
	real(8), intent(in) :: nod_coord(:,:)
	real(8),intent(out) :: gzi
	integer,intent(in):: x2,x11,x12
		real(8) l
		real(8),allocatable::avec(:)

		allocate(avec(2))
		l = dot_product( nod_coord(x12,1:2) - nod_coord(x11,1:2), &
			nod_coord(x12,1:2) - nod_coord(x11,1:2) ) 
		l=l**(1.0d0/2.0d0)
	 
		avec(1:2) = ( nod_coord(x12,1:2) - nod_coord(x11,1:2)   )/l
		
		if(l==0.0d0)then
			print *, "calcgz l=0"
			gzi=0.0d0
		else
			gzi=1.0d0/l*dot_product( nod_coord(x2,1:2) -nod_coord(x11,1:2),avec(1:2) )
		endif

	 deallocate(avec)
	 
end subroutine calcgz
!==========================================================
subroutine eigen_2d(Amat,eigenvector)
	real(8),intent(in)::Amat(:,:)
	real(8),intent(inout)::eigenvector(:,:)

	real(8)::b,c,phy,eigenvalue(2)
	integer i,j
	
	eigenvalue(:)=0.0d0
	eigenvector(:,:)=0.0d0
	
	b=-1.0d0*(Amat(1,1)+Amat(2,2))
	c=Amat(1,1)*Amat(2,2)-Amat(1,2)*Amat(1,2)
	
	if(Amat(1,2)/=Amat(2,1) )then
		 stop "input matrice is not symmetric"
	endif
	
	do i=1,2
		eigenvalue(i)=(-1.0d0*b+((-1.0d0)**dble(i))*(b*b-4.0d0*c)**(1.0d0/2.0d0))*(0.50d0)
	enddo
	
	do i=1,2
		if(Amat(1,2)==0 )then
			cycle
		elseif(Amat(1,2)/=0 )then
			phy=atan( (eigenvalue(i)-Amat(1,1))/Amat(1,2) ) 
			
			do j=1,2
				eigenvector(i,1:2)=(/cos(phy),sin(phy)/)
			enddo
		endif
	enddo
	
	do i=1,2
		eigenvector(i,:)=eigenvalue(i)*eigenvector(i,:)
	enddo
end subroutine eigen_2d
!==========================================================
function signmm(a) result(b)
	real(8),intent(in)::a
	real(8) b
	
	if(a>0)then
		b=1.0d0
	elseif(a<0)then
		b=-1.0d0
	elseif(a==0)then
		b=0.0d0
	else
		stop "ERROR: Invalid Real(8) in function_signm"
	endif
	
end function signmm 
!==========================================================
recursive function det_mat(a,n) result(det)
  	integer, intent(in) :: n
  	real(8), intent(in) :: a(n, n)
  	real(8) det, b(n-1, n-1)
  	integer i
	if (n > 1) then
		det = 0.0d0
		do i = 1, n
		  	b(1:i-1, 1:n-1) = a(1:i-1, 2:n)
		  	b(i:n-1, 1:n-1) = a(i+1:n, 2:n)
		  	det = det + (-1.0d0) ** (i + 1) &
			* a(i, 1) * det_mat(b, n-1)
		
		enddo
	else
		det = a(1,1)
	endif
end function det_mat
!=====================================================================================
subroutine trans_rank_2(A,A_T)
	real(8),intent(in)::A(:,:)
	real(8),allocatable,intent(out)::A_T(:,:)
	integer n,m,i,j
	
	n=size(A,1)
	m=size(A,2)
	if(.not. allocated(A_T) )allocate(A_T(m,n))
	
	do i=1,n
	  	do j=1, m
		  	A_T(j,i)=A(i,j)
	  	enddo
	enddo
	
 end subroutine trans_rank_2
!================================================================================== 
function trans1(A) result(A_T)
	real(8),intent(in)::A(:)
	real(8),allocatable::A_T(:,:)
	integer n,m,i,j
	
	n=size(A)
	if(.not. allocated(A_T) )allocate(A_T(1,n))
	
	do i=1,n
		A_T(1,i)=A(i)
	enddo
	
 end function trans1
!==================================================================================    
function trans2(A) result(A_T)
	real(8),intent(in)::A(:,:)
	real(8),allocatable::A_T(:,:)
	integer n,m,i,j
	
	n=size(A,1)
	m=size(A,2)
	if(.not. allocated(A_T) )allocate(A_T(m,n))
	
	do i=1,n
	  	do j=1, m
		  	A_T(j,i)=A(i,j)
	  	enddo
	enddo
	
 end function trans2
!================================================================================== 
subroutine inverse_rank_2(A,A_inv)
	real(8),intent(in)::A(:,:)
	real(8),allocatable::A_inv(:,:)
	real(8) detA,detA_1
	integer m,n
	
	m=size(A,1)
	n=size(A,2)
	if(.not. allocated(A_inv) )allocate(A_inv(m,n))
	detA=det_mat(A,n)
	if(detA==0.0d0) stop "ERROR: inverse, detA=0"
	detA_1=1.0d0/detA
	if(n==2)then
	  	A_inv(1,1)=detA_1*A(2,2)
	  	A_inv(1,2)=-detA_1*A(1,2)
	  	A_inv(2,1)=-detA_1*A(2,1)
	  	A_inv(2,2)=detA_1*A(1,1)
	elseif(n==3)then
	  	A_inv(1,1)=detA_1*(A(2,2)*A(3,3)-A(2,3)*A(3,2))
	  	A_inv(1,2)=detA_1*(A(1,3)*A(3,2)-A(1,2)*A(3,3))
	  	A_inv(1,3)=detA_1*(A(1,2)*A(2,3)-A(1,3)*A(2,2))
	  	A_inv(2,1)=detA_1*(A(2,3)*A(3,1)-A(2,1)*A(3,3))
	  	A_inv(2,2)=detA_1*(A(1,1)*A(3,3)-A(1,3)*A(3,1))
	  	A_inv(2,3)=detA_1*(A(1,3)*A(2,1)-A(1,1)*A(2,3))
	  	A_inv(3,1)=detA_1*(A(2,1)*A(3,2)-A(2,2)*A(3,1))
	  	A_inv(3,2)=detA_1*(A(1,2)*A(3,1)-A(1,1)*A(3,2))
	  	A_inv(3,3)=detA_1*(A(1,1)*A(2,2)-A(1,2)*A(2,1))
	else
	  	print *, "ERROR: Aij with i=j=",n,"/=2or3"
	endif
	
 end subroutine inverse_rank_2
!================================================================================== 
subroutine tensor_exponential(A,expA,TOL,itr_tol)
  	real(8),intent(in)::A(:,:),TOL
  	real(8),allocatable,intent(inout)::expA(:,:)
  	integer, intent(in)::itr_tol
  	real(8),allocatable::increA(:,:)
  	real(8) increment,NN
  	integer i,j,n
	
  	if(.not. allocated(expA) )allocate(expA(size(A,1),size(A,2) ))
  	allocate(increA(size(A,1),size(A,2) ))
  	if(size(A,1)/=size(A,2)) stop "ERROR:tensor exp is not a square matrix"
	
  	expA(:,:)=0.0d0
  	do n=1,size(expA,1)
	  	expA(n,n)=1.0d0
  	enddo
  	NN=1.0d0
  	increA(:,:)=expA(:,:)
  	do n=1,itr_tol
	  	if(n>1)then
		  	NN       = NN*(NN+1.0d0)
	  	endif
	  	increA(:,:)=matmul(increA,A)
	  	expA(:,:)= expA(:,:)+1.0d0/NN*increA(:,:)
	  
	  	increment=0.0d0
	  	do i=1,size(A,1)
		  	do j=1,size(A,2)
			  	increment=increment+1.0d0/NN*increA(i,j)*increA(i,j)
		  	enddo
	  	enddo
	  
	  	if(increment<=TOL)then
		  	exit
	  	else
		  	if(n>=itr_tol)then
			   	stop "tensor exponential is not converged"
		  	endif
			cycle
	  	endif
  	enddo
  
  	deallocate(increA)
  
end subroutine tensor_exponential
!================================================================================== 
subroutine tensor_expo_der(A,expA_A,TOL,itr_tol)
  	real(8),intent(in)::A(:,:),TOL
  	real(8),allocatable,intent(inout)::expA_A(:,:,:,:)
  	integer, intent(in)::itr_tol
  	real(8),allocatable::increA_1(:,:),increA_2(:,:),increA_3(:,:,:,:),I_ij(:,:),A_inv(:,:)
  	real(8) increment,NN
  	integer i,j,k,l,n,m,o
	
  	if(.not. allocated(expA_A) )allocate(expA_A(size(A,1),size(A,1),size(A,1),size(A,1) ))
  	allocate(I_ij(size(A,1),size(A,1) ))
  	allocate(increA_1(size(A,1),size(A,1) ))
  	allocate(increA_2(size(A,1),size(A,1) ))
  	allocate(increA_3(size(A,1),size(A,1),size(A,1),size(A,1) ) )
  	if(size(A,1)/=size(A,2)) stop "ERROR:tensor exp is not a square matrix"
  
  	call inverse_rank_2(A,A_inv)

  	I_ij(:,:)=0.0d0
  	do n=1,size(expA_A,1)
		I_ij(n,n)=1.0d0
  	enddo
  	NN=1.0d0
  
  	do i=1,size(A,1)
	  	do j=1,size(A,1)
		  	do k=1, size(A,1)
			  	do l=1, size(A,1)
				  	expA_A(i,j,k,l)=I_ij(i,k)*I_ij(l,j)
			  	enddo
		  	enddo
	  	enddo
  	enddo
  
  	increA_1(:,:)=I_ij(:,:)
  	increA_2(:,:)=I_ij(:,:)
  	do n=1,itr_tol
		if(n>2)then
			NN = NN*(NN+1.0d0)
		endif
		increA_1(:,:)=A_inv(:,:)
		increA_2(:,:)=matmul(increA_2,A)
	  
	  	increA_3(:,:,:,:)=0.0d0
	  	do m=1,n
			increA_1(:,:)=matmul(increA_1,A    )
		  
		  	increA_2(:,:)=matmul(increA_2,A_inv)

		  	do i=1,size(A,1)
				do j=1,size(A,1)
				  	do k=1, size(A,1)
					  	do l=1, size(A,1)
						  	increA_3(i,j,k,l)=increA_3(i,j,k,l)+increA_1(i,k)*increA_2(l,j)
						  	expA_A(i,j,k,l)=expA_A(i,j,k,l)+1.0d0/NN*increA_3(i,j,k,l)
					  	enddo
					enddo
			  	enddo
		  	enddo			
	  	enddo

	  	do i=1,size(A,1)
		  	do j=1,size(A,1)
			  	do k=1, size(A,1)
				  	do l=1, size(A,1)
					  	increment=increment+1.0d0/NN*increA_3(i,j,k,l)&
						  	*increA_3(i,j,k,l)&
						  	*increA_3(i,j,k,l)&
						  	*increA_3(i,j,k,l)
				  	enddo
			  	enddo
		  	enddo
	  	enddo			

	  	if(increment<=TOL)then
			exit
	  	else
		  	if(n>=itr_tol)then
			   	stop "tensor exponential is not converged"
		  	endif
		  	cycle
	  	endif
  	enddo
  
  	deallocate(increA_1,increA_2,increA_3,I_ij,A_inv)
  
end subroutine tensor_expo_der
!================================================================================== 

function GetNormRe(a) result(b)
	real(8),intent(in)::a(:)
	real(8) :: b
	b=dot_product(a,a)
end function
!================================================================================== 

function GetNormMatRe(a) result(b)
	real(8),intent(in)::a(:,:)
	real(8) :: b
	integer :: i,j
	b=0
	do i=1,size(a,1)
		do j=1,size(a,2)
			b=b+a(i,j)*a(i,j)
		enddo
	enddo
end function
!================================================================================== 

function trace(a) result(b)
	real(8),intent(in)::a(:,:)
	real(8) :: b
	integer :: i,j
	b=0
	do i=1,size(a,1)
		b=b+a(i,i)
	enddo
end function
!================================================================================== 

!================================================================================== 
function pi(n) result(res)
	integer,intent(in)::n
	real(8) :: ptr
	real(8) :: an,bn,tn,pn
	real(8) :: atr,btr,ttr
	real(8) :: res

	integer :: i

	an=1.0d0
	bn=1.0d0/sqrt(2.0d0)
	tn=0.250d0
	pn=1.00d0
	do i=1,n
		atr=0.50d0*(an+bn)
		btr=dsqrt(an*bn)
		ttr=tn-pn*(atr-an)*(atr-an)
		ptr=2.0d0*pn

		an=atr
		bn=btr
		tn=ttr
		pn=ptr

		res=(atr+btr)*(atr+btr)/4.0d0/ttr
	enddo

end function
!================================================================================== 




!================================================================================== 
function fstring_int(x) result(a)
	integer,intent(in) :: x
	character(len=20)	:: a

	write(a,*) x

end function
!================================================================================== 




!================================================================================== 
function fstring_int_len(x,length) result(a)
	integer,intent(in) :: x
	integer,intent(in) :: length
	character(len=length)	:: a

	write(a,*) x

end function
!================================================================================== 



!================================================================================== 
function fstring_real(x) result(a)
	real(8),intent(in) :: x
	character(len=20)	:: a


	write(a,'(f0.8)') x

end function
!================================================================================== 



!================================================================================== 
function fstring_real_len(x,length) result(a)
	real(8),intent(in) :: x
	integer,intent(in) :: length
	character(len=60)	:: a
	character*40						:: form

	
	write(a,'(f0.10)') x

end function
!================================================================================== 



!================================================================================== 
function fint(ch)	result(a)
	character(*),intent(in)			:: ch
	integer				:: a

	read(ch,*) a

end function
!================================================================================== 


!================================================================================== 
function freal(ch)	result(a)
	character(*),intent(in)			:: ch
	real(8)				:: a

	read(ch,*) a

end function
!================================================================================== 



!================================================================================== 
function input_Int(default,option) result(val)
	integer,intent(in) :: default
	integer,optional,intent(in)::option
	integer :: val

	if(present(option) )then
		val=option
	else
		val=default
	endif

end function
!================================================================================== 




!================================================================================== 
function input_Real(default,option) result(val)
	real(8),intent(in) :: default
	real(8),optional,intent(in)::option
	real(8) :: val

	if(present(option) )then
		val=option
	else
		val=default
	endif

end function
!================================================================================== 





!================================================================================== 
function input_IntVec(default,option) result(val)
	integer,intent(in) :: default(:)
	integer,optional,intent(in)::option(:)
	integer,allocatable :: val(:)
	integer :: n,m

	if(present(option) )then
		n=size(option,1)
		allocate(val(n) )
		val(:)=option(:)
	else
		n=size(default,1)
		allocate(val(n) )
		val(:)=default(:)
	endif

end function
!================================================================================== 

!================================================================================== 
function input_Realvec(default,option) result(val)
	real(8),intent(in) :: default(:)
	real(8),optional,intent(in)::option(:)
	real(8),allocatable :: val(:)
	integer :: n,m

	if(present(option) )then
		n=size(option,1)
		allocate(val(n) )
		val(:)=option(:)
	else
		n=size(default,1)
		allocate(val(n) )
		val(:)=default(:)
	endif

end function
!================================================================================== 




!================================================================================== 
function input_IntArray(default,option) result(val)
	integer,intent(in) :: default(:,:)
	integer,optional,intent(in)::option(:,:)
	integer,allocatable :: val(:,:)
	integer :: n,m

	if(present(option) )then
		n=size(option,1)
		m=size(option,2)
		allocate(val(n,m) )
		val(:,:)=option(:,:)
	else
		n=size(default,1)
		m=size(default,2)
		allocate(val(n,m) )
		val(:,:)=default(:,:)
	endif

end function
!================================================================================== 

!================================================================================== 
function input_RealArray(default,option) result(val)
	real(8),intent(in) :: default(:,:)
	real(8),optional,intent(in)::option(:,:)
	real(8),allocatable :: val(:,:)
	integer :: n,m

	if(present(option) )then
		n=size(option,1)
		m=size(option,2)
		allocate(val(n,m) )
		val(:,:)=option(:,:)
	else
		n=size(default,1)
		m=size(default,2)
		allocate(val(n,m) )
		val(:,:)=default(:,:)
	endif

end function
!================================================================================== 


!================================================================================== 
function input_String(default,option) result(val)
	character(*),intent(in) :: default
	character(*),optional,intent(in)::option
	character(len(default) ) :: val

	if(present(option) )then
		val=option
	else
		val=default
	endif

end function
!================================================================================== 

!================================================================================== 
function input_logical(default,option) result(val)
	logical,intent(in) :: default
	logical,optional,intent(in)::option
	logical :: val

	if(present(option) )then
		val=option
	else
		val=default
	endif

end function
!================================================================================== 

function zeroif_Int(val,negative,positive) result(retval)
	integer,intent(in)::val
	integer :: retval
	logical,optional,intent(in) :: negative,positive

	if(val/=val)then
		print *, "ERROR :: MAthClass >> zeroif_Int is invalid"
	endif
	retval=val
	if(present(negative) )then
		if(negative .eqv. .true.)then
			if(val<0)then
				retval=0
			endif
		endif
	endif
	
	if(present(positive) )then
		if(positive .eqv. .true.)then
			if(val>0)then
				retval=0
			endif
		endif
	endif

end function
	

function zeroif_Real(val,negative,positive) result(retval)
	real(8),intent(in)::val
	real(8) :: retval
	logical,optional,intent(in) :: negative,positive

	if(val/=val)then
		print *, "ERROR :: MAthClass >> zeroif_Int is invalid"
	endif
	retval=val
	if(present(negative) )then
		if(negative .eqv. .true.)then
			if(val<0.0d0)then
				retval=0.0d0
			endif
		endif
	endif
	
	if(present(positive) )then
		if(positive .eqv. .true.)then
			if(val>0.0d0)then
				retval=0.0d0
			endif
		endif
	endif

end function

! ########################################################
subroutine removeWord_String(str,keyword,itr,Compare)
	character(*),intent(inout)::str
	character(*),intent(in   )::keyword
	
	integer :: len_total,len_kw,i,j,n,itr_max
	integer,optional,intent(in)::itr
	logical,optional,intent(in)::Compare
	logical :: bk
	

	if(present(Compare))then
		if(Compare .eqv. .true.)then
			print *, "Before :: ",str	
		endif
	endif

	itr_max=input(default=1,option=itr)
	bk=.false.
	len_total=len(str)
	len_kw	 =len(keyword)

	do i=1,itr_max
		n=index(str,keyword)
		do j=n,n+len_kw
			str(j:j)=" "
		enddo
		if(n==0)then
			exit
		endif	
	enddo

	if(present(Compare))then
		if(Compare .eqv. .true.)then
			print *, "After :: ",str	
		endif
	endif


	
end subroutine
! ########################################################

end module MathClass
