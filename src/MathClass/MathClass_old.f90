module vector
  implicit none
contains
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
		real(8) l
		real(8),allocatable::avec(:)
		integer,intent(in):: x2,x11,x12

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

end module vector
