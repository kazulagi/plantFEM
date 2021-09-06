!=====================================================================================
module det  ! �s�񎮌v�Z���W���[���u���l�v�Z�̂��߂�Fortran90/95�v���O���~���O����v�����p
  implicit none
contains
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
	real(8),allocatable::increA(:,:)
	integer, intent(in)::itr_tol
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
	real(8),allocatable::increA_1(:,:),increA_2(:,:),increA_3(:,:,:,:),I_ij(:,:),A_inv(:,:)
	integer, intent(in)::itr_tol
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
end module det

