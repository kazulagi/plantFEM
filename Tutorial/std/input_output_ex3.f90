program leqsol95

    ! LAPACK95のコンパイル時にあわせて作成されたモジュールを使います。
      use f95_lapack
      use obj
    
      implicit none
    
    
      integer(int32), parameter :: NN=10000! rank 
      real(real64)  :: t1,t2 ! time measure
      !real(real64) :: A(1:N,1:N), B(1:N), X(1:N) ! A, x and B
      !real(real64) :: Val(1:10) 
      !integer(int32) :: index_i(1:10),index_j(1:10)
      real(real64)  ,allocatable :: Val_c(:) 
      integer(int32),allocatable :: index_i_c(:),index_j_c(:)
      integer(int32) :: i,m,k,j
      real(real64) :: Amat(NN,NN),bvec(NN),xvec(NN)
      type(LinearSolver_) :: solver ! linear solver instance.
      type(IO_) :: f
      type(Random_) ::rdm
  
      call rdm%init()
      !call f%open("Amat.txt")
      do i=1,size(Amat,1)
          Amat(i,i)=rdm%random()+10.0d0
      enddo
      do i=1,size(Amat,1)-1
          Amat(i,i+1)=Amat(i,i+1)+rdm%random()+1.0d0
          Amat(i+1,i)=Amat(i+1,i)+rdm%random()+2.0d0
      enddo
      do i=1,NN
          Bvec(i)=rdm%random()+2.0d0
      enddo
    ! LAPACK95（ラッパー）を介してLAPACKのルーチンを呼び出します。
    ! K に K を上三角行列にコレスキー分解した結果が，
    ! P に連立一次方程式の解が格納されます。
    ! K や P を書き換えられたくない場合は注意しましょう。

    call cpu_time(t1)  
      call LA_POSV( Amat, Bvec )
      call cpu_time(t2)  
      !write(*,'(A)') 'd ='
      !write(*,'(F12.4)') P
      print *, "Solved by LAPACK-LA_POSV","/",t2-t1," sec."
    
    end program leqsol95