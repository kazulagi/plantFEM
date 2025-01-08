module SparseClass
   use iso_c_binding
   use ArrayClass
   use MathClass
   use RandomClass
   use RangeClass
   implicit none

!> Please edit "src/SparseClass/SparseClass.c"
!> for
!>  'c_dot_product'
!>  'crs_spmv_real32'
!>  'crs_spmv_real64'

   interface
      Subroutine c_dot_product(a, b, n, ret) bind(C, Name='c_dot_product')
         import
         integer(C_size_t), value :: n
         real(c_double), intent(in) :: a(n), b(n)
         real(c_double), intent(out) :: ret(1)
      End Subroutine
   End Interface

   interface
      Subroutine crs_spmv_real32(val, row_ptr, col_idx, old_vector, new_vector, n, col_size) &
         Bind(C, Name="crs_spmv_real32")
         import
         Integer(C_size_t), Value :: n, col_size
         real(C_float), Intent(In) :: val(n)
         integer(C_int64_t), Intent(In) :: row_ptr(n + 1)
         integer(C_int), Intent(In) :: col_idx(col_size)
         real(C_float), Intent(In)  :: old_vector(n)
         real(C_float), Intent(InOut) :: new_vector(n)

      End Subroutine
   End interface

   interface
      Subroutine crs_spmv_real64(val, row_ptr, col_idx, old_vector, new_vector, n, col_size) &
         Bind(C, Name="crs_spmv_real64")
         use iso_c_binding
         import
         Integer(C_size_t), Value :: n, col_size
         real(C_double), Intent(In) :: val(n)
         integer(C_int64_t), Intent(In) :: row_ptr(n + 1)
         integer(C_int), Intent(In) :: col_idx(col_size)
         real(C_double), Intent(In)  :: old_vector(n)
         real(C_double), Intent(InOut) :: new_vector(n)

      End Subroutine
   End interface

!  interface
!  Subroutine crs_spmv_real64_opencl(val,row_ptr,col_idx,old_vector,new_vector,n,col_size) &
!        Bind(C,Name="crs_spmv_real64_opencl")
!        use iso_c_binding
!    import
!    Integer(C_size_t),Value :: n,col_size
!    real(C_double),Intent(In) :: val(n)
!    integer(C_int64_t),Intent(In) :: row_ptr(n+1)
!    integer(C_int),Intent(In) :: col_idx(col_size)
!    real(C_double),Intent(In)  :: old_vector(n)
!    real(C_double),Intent(InOut) :: new_vector(n)
!
!  End Subroutine
!End interface

   interface
      Subroutine c_sparse_matvec(row_ptr, col_idx, val, x, n, n_col, ret) bind(C, Name='c_sparse_matvec')
         import
         integer(C_size_t), value :: n, n_col
         integer(c_int), intent(in) :: row_ptr(n), col_idx(n_col)
         real(c_double), intent(in) :: val(n - 1), x(n - 1)
         real(c_double), intent(out) :: ret(n - 1)
      End Subroutine
   End Interface

   interface sinc
      module procedure sinc_complex64, sinc_real64, sinc_real64_vec
   end interface

   type :: COO_Row_
      real(real64), allocatable :: val(:)
      complex(real64), allocatable :: val_complex64(:)
      integer(int32), allocatable :: col(:)
   end type

   type :: COO_
      type(COO_Row_), allocatable :: row(:)
   contains
      procedure, public :: init => initCOO
      procedure, public :: update => updateCOO
      procedure, public :: set => updateCOO

      procedure, pass :: addCOO
      procedure, pass :: add_complexCOO
      generic :: add => addCOO, add_complexCOO

      procedure, public :: getDenseMatrix => getDenseMatrixCOO
      procedure, public :: to_dense => getDenseMatrixCOO
      procedure, public :: remove => removeCOO
      procedure, public :: getAllCol => getAllColCOO
      procedure, public :: DOF => DOFCOO
      procedure, public :: to_CRS => to_CRSCOO
      procedure, public :: get => getCOO
      procedure, public :: ne => neCOO
      procedure, public :: maxval => maxvalCOO
      procedure, public :: random => randomCOO ! create random sparse matrix

      ! typical matrix
      procedure, public :: eyes => eyesCOO
      procedure, public :: poisson => poissonCOO

      !procedure,public ::getAllCol_as_row_obj => getAllCol_as_row_objCOO
   end type

   type :: CCS_
      integer(int32), allocatable :: col_ptr(:)
      integer(int32), allocatable :: row_idx(:)
      real(real64), allocatable :: val(:)
   contains
      procedure, public :: get_column => get_column_CCS
   end type

   type :: CRS_
      integer(int32), allocatable :: col_idx(:)

      ! Destructive
      integer(int64), allocatable :: row_ptr(:)
      real(real64), allocatable :: val(:)

      real(real32), allocatable :: val_real32(:)
      complex(real64), allocatable :: val_complex64(:)
      integer(int32) :: dtype = real64

   contains
      procedure, public :: init => initCRS
      procedure, public :: eyes => eyesCRS
      procedure, public :: Lanczos => LanczosCRS

      procedure, pass :: matmulCRS
      !procedure,pass :: matmul_opencl_CRS

      procedure, pass :: matmul_c_CRS
      procedure, pass :: matmul_c_real32_CRS

      procedure, pass :: matmul_real32_CRS
      procedure, pass :: matmul_complex_CRS
      generic :: matmul => matmulCRS, matmul_complex_CRS, matmul_real32_CRS, matmul_c_CRS, &
         matmul_c_real32_CRS!,matmul_opencl_CRS

      procedure, public :: to_real32 => to_real32_CRS

      procedure, public :: SpMV => spmv_as_subroutine_CRS
      procedure, public :: eig => eigCRS
      procedure, public :: to_dense => to_denseCRS
      procedure, public :: DOF => DOFCRS

      procedure, public :: remove => removeCRS
      procedure, public :: size => sizeCRS
      procedure, public :: update => updateCRS
      procedure, public :: add => addCRS

      ! 対象行列とみなし，埋まっていない要素をすべて埋める．
      procedure, public :: fillSymmetric => fillSymmetric_CRS

      procedure, public :: get => getCRS
      procedure, public :: is_nonzero => is_nonzeroCRS
      procedure, public :: diag => diagCRS
      procedure, public :: lumped => lumpedCRS

      procedure, public :: divide_by => divide_by_CRS
      procedure, public :: mult_by => mult_by_CRS

      procedure, public :: to_CCS => to_CCSCRS

      procedure, public :: load => loadCRS

      procedure, public :: ILU => ILUCRS
      procedure, public :: ILU_matvec => ILU_matvecCRS
      procedure, public :: BICGSTAB => BICGSTAB_CRSSparse

      procedure, pass :: tensor_exponential_crs
      procedure, pass :: tensor_sqrt_crs
      procedure, pass :: tensor_exp_sqrt_crs
      procedure, pass :: tensor_log_crs
      procedure, public :: tensor_log_crs_modified => tensor_log_crs_modified_Sparse

      procedure, pass :: fixCRS
      procedure, pass :: fix_zeroCRS

      procedure, pass :: tensor_exponential_complex64_crs
      procedure, pass :: tensor_exp_sqrt_complex64_crs

      procedure, pass :: tensor_cos_sqrt_complex64_crs
      procedure, pass :: tensor_cos_sqrt_real64_crs
      procedure, pass :: tensor_cos_sqrt_LPF_real64_crs

      procedure, pass :: tensor_sinc_sqrt_complex64_crs
      procedure, pass :: tensor_sinc_sqrt_real64_crs
      procedure, pass :: tensor_t_sinc_sqrt_LPF_real64_crs

      procedure, pass :: tensor_sqrt_complex64_crs
      procedure, pass :: tensor_log_complex64_crs
      procedure, pass :: fix_complex64_CRS
      procedure, pass :: tensor_d1_wave_kernel_complex64_crs
      procedure, pass :: tensor_wave_kernel_complex_64_crs
      procedure, pass :: tensor_wave_kernel_real_64_crs
      procedure, pass :: tensor_wave_kernel_LPF_real_64_crs
      procedure, pass :: tensor_wave_kernel_RHS_real_64_crs
      procedure, pass :: tensor_wave_kernel_RHS_LPF_real_64_crs
      procedure, pass :: tensor_wave_kernel_RHS_complex_64_crs

      procedure, pass :: tensor_cos_sqrt_cos_sqrt_complex64_crs
      procedure, pass :: tensor_cos_sqrt_sinc_sqrt_complex64_crs
      procedure, pass :: tensor_sinc_sqrt_cos_sqrt_complex64_crs
      procedure, pass :: tensor_sinc_sqrt_sinc_sqrt_complex64_crs

      generic, public :: tensor_exponential => tensor_exponential_complex64_crs, tensor_exponential_crs
      generic, public :: tensor_exp => tensor_exponential_complex64_crs, tensor_exponential_crs
      generic, public :: tensor_exp_sqrt => tensor_exp_sqrt_complex64_crs, tensor_exp_sqrt_crs

      generic, public :: tensor_cos_sqrt => tensor_cos_sqrt_complex64_crs, &
         tensor_cos_sqrt_real64_crs
      generic, public :: tensor_sinc_sqrt => tensor_sinc_sqrt_complex64_crs, &
         tensor_sinc_sqrt_real64_crs

      generic, public :: tensor_cos_sqrt_LPF => tensor_cos_sqrt_LPF_real64_crs
      generic, public :: tensor_t_sinc_sqrt_LPF => tensor_t_sinc_sqrt_LPF_real64_crs

      generic, public :: tensor_sqrt => tensor_sqrt_complex64_crs, tensor_sqrt_crs
      generic, public :: tensor_log => tensor_log_complex64_crs, tensor_log_crs

      !>>> not verified.
      generic, public :: tensor_cos_sqrt_cos_sqrt => tensor_cos_sqrt_cos_sqrt_complex64_crs
      generic, public :: tensor_cos_sqrt_sinc_sqrt => tensor_cos_sqrt_sinc_sqrt_complex64_crs
      generic, public :: tensor_sinc_sqrt_cos_sqrt => tensor_sinc_sqrt_cos_sqrt_complex64_crs
      generic, public :: tensor_sinc_sqrt_sinc_sqrt => tensor_sinc_sqrt_sinc_sqrt_complex64_crs
      !<<< not verified.

      generic, public :: tensor_wave_kernel => tensor_wave_kernel_complex_64_crs, &
         tensor_wave_kernel_real_64_crs
      generic, public :: tensor_wave_kernel_LPF => tensor_wave_kernel_LPF_real_64_crs

      generic, public :: tensor_wave_kernel_RHS => tensor_wave_kernel_RHS_real_64_crs, &
         tensor_wave_kernel_RHS_complex_64_crs, tensor_wave_kernel_RHS_LPF_real_64_crs

      generic, public :: tensor_d1_wave_kernel => tensor_d1_wave_kernel_complex64_crs
      generic, public :: fix => fix_complex64_CRS, fixCRS, fix_zeroCRS
         
   end type

   public :: operator(+)
   public :: operator(-)
   public :: operator(*)
   !public :: operator(/)

   interface operator(+)
      module procedure addCRS_and_CRS
   end interface

   interface operator(-)
      module procedure diffCRS_and_CRS
   end interface

   interface LAPACK_EIG
      module procedure LAPACK_EIG_DENSE, LAPACK_EIG_SPARSE
   end interface

   interface operator(*)
      module procedure multReal64_and_CRS, multCRS_and_Real64
   end interface

   interface matmul
      module procedure :: matmul_CRS_CRS, matmul_CRS_vec
   end interface

   interface LOBPCG
      module procedure LOBPCG_CRS, LOBPCG_SINGLE_CRS
   end interface LOBPCG

   interface to_diag
      module procedure to_diag_vector_to_CRS
   end interface

   interface speye
      module procedure speyeCOO
   end interface

   interface imag_I
      module procedure imag_ICOO
   end interface

!    interface operator(/)
!        module procedure divide_by_diagonal_vector_CRS, divide_by_scalar_CRS
!    end interface

   interface to_COO
      module procedure to_COO_from_DenseMatrix, to_COO_from_ArrayObject
   end interface

   interface to_CRS
      module procedure to_CRS_from_DenseMatrix, to_CRS_from_ArrayObject
   end interface

   type :: Sparse_
      type(COO_) :: COO
      type(CRS_) :: CRS
      type(CCS_) :: CCS
   end type
contains

   subroutine initCOO(this, num_row)
      class(COO_), intent(inout) :: this
      integer(int32), intent(in) :: num_row
      allocate (this%row(num_row))
   end subroutine

   function to_CRSCOO(this, remove_coo) result(CRS_version)
      class(COO_), intent(inout) :: this
      logical, optional, intent(in) :: remove_coo
      type(CRS_) :: CRS_version
      integer(int32) :: i, idx
      integer(int64) :: n, j

      CRS_version%col_idx = this%getAllCol()

      CRS_version%row_ptr = int(zeros(size(this%row) + 1))
      CRS_version%row_ptr(1) = 1
      do i = 2, size(this%row) + 1
         if (.not. allocated(this%row(i - 1)%col)) then
            CRS_version%row_ptr(i) = CRS_version%row_ptr(i - 1)
            cycle
         end if
         CRS_version%row_ptr(i) = CRS_version%row_ptr(i - 1) + size(this%row(i - 1)%col)

         if (present(remove_coo)) then
            if (remove_coo) then
               deallocate (this%row(i - 1)%col)
            end if
         end if

      end do

      !CRS_version%val
      n = size(CRS_version%col_idx)
      allocate (CRS_version%val(n))
      CRS_version%val(:) = 0.0d0

      do i = 1, size(this%row)
         if (.not. allocated(this%row(i)%val)) then
            cycle
         end if
         idx = 0
         do j = CRS_version%row_ptr(i), CRS_version%row_ptr(i + 1) - 1
            idx = idx + 1
            CRS_version%val(j) = this%row(i)%val(idx)
         end do

         if (present(remove_coo)) then
            if (remove_coo) then
               deallocate (this%row(i)%val)
            end if
         end if

      end do

      call this%remove()

   end function

   subroutine updateCOO(this, row, col, val)
      class(COO_), intent(inout) :: this
      integer(int32), intent(in)   :: row
      integer(int32), intent(in)   :: col
      real(real64), intent(in)   :: val
      integer(int32) :: i, col_id

      if (row > this%DOF() .or. row < 1) return
      if (col > this%DOF() .or. col < 1) return

      if (.not. allocated(this%row)) then
         print *, "ERROR :: initCOO"
         print *, "Please call [COO]%init() before this operation."
         return
      end if

      if (.not. allocated(this%row(row)%val)) then
         this%row(row)%val = [val]
         this%row(row)%col = [col]
      else

         ! check duplication
         if (minval(abs(this%row(row)%col(:) - col)) == 0) then
            ! duplication
            do i = 1, size(this%row(row)%col)
               if (this%row(row)%col(i) == col) then
                  col_id = i
                  exit
               end if
            end do
            this%row(row)%val(col_id) = val
         else
            this%row(row)%val = this%row(row)%val//[val]
            this%row(row)%col = this%row(row)%col//[col]
         end if
      end if

   end subroutine

   subroutine addCOO(this, row, col, val)
      class(COO_), intent(inout) :: this
      integer(int32), intent(in)   :: row
      integer(int32), intent(in)   :: col
      real(real64), intent(in)   :: val
      integer(int32) :: i, col_id

      if (row > this%DOF() .or. row < 1) return
      if (col > this%DOF() .or. col < 1) return

      if (.not. allocated(this%row)) then
         print *, "ERROR :: initCOO"
         print *, "Please call [COO]%init() before this operation."
         return
      end if

      if (.not. allocated(this%row(row)%val)) then
         this%row(row)%val = [val]
         this%row(row)%col = [col]
      else

         ! check duplication
         if (minval(abs(this%row(row)%col(:) - col)) == 0) then
            ! duplication
            do i = 1, size(this%row(row)%col)
               if (this%row(row)%col(i) == col) then
                  col_id = i
                  exit
               end if
            end do
            this%row(row)%val(col_id) = this%row(row)%val(col_id) + val
         else
            this%row(row)%val = this%row(row)%val//[val]
            this%row(row)%col = this%row(row)%col//[col]
         end if
      end if

   end subroutine
! ###############################################

! ###############################################
   subroutine add_complexCOO(this, row, col, val)
      class(COO_), intent(inout) :: this
      integer(int32), intent(in)   :: row
      integer(int32), intent(in)   :: col
      complex(real64), intent(in)   :: val
      integer(int32) :: i, col_id

      if (row > this%DOF() .or. row < 1) return
      if (col > this%DOF() .or. col < 1) return

      if (.not. allocated(this%row)) then
         print *, "ERROR :: initCOO"
         print *, "Please call [COO]%init() before this operation."
         return
      end if

      if (.not. allocated(this%row(row)%val_complex64)) then
         this%row(row)%val_complex64 = [val]
         this%row(row)%col = [col]
      else

         ! check duplication
         if (minval(abs(this%row(row)%col(:) - col)) == 0) then
            ! duplication
            do i = 1, size(this%row(row)%col)
               if (this%row(row)%col(i) == col) then
                  col_id = i
                  exit
               end if
            end do
            this%row(row)%val_complex64(col_id) = this%row(row)%val_complex64(col_id) + val
         else
            this%row(row)%val_complex64 = this%row(row)%val_complex64//[val]
            this%row(row)%col = this%row(row)%col//[col]
         end if
      end if

   end subroutine
! ###############################################

! ###############################################
   function getDenseMatrixCOO(this) result(dense_matrix)
      class(COO_), intent(in) :: this
      real(real64), allocatable :: dense_matrix(:, :)
      integer(int32) :: i, j

      dense_matrix = zeros(size(this%row), size(this%row))

      do i = 1, size(this%row)
         print *, i
         if (allocated(this%row(i)%col)) then
            do j = 1, size(this%row(i)%col)
               dense_matrix(i, this%row(i)%col(j)) = this%row(i)%val(j)
            end do
         end if
      end do

   end function
! ###############################################

! ###############################################
   subroutine removeCOO(this)
      class(COO_), intent(inout) :: this

      if (allocated(this%row)) deallocate (this%row)

   end subroutine
! ###############################################

   function getAllColCOO(this) result(cols)
      class(COO_), intent(in) :: this
      type(COO_ROW_) :: row
      integer(int32), allocatable :: cols(:)
      integer(int32) :: num_total_cols, i

      num_total_cols = 0
      do i = 1, size(this%row)
         if (allocated(this%row(i)%col)) then
            num_total_cols = num_total_cols + size(this%row(i)%col)
         else
            cycle
         end if
      end do
      allocate (cols(num_total_cols))
      cols(:) = 0
      num_total_cols = 0
      do i = 1, size(this%row)
         if (allocated(this%row(i)%col)) then
            cols(num_total_cols + 1:num_total_cols + size(this%row(i)%col)) &
               = this%row(i)%col(1:size(this%row(i)%col))
            num_total_cols = num_total_cols + size(this%row(i)%col)
         else
            cycle
         end if
      end do

   end function

!recursive function getAllCol_as_row_objCOO(this,row_old) result(row_new)
!    class(COO_),intent(in) :: this
!    type(COO_Row_),optional,intent(in) :: row_old(:)
!    type(COO_Row_) :: row_new
!    type(COO_Row_),allocatable :: row_new_buf(:)
!
!    integer(int32) :: num_row,half_row,num_elem
!
!    print *, "hello"
!    if(.not.present(row_old) )then
!        print *, "hello"
!        row_new = this%getAllCol_as_row_obj(this%row)
!
!    else
!        if(size(row_old) ==1 )then
!            if(allocated(row_old(1)%col) )then
!                row_new%col = row_old(1)%col
!            else
!                allocate(row_new%col(0) )
!            endif
!        elseif(size(row_old) ==2 )then
!            if(allocated(row_old(1)%col) .and. allocated(row_old(1)%col) )then
!                row_new%col = row_old(1)%col
!                row_new%col = row_new%col // row_old(2)%col
!            elseif(allocated(row_old(1)%col))then
!                row_new%col = row_old(1)%col
!            elseif(allocated(row_old(2)%col))then
!                row_new%col = row_old(2)%Col
!            else
!                allocate(row_new%col(0) )
!            endif
!        else
!            num_row = size(row_old)
!            half_row = num_row/2
!            print *, half_row, num_row
!            allocate(row_new_buf(2) )
!            row_new_buf(1) = this%getAllCol_as_row_obj(this%row(1:half_row) )
!            row_new_buf(2) = this%getAllCol_as_row_obj(this%row(half_row:) )
!            row_new%col = row_new_buf(1)%col // row_new_buf(2)%col
!        endif
!    endif
!
!end function
!
   subroutine LanczosCRS(this, DiagonalVector, subDiagonalVector, V)
      class(CRS_), intent(in) :: this

      real(real64), allocatable, intent(inout) :: V(:, :)!,T(:,:)
      real(real64), allocatable, intent(inout) :: DiagonalVector(:)
      real(real64), allocatable, intent(inout) :: subDiagonalVector(:)
      real(real64), allocatable :: v1(:), w1(:), vj(:), wj(:)
      real(real64) :: alp1, beta_j, alp_j
      type(Random_) :: random
      integer(int32) :: i, N, j, m

      ! https://en.wikipedia.org/wiki/Lanczos_algorithm

      !N = size(A,1)
      N = size(this%row_ptr) - 1
      V = zeros(N, N)
      !T = zeros(N,N)
      DiagonalVector = zeros(N)
      subDiagonalVector = zeros(N - 1)

      v1 = 2.0d0*random%randn(N) - 1.0d0
      v1 = v1/norm(v1)
      w1 = this%matmul(v1)
      alp1 = dot_product(w1, v1)
      w1 = w1 - alp1*v1
      beta_j = norm(w1)

      !T(1,1) = alp1
      V(:, 1) = v1
      DiagonalVector(1) = alp1

      do i = 2, N
         beta_j = norm(w1)
         if (beta_j /= 0.0d0) then
            vj = w1/beta_j
         else
            vj = 2.0d0*random%randn(N) - 1.0d0
            stop
         end if
         wj = this%matmul(vj)
         alp_j = dot_product(wj, vj)
         wj = wj - alp_j*vj - beta_j*v1
         v1 = vj
         w1 = wj

         !T(i,i) = alp_j
         !T(i,i-1) = beta_j
         !T(i-1,i) = beta_j
         V(:, i) = vj
         DiagonalVector(i) = alp_j
         subDiagonalVector(i - 1) = beta_j
      end do

   end subroutine

   function matmulCRS(CRS, old_vector, run_mode, cache_size) result(new_vector)
      class(CRS_), intent(in) :: CRS
      real(real64), intent(in)  :: Old_vector(:)
      real(real64), allocatable :: new_vector(:)
      integer(int32), optional, intent(in) :: run_mode
      integer(int32) :: mode_id
      integer(int32), optional, intent(in) :: cache_size

      mode_id = input(default=0, option=run_mode)
      if (mode_id == 1) then
         ! elemental mode
         new_vector = crs_matvec_generic_elemental_SparseClass( &
                      CRS_value=CRS%val, &
                      CRS_col=CRS%col_idx, &
                      CRS_row_ptr=CRS%row_ptr, &
                      old_vector=old_vector)
      else
         new_vector = crs_matvec_generic_SparseClass( &
                      CRS_value=CRS%val, &
                      CRS_col=CRS%col_idx, &
                      CRS_row_ptr=CRS%row_ptr, &
                      old_vector=old_vector, &
                      cache_size=cache_size)
      end if
   end function

   function matmul_c_CRS(CRS, old_vector, c_routine) result(new_vector)

      class(CRS_), intent(in) :: CRS
      real(real64), intent(in)  :: Old_vector(:)
      real(real64), allocatable :: new_vector(:)
      character(*), intent(in) :: c_routine
      integer(int64) :: n, col_size

      n = crs%size()
      col_size = size(crs%col_idx)
      allocate (new_vector(crs%size()))
      new_vector(:) = 0.0d0
      call crs_spmv_real64(val=crs%val, row_ptr=crs%row_ptr, &
                           col_idx=crs%col_idx, &
                           old_vector=old_vector, new_vector=new_vector, n=n, col_size=col_size)

   end function

!  function matmul_opencl_CRS(CRS,old_vector,opencl) result(new_vector)
!
!    class(CRS_),intent(in) :: CRS
!    real(real64),intent(in)  :: Old_vector(:)
!    real(real64),allocatable :: new_vector(:)
!    logical,intent(in) :: opencl
!    integer(int64) :: n,col_size
!
!    n = crs%size()
!    col_size=size(crs%col_idx)
!    allocate(new_vector(crs%size() ) )
!    new_vector(:) = 0.0d0
!    call crs_spmv_real64_opencl(val=crs%val,row_ptr=crs%row_ptr,&
!        col_idx=crs%col_idx,&
!        old_vector=old_vector,new_vector=new_vector,n=n,col_size=col_size)
!
!  end function

   function matmul_c_real32_CRS(CRS, old_vector, c_routine) result(new_vector)

      class(CRS_), intent(in) :: CRS
      real(real32), intent(in)  :: Old_vector(:)
      real(real32), allocatable :: new_vector(:)
      character(*), intent(in) :: c_routine
      integer(int64) :: n, col_size

      n = crs%size()
      col_size = size(crs%col_idx)
      allocate (new_vector(crs%size()))
      new_vector(:) = 0.0d0
      call crs_spmv_real32(val=real(crs%val), row_ptr=crs%row_ptr, &
                           col_idx=crs%col_idx, &
                           old_vector=old_vector, new_vector=new_vector, n=n, col_size=col_size)

   end function

   function matmul_real32_CRS(CRS, old_vector) result(new_vector)
      class(CRS_), intent(in) :: CRS
      real(real32), intent(in)  :: Old_vector(:)
      real(real32), allocatable :: new_vector(:)

      if (CRS%dtype /= real32) then
         print *, "ERROR :: matmul_real32_CRS >> vector type is real32, but &
 &            CRS%dtype /= real32. Plase use this%to_real32() before calling this."
         return
      end if

      new_vector = crs_matvec_generic_real32_SparseClass( &
                   CRS_value=CRS%val_real32, &
                   CRS_col=CRS%col_idx, &
                   CRS_row_ptr=CRS%row_ptr, &
                   old_vector=old_vector)

   end function

   function matmul_complex_CRS(CRS, old_vector) result(new_vector)
      class(CRS_), intent(in) :: CRS
      complex(real64), intent(in)  :: Old_vector(:)
      complex(real64), allocatable :: new_vector(:)
      integer(int32) :: n, row
      integer(int64) :: CRS_id

      n = size(CRS%row_ptr) - 1
      if (size(old_vector) /= n) then
         print *, "ERROR crs_matvec :: inconsistent size for old_vector"
         return
      end if

      new_vector = zeros(n)

      ! accerelation
      !$OMP parallel default(shared) private(CRS_id,row)
      !$OMP do reduction(+:new_vector)
      do row = 1, n
         do CRS_id = CRS%row_ptr(row), CRS%row_ptr(row + 1) - 1
            new_vector(row) = new_vector(row) + CRS%val(CRS_id)*old_vector(CRS%col_idx(CRS_id))
         end do
      end do
      !$OMP end do
      !$OMP end parallel

!    new_vector = crs_matvec_generic_complex_SparseClass(&
!      CRS_value= dcmplx(CRS%val),&
!      CRS_col=CRS%col_idx,&
!      CRS_row_ptr=CRS%row_ptr,&
!      old_vector=old_vector)

   end function

   function crs_matvec_generic_SparseClass(CRS_value, CRS_col, CRS_row_ptr, old_vector, &
                                           cache_size) result(new_vector)
      real(real64), intent(in)  :: CRS_value(:), Old_vector(:)
      integeR(int32), intent(in):: CRS_col(:)
      integeR(int64), intent(in):: CRS_row_ptr(:)
      integer(int32), optional, intent(in) :: cache_size

      real(real64), allocatable :: new_vector(:)
      integer(int32) :: i, j, n, gid, lid, row, col
      integer(int64) :: CRS_id

      integer(int64) :: block_idx, num_block, mem_size, num_row_in_block
      integer(int64), allocatable :: row_idx_from(:), row_idx_to(:)

      !> x_i = A_ij b_j

      n = size(CRS_row_ptr) - 1
      if (size(old_vector) /= n) then
         print *, "ERROR crs_matvec :: inconsistent size for old_vector"
         return
      end if

      new_vector = zeros(n)

      ! accerelation
!    do concurrent (row=1:n)
!        new_vector(row) = new_vector(row) + dot_product( &
!            CRS_value(CRS_row_ptr(row):CRS_row_ptr(row+1)-1),  &
!            old_vector(CRS_col(CRS_row_ptr(row):CRS_row_ptr(row+1)-1) )&
!            )
!    enddo

      !v2.0

      !v2.0
!    !if(.not.present(cache_size) )then
      !$OMP parallel default(shared)
      !$OMP do reduction(+:new_vector)
      do row = 1, n
         new_vector(row) = new_vector(row) + dot_product( &
                           CRS_value(CRS_row_ptr(row):CRS_row_ptr(row + 1) - 1), &
                           old_vector(CRS_col(CRS_row_ptr(row):CRS_row_ptr(row + 1) - 1)) &
                           )
      end do
      !$OMP end do
      !$OMP end parallel
!    else
!        ! This does not work
!
!        ! (1) 行列およびベクトルのメモリ消費量を算出する．
!        mem_size = sizeof(CRS_value) + sizeof(CRS_col)+sizeof(CRS_row_ptr)&
!            +sizeof(new_vector)+sizeof(old_vector)
!        ! (2) メモリ消費量/キャッシュサイズ=ブロック数を算出する．
!
!        ! (3) 1~nを，1~k, k+1~2*k, 2k+1~3*k,...とブロックに分割し，
!        ! 　　j番目の開始idxをrow_idx_from(j), 終了idxをrow_idx_to(j)へ格納
!        if(mem_size <= cache_size)then
!            num_block = 1
!            row_idx_from = [1]
!            row_idx_to   = [n]
!
!        else
!            num_block = mem_size/cache_size
!            if(mod(n,num_block)==0 )then
!                allocate(row_idx_from(num_block) )
!                allocate(row_idx_to(num_block) )
!                num_row_in_block = n/num_block
!                do i=1,num_block
!                    row_idx_from(i) = (i-1)*num_row_in_block+1
!                    row_idx_to(i)   = i*num_row_in_block
!                enddo
!            else
!                allocate(row_idx_from(num_block+1) )
!                allocate(row_idx_to(num_block+1) )
!                num_row_in_block = n/num_block
!                do i=1,num_block
!                    row_idx_from(i) = (i-1)*num_row_in_block+1
!                    row_idx_to(i)   = i*num_row_in_block
!                enddo
!                row_idx_from(num_block+1) = num_block*num_row_in_block+1
!                row_idx_to(num_block+1)   = n
!            endif
!        endif
!
!        do block_idx = 1,num_block
!            !$OMP parallel default(shared)
!            !$OMP do reduction(+:new_vector)
!            do row = row_idx_from(block_idx), row_idx_to(block_idx)
!                new_vector(row) = new_vector(row) + dot_product( &
!                    CRS_value(CRS_row_ptr(row):CRS_row_ptr(row+1)-1),  &
!                    old_vector(CRS_col(CRS_row_ptr(row):CRS_row_ptr(row+1)-1) )&
!                    )
!            enddo
!            !$OMP end do
!            !$OMP end parallel
!        enddo
!    endif

      !<v1.0>
!    !$OMP parallel default(shared) private(CRS_id,col)
!    !$OMP do reduction(+:new_vector)
!    do row = 1, n
!        do concurrent(CRS_id=CRS_row_ptr(row):CRS_row_ptr(row+1)-1)
!            new_vector(row) = new_vector(row) + CRS_value(CRS_id)*old_vector(CRS_col(CRS_id))
!        enddo
!    enddo
!    !$OMP end do
!    !$OMP end parallel
!
   end function
! ###################################################################

   function crs_matvec_generic_elemental_SparseClass(CRS_value, CRS_col, CRS_row_ptr, old_vector) result(new_vector)
      real(real64), intent(in)  :: CRS_value(:), Old_vector(:)
      integeR(int32), intent(in):: CRS_col(:)
      integeR(int64), intent(in):: CRS_row_ptr(:)

      real(real64), allocatable :: new_vector(:)
      integer(int32) :: i, j, n, gid, lid, row, col
      integer(int64) :: CRS_id

      !> x_i = A_ij b_j

      n = size(CRS_row_ptr) - 1
      if (size(old_vector) /= n) then
         print *, "ERROR crs_matvec :: inconsistent size for old_vector"
         return
      end if

      new_vector = zeros(n)

      ! accerelation
!    do concurrent (row=1:n)
!        new_vector(row) = new_vector(row) + dot_product( &
!            CRS_value(CRS_row_ptr(row):CRS_row_ptr(row+1)-1),  &
!            old_vector(CRS_col(CRS_row_ptr(row):CRS_row_ptr(row+1)-1) )&
!            )
!    enddo

      !v2.0

      !v2.0

      !$OMP parallel default(shared)
      !$OMP do reduction(+:new_vector)
      do row = 1, n
         new_vector(row) = new_vector(row) + dot_product( &
                           CRS_value(CRS_row_ptr(row):CRS_row_ptr(row + 1) - 1), &
                           old_vector(CRS_col(CRS_row_ptr(row):CRS_row_ptr(row + 1) - 1)) &
                           )
      end do
      !$OMP end do
      !$OMP end parallel

      !<v1.0>
!    !$OMP parallel default(shared) private(CRS_id,col)
!    !$OMP do reduction(+:new_vector)
!    do row = 1, n
!        do concurrent(CRS_id=CRS_row_ptr(row):CRS_row_ptr(row+1)-1)
!            new_vector(row) = new_vector(row) + CRS_value(CRS_id)*old_vector(CRS_col(CRS_id))
!        enddo
!    enddo
!    !$OMP end do
!    !$OMP end parallel
!
   end function
! ###################################################################

   subroutine spmv_as_subroutine_CRS(this, old_vector, new_vector)
      class(CRS_), intent(in) :: this
      real(real64), intent(in)  :: old_vector(:)
      real(real64), intent(out) :: new_vector(:)
      integer(int32) :: row
      new_vector(:) = 0.0d0

      !$OMP parallel default(shared)
      !$OMP do reduction(+:new_vector)
      do row = 1, size(new_vector)
         new_vector(row) = new_vector(row) + dot_product( &
                           this%val(this%row_ptr(row):this%row_ptr(row + 1) - 1), &
                           old_vector(this%col_idx(this%row_ptr(row):this%row_ptr(row + 1) - 1)) &
                           )
      end do
      !$OMP end do
      !$OMP end parallel

   end subroutine

   function crs_matvec_generic_real32_SparseClass(CRS_value, CRS_col, CRS_row_ptr, old_vector) result(new_vector)
      real(real32), intent(in)  :: CRS_value(:), Old_vector(:)
      integeR(int32), intent(in):: CRS_col(:)
      integeR(int64), intent(in):: CRS_row_ptr(:)

      real(real32), allocatable :: new_vector(:)
      integer(int32) :: i, j, n, gid, lid, row, col
      integer(int64) :: CRS_id

      !> x_i = A_ij b_j

      n = size(CRS_row_ptr) - 1
      if (size(old_vector) /= n) then
         print *, "ERROR crs_matvec :: inconsistent size for old_vector"
         return
      end if

      new_vector = zeros(n)

      ! accerelation
!    do concurrent (row=1:n)
!        new_vector(row) = new_vector(row) + dot_product( &
!            CRS_value(CRS_row_ptr(row):CRS_row_ptr(row+1)-1),  &
!            old_vector(CRS_col(CRS_row_ptr(row):CRS_row_ptr(row+1)-1) )&
!            )
!    enddo

      !v2.0
      !$OMP parallel default(shared)
      !$OMP do reduction(+:new_vector)
      do row = 1, n
         new_vector(row) = new_vector(row) + dot_product( &
                           CRS_value(CRS_row_ptr(row):CRS_row_ptr(row + 1) - 1), &
                           old_vector(CRS_col(CRS_row_ptr(row):CRS_row_ptr(row + 1) - 1)) &
                           )
      end do
      !$OMP end do
      !$OMP end parallel

      !<v1.0>
!    !$OMP parallel default(shared) private(CRS_id,col)
!    !$OMP do reduction(+:new_vector)
!    do row = 1, n
!        do concurrent(CRS_id=CRS_row_ptr(row):CRS_row_ptr(row+1)-1)
!            new_vector(row) = new_vector(row) + CRS_value(CRS_id)*old_vector(CRS_col(CRS_id))
!        enddo
!    enddo
!    !$OMP end do
!    !$OMP end parallel
!
   end function
! ###################################################################

!function crs_opencl_matvec(CRS_row_ptr,CRS_col,CRS_value,old_vector) result(new_vector)
!    integer(int32), intent(in)  :: CRS_row_ptr(:),CRS_col(:)
!    real(real64),intent(in) :: CRS_value(:), old_vector(:)
!
!
!end function
! ###################################################################

   function crs_matvec_generic_complex_SparseClass(CRS_value, CRS_col, CRS_row_ptr, old_vector) result(new_vector)
      complex(real64), intent(in)  :: CRS_value(:), Old_vector(:)
      integeR(int32), intent(in):: CRS_col(:)
      integeR(int64), intent(in):: CRS_row_ptr(:)

      complex(real64), allocatable :: new_vector(:)
      integer(int32) :: i, j, n, gid, lid, row, col
      integer(int64) :: CRS_id
      !> x_i = A_ij b_j

      n = size(CRS_row_ptr) - 1
      if (size(old_vector) /= n) then
         print *, "ERROR crs_matvec :: inconsistent size for old_vector"
         return
      end if

      new_vector = zeros(n)

! accerelation

      !$OMP parallel default(shared) private(CRS_id,row,col)
      !$OMP do reduction(+:new_vector)
      do row = 1, n
         do CRS_id = CRS_row_ptr(row), CRS_row_ptr(row + 1) - 1
            new_vector(row) = new_vector(row) + CRS_value(CRS_id)*old_vector(CRS_col(CRS_id))
         end do
      end do
      !$OMP end do
      !$OMP end parallel

!    !$OMP parallel do default(shared) private(CRS_id,col)
!    do row = 1 , n
!        do CRS_id = CRS_row_ptr(row), CRS_row_ptr(row+1)-1
!            col = CRS_col(CRS_id)
!            !$OMP atomic
!            new_vector(row) = new_vector(row) + CRS_value(CRS_id)*old_vector(col)
!        enddo
!    enddo
!    !$OMP end parallel do

   end function
! ###################################################################
!function crs_opencl_matvec(CRS_row_ptr,CRS_col,CRS_value,old_vector) result(new_vector)
!    integer(int32), intent(in)  :: CRS_row_ptr(:),CRS_col(:)
!    real(real64),intent(in) :: CRS_value(:), old_vector(:)
!
!
!end function
! ###################################################################

   subroutine eigCRS(this, Eigen_vectors, eigen_values)
      class(CRS_), intent(in) :: this
      real(real64), allocatable :: A(:, :), V(:, :)
      real(real64), allocatable :: D(:), E(:)
      real(real64), allocatable :: WORK(:)
      integer(int32), allocatable :: IWORK(:), IFAIL(:)
      real(real64), allocatable, intent(inout) :: Eigen_vectors(:, :), eigen_values(:)
      integer(int32) :: num_eigen, INFO
      real(real64)  :: VL, VU, ABSTOL
      integer(int32):: IL, IU
      character(1) :: JOBZ = "V"
      character(1) :: RANGE = "A"

      call this%Lanczos(DiagonalVector=D, subDiagonalVector=E, V=V)

      !call print("--")
      !call print(V)

      num_eigen = size(D)
      eigen_values = zeros(size(D))
      eigen_vectors = zeros(size(D), num_eigen)
      WORK = zeros(5*size(D))
      IWORK = int(zeros(5*size(D)))
      IFAIL = int(zeros(size(D)))
      ABSTOL = dble(1.0e-14)

      ! LAPACK
      call DSTEVX(JOBZ, RANGE, size(D), &
                  D, E, VL, VU, IL, IU, ABSTOL, &
                  num_eigen, eigen_values, eigen_vectors, size(D), &
                  WORK, IWORK, IFAIL, INFO)

      !eigen_Vectors = matmul(V,matmul(eigen_values, transpose(V) ) )
      eigen_Vectors = matmul(V, eigen_vectors)
      do i_i = 1, 3
         eigen_vectors(:, i_i) = eigen_vectors(:, i_i)/norm(eigen_vectors(:, i_i))
         eigen_vectors(:, i_i) = eigen_vectors(:, i_i)/eigen_vectors(3, i_i)
      end do

   end subroutine

   subroutine initCRS(this, val, col_idx, row_ptr)
      class(CRS_), intent(inout) :: this
      integer(int32), intent(in) :: col_idx(:)
      integer(int64), intent(in) ::  row_ptr(:)
      real(real64), intent(in)   :: val(:)

      this%val = val
      this%col_idx = col_idx
      this%row_ptr = row_ptr

   end subroutine

   function to_denseCRS(this) result(dense_mat)
      class(CRS_), intent(in) :: this
      real(real64), allocatable  :: dense_mat(:, :)
      integer(int32) :: i, n, row, col
      integeR(int64) :: j

      n = size(this%row_ptr) - 1
      allocate (dense_mat(n, n))
      dense_mat(:, :) = 0.0d0
      do row = 1, n
         do j = this%row_ptr(row), this%row_ptr(row + 1) - 1
            col = this%col_idx(j)
            dense_mat(row, col) = this%val(j)
         end do
      end do

   end function

   pure function addCRS_and_CRS(CRS1, CRS2) result(CRS_ret)
      type(CRS_), intent(in) :: CRS1, CRS2
      type(CRS_) :: CRS_ret
      integer(int32) :: i, j, row
      integer(int64) :: col_2, col_1

      ! sum : CRS_ret = CRS1 + CRS2
      CRS_ret = CRS1

      ! ignore fill-in
    !!$OMP parallel do private(col_2,col_1)
      do row = 1, size(CRS2%row_ptr) - 1
         ! for each row
         do col_2 = CRS2%row_ptr(row), CRS2%row_ptr(row + 1) - 1
            ! search same col
            do col_1 = CRS1%row_ptr(row), CRS1%row_ptr(row + 1) - 1
               if (CRS1%col_idx(col_1) == CRS2%col_idx(col_2)) then
                    !!$OMP atomic
                  CRS_ret%val(col_1) = CRS_ret%val(col_1) + CRS2%val(col_2)
                  exit
               end if
            end do
         end do
      end do
    !!$OMP end parallel do

   end function

   pure function diffCRS_and_CRS(CRS1, CRS2) result(CRS_ret)
      type(CRS_), intent(in) :: CRS1, CRS2
      type(CRS_) :: CRS_ret
      integer(int32) :: i, j, row
      integer(int64) :: col_2, col_1

      ! sum : CRS_ret = CRS1 + CRS2
      CRS_ret = CRS1

      ! ignore fill-in
      do row = 1, size(CRS2%row_ptr) - 1
         ! for each row
         do col_2 = CRS2%row_ptr(row), CRS2%row_ptr(row + 1) - 1
            ! search same col
            do col_1 = CRS1%row_ptr(row), CRS1%row_ptr(row + 1) - 1
               if (CRS1%col_idx(col_1) == CRS2%col_idx(col_2)) then
                  CRS_ret%val(col_1) = CRS_ret%val(col_1) - CRS2%val(col_2)
                  exit
               end if
            end do
         end do
      end do

   end function

   function multReal64_and_CRS(scalar64, CRS1) result(CRS_ret)
      real(real64), intent(in) :: scalar64
      type(CRS_), intent(in) :: CRS1
      type(CRS_) :: CRS_ret
      integer(int32) :: i

      CRS_ret = CRS1
      !$OMP parallel do
      do i = 1, size(CRS_ret%val)
         CRS_ret%val(i) = scalar64*CRS_ret%val(i)
      end do
      !$OMP end parallel do

   end function

   function multCRS_and_Real64(CRS1, scalar64) result(CRS_ret)
      type(CRS_), intent(in) :: CRS1
      real(real64), intent(in) :: scalar64
      type(CRS_) :: CRS_ret
      integer(int32)::i

      CRS_ret = CRS1

      !$OMP parallel do
      do i = 1, size(CRS_ret%val)
         CRS_ret%val = scalar64*CRS_ret%val
      end do
      !$OMP end parallel do
   end function

   pure function DOFCOO(this) result(Degree_of_freedom)
      class(COO_), intent(in) :: this
      integer(int32) :: Degree_of_freedom

      if (allocated(this%row)) then
         Degree_of_freedom = size(this%row)
      else
         Degree_of_freedom = 0
      end if
   end function

   pure function DOFCRS(this) result(Degree_of_freedom)
      class(CRS_), intent(in) :: this
      integer(int32) :: Degree_of_freedom

      if (allocated(this%row_ptr)) then
         Degree_of_freedom = size(this%row_ptr) - 1
      else
         Degree_of_freedom = 0
      end if
   end function

! #######################################################
   pure function getCOO(this, row, col) result(ret)
      class(COO_), intent(in) :: this
      integer(int32), intent(in) :: row, col
      real(real64) :: ret
      integer(int32) :: i

      ret = 0.0d0
      if (.not. allocated(this%row(row)%col)) then
         return
      end if

      do i = 1, size(this%row(row)%col, 1)
         if (this%row(row)%col(i) == col) then
            ret = this%row(row)%val(i)
         end if
      end do

   end function

   pure function neCOO(this) result(ret)
      class(COO_), intent(in) :: this
      real(real64) :: ret
      integer(int32) :: i

      ! get number of entity
      ret = 0.0d0
      do i = 1, size(this%row)
         if (allocated(this%row(i)%col)) then
            ret = ret + size(this%row(i)%col)
         end if
      end do

   end function

   pure function maxvalCOO(this) result(ret)
      class(COO_), intent(in) :: this
      real(real64) :: ret
      real(real64), allocatable :: val(:)
      integer(int32) :: i, itr

      ! get number of entity
      ret = 0.0d0
      itr = 0
      do i = 1, size(this%row)
         if (allocated(this%row(i)%col)) then
            if (itr == 0) then
               val = this%row(i)%val(:)
               ret = maxval(val)
               itr = itr + 1
            else
               val = this%row(i)%val(:)
               if (ret < maxval(val)) then
                  ret = maxval(val)
               else
                  cycle
               end if
            end if
         end if
      end do

   end function

! ##################################################
   function sizeCRS(this) result(n)
      class(CRS_), intent(in) :: this
      integer(int32) :: n

      if (allocated(this%row_ptr)) then
         n = size(this%row_ptr) - 1
      else
         n = 0
      end if

   end function
! ##################################################

! ##################################################
   subroutine updateCRS(this, row, col, val)
      class(CRS_), intent(inout) :: this
      integer(int32), intent(in) :: row, col
      real(real64), intent(in) :: val
      integer(int64) :: i, j

      ! update but ignore fill-in
      if (col > maxval(this%col_idx(this%row_ptr(row):this%row_ptr(row + 1) - 1))) return
      if (col < minval(this%col_idx(this%row_ptr(row):this%row_ptr(row + 1) - 1))) return

      do i = this%row_ptr(row), this%row_ptr(row + 1) - 1
         if (this%col_idx(i) == col) then
            this%val(i) = val
            return
         end if
      end do

   end subroutine
! ##################################################

! ##################################################
   subroutine addCRS(this, row, col, val)
      class(CRS_), intent(inout) :: this
      integer(int32), intent(in) :: row, col
      real(real64), intent(in) :: val
      integer(int64) :: i, j

      ! update but ignore fill-in
      if (col > maxval(this%col_idx(this%row_ptr(row):this%row_ptr(row + 1) - 1))) return
      if (col < minval(this%col_idx(this%row_ptr(row):this%row_ptr(row + 1) - 1))) return

      do i = this%row_ptr(row), this%row_ptr(row + 1) - 1
         if (this%col_idx(i) == col) then
            this%val(i) = this%val(i) + val
            return
         end if
      end do

   end subroutine
! ##################################################

! ##################################################
   function getCRS(this, row, col) result(val)
      class(CRS_), intent(in) :: this
      integer(int32), intent(in) :: row, col
      real(real64) :: val
      integer(int64) :: i

      ! update but ignore fill-in
      val = 0.0d0
      if (col > maxval(this%col_idx(this%row_ptr(row):this%row_ptr(row + 1) - 1))) return

      if (col < minval(this%col_idx(this%row_ptr(row):this%row_ptr(row + 1) - 1))) return

      do i = this%row_ptr(row), this%row_ptr(row + 1) - 1
         if (this%col_idx(i) == col) then
            val = this%val(i)
            return
         end if
      end do

   end function
! ##################################################

! ##################################################
   logical function is_nonzeroCRS(this, row, col)
      class(CRS_), intent(in) :: this
      integer(int32), intent(in) :: row, col
      real(real64) :: val
      integer(int64) :: i

      is_nonzeroCRS = .false.
      if (row > this%size()) return

      !if (col > maxval(this%col_idx(this%row_ptr(row):this%row_ptr(row+1)-1))  ) return

      !if (col < minval(this%col_idx(this%row_ptr(row):this%row_ptr(row+1)-1))  ) return

      do i = this%row_ptr(row), this%row_ptr(row + 1) - 1
         if (this%col_idx(i) == col) then
            is_nonzeroCRS = .true.
            return
         end if
      end do

   end function
! ##################################################

   function lumpedCRS(this) result(ret)
      class(CRS_), intent(in) :: this
      real(real64), allocatable  :: ret(:)

      ret = this%diag(cell_centered=.true.)

   end function

! ##################################################
   function diagCRS(this, cell_centered) result(diag_vec)
      class(CRS_), intent(in) :: this
      real(real64), allocatable  :: diag_vec(:)
      logical, optional, intent(in) :: cell_centered
      integeR(int64) :: i, j

      if (present(cell_centered)) then
         if (cell_centered) then

            diag_vec = zeros(this%size())
            do i = 1, this%size() ! for rows
               do j = this%row_ptr(i), this%row_ptr(i + 1) - 1 ! for columns
                  diag_vec(i) = diag_vec(i) + this%val(j)
               end do
            end do
            return
         end if
      end if

      diag_vec = zeros(this%size())
      do i = 1, this%size()
         do j = this%row_ptr(i), this%row_ptr(i + 1) - 1
            if (this%col_idx(j) == i) then
               diag_vec(i) = this%val(j)
               exit
            end if
         end do
      end do

   end function
! ##################################################

   recursive subroutine ILUCRS(this, fill_in_order, RHS, debug)
      class(CRS_), intent(inout) :: this
      integer(int32), intent(in) :: fill_in_order !
      real(real64), optional, intent(inout)   :: RHS(:) ! RHS vector
      real(real64), allocatable  :: diag_vec(:)
      integer(int32), allocatable  :: col_line(:), range_col(:, :)
      type(CCS_) :: ccs
      logical, optional, intent(in) :: debug
      real(real64) :: A_k_j, A_ik
      integer(int32) :: i, k, l, n, m, row, col, col_idx, row_idx, kk, jj
      integer(int64) :: j
      integer(int32), allocatable :: nonzero_k_list(:), nonzero_j_list(:)
      logical :: debug_mode_on = .false.

      if (present(debug)) then
         debug_mode_on = debug
      end if

      if (present(RHS)) then
         ! Given: Ax = y
         ! A = LU
         call this%ILU(fill_in_order)

         ! Forward substitution
         do i = 2, this%size()
            do j = this%row_ptr(i), this%row_ptr(i + 1) - 1
               if (this%col_idx(j) < i) then
                  ! execute Forward substitution
                  !print *, i, this%col_idx(j),this%val(j)
                  RHS(i) = RHS(i) - this%val(j)*RHS(this%col_idx(j))
               end if
            end do
         end do

         ! Backward substitution
         diag_vec = this%diag()
         do i = this%size(), 1, -1
            if (diag_vec(i) == 0.0d0) cycle
            RHS(i) = RHS(i)/diag_vec(i)
         end do

         ! Forward substitution
         do i = this%size(), 1, -1
            do j = this%row_ptr(i), this%row_ptr(i + 1) - 1
               if (this%col_idx(j) > i) then
                  ! execute Forward substitution
                  RHS(i) = RHS(i) - this%val(j)/diag_vec(i)*RHS(this%col_idx(j))
               end if
            end do
         end do

         return
      end if

      select case (fill_in_order)
      case default
         print *, "fill-in order", fill_in_order, "is not implemented. Try 0"
         stop
      case (0)
         n = this%size()
         if (debug_mode_on) then
            print *, "[ILU(0)] >> started"
         end if
         diag_vec = this%diag()

         range_col = zeros(n, 2)

            !!$OMP parallel do
         !do row=1,n
         !    range_col(row,1) = minval(this%col_idx(this%row_ptr(row):this%row_ptr(row+1)-1))
         !    range_col(row,2) = maxval(this%col_idx(this%row_ptr(row):this%row_ptr(row+1)-1))
         !enddo
            !!$OMP end parallel do

         do i = 2, n
            if (debug_mode_on) then
               print *, "[ILU(0)] >> U", i, "/", n
            end if
            ! >>>>>>> slow
            nonzero_k_list = this%col_idx(this%row_ptr(i):this%row_ptr(i + 1) - 1)
                !! >> notice!!
                !! each col should be sorted.

            !do k = range_col(i,1) , i-1
            do kk = 1, size(nonzero_k_list)
               k = nonzero_k_list(kk)

               if (k >= i) cycle

               A_ik = this%get(i, k)/diag_vec(k)
               call this%update(row=i, col=k, val=A_ik)

               !$OMP parallel default(shared) private(j)
               !$OMP do
               do jj = 1, size(nonzero_k_list)
                  j = nonzero_k_list(jj)
                  if (j < k + 1) cycle
                  call this%add(row=int(i), col=int(j), val=-A_ik*this%get(int(k), int(j)))
               end do
               !$OMP end do
               !$OMP end parallel

               diag_vec(i) = this%get(int(i), int(i))

            end do
         end do
         ! <<<<<<<<< slow

!            !! ccs
         !ccs = this%to_CCS()

         ! [5,  6,  7]
         ! [2,  8,  9]
         ! [3,  4, 10]

         ! [   5,  6,  7]
         ! [10/5, 20, 23]
         ! [15/5, 50, 67]

!!          ! 高速バージョン

!            do i=2,n
!                if(debug_mode_on)then
!                    print *, "[ILU(0)] >> U",i,"/",n
!                endif
!                ! >>>>>>> slow
!                k = i-1
!                do row_idx = ccs%col_ptr(i-1),ccs%col_ptr(i)-1
!                    if(ccs%row_idx(row_idx) < i ) cycle
!                    if(i==3 ) then
!                        print *,"dbg",ccs%row_idx(row_idx),i-1,ccs%val(row_idx), diag_vec(i-1)
!
!                    endif
!
!                    ccs%val(row_idx) = ccs%val(row_idx)/diag_vec(i-1)
!                    if(ccs%row_idx(row_idx) == i ) then
!                        A_ik = ccs%val(row_idx)
!                    endif
!                enddo
!                if(i==3 ) stop
!
!                do j = this%row_ptr(i),this%row_ptr(i+1)-1
!                    if(this%col_idx(j) < i ) then
!                        cycle
!                    elseif(this%col_idx(j) == i ) then
!                        diag_vec(i) = diag_vec(i) - A_ik*this%get( k,i )
!                        call this%update(i,i,diag_vec(i) )
!                    else
!                        call this%update(row=i,col= this%col_idx(j) ,&
!                            val=this%get(i,this%col_idx(j) ) - A_ik*this%get(k,this%col_idx(j) ) )
!                    endif
!                enddo
!
!            enddo
!            ! [   5,  6,  7]
!            ! [10/5, 20, 23]
!            ! [15/5, 50, 67]
!            call this%load(ccs=ccs,position="L")
!            return

!
      end select

   end subroutine

! ################################################
   subroutine ILU_matvecCRS(this, old_vector, new_vector)
      class(CRS_), intent(in) :: this ! ILU factorlized matrix
      integer(int64) :: i, j
      real(real64), intent(in) :: old_vector(:)
      real(real64), allocatable, intent(inout) :: new_vector(:)
      real(real64), allocatable :: diag_vec(:)
      real(real64) :: new_vector_i
      new_vector = old_vector

      ! Forward substitution
      do i = 2, this%size()
         new_vector_i = new_vector(i)
         !$OMP parallel
         !$OMP do reduction(+:new_vector_i)
         do j = this%row_ptr(i), this%row_ptr(i + 1) - 1
            if (this%col_idx(j) < i) then
               ! execute Forward substitution
               new_vector_i = new_vector_i - this%val(j)*new_vector(this%col_idx(j))
            end if
         end do
         !$OMP end do
         !$OMP end parallel
         new_vector(i) = new_vector_i
      end do
      ! Backward substitution
      diag_vec = this%diag()

      !$OMP parallel
      !$OMP do
      do i = this%size(), 1, -1
         if (diag_vec(i) == 0.0d0) then
            diag_vec(i) = 1.0d0
         end if
         new_vector(i) = new_vector(i)/diag_vec(i)
      end do
      !$OMP end do
      !$OMP end parallel

      ! Forward substitution
      do i = this%size(), 1, -1

         new_vector_i = new_vector(i)
         !$OMP parallel
         !$OMP do reduction(+:new_vector_i)
         do j = this%row_ptr(i), this%row_ptr(i + 1) - 1
            if (this%col_idx(j) > i) then
               ! execute Forward substitution
               new_vector(i) = new_vector(i) - this%val(j)/diag_vec(i)*new_vector(this%col_idx(j))
            end if
         end do
         !$OMP end do
         !$OMP end parallel
         new_vector(i) = new_vector_i

      end do

   end subroutine
! ##################################################

   function to_CCSCRS(this) result(CCS)
      class(CRS_), intent(in) :: this
      type(CCS_) :: CCS
      integer(int64) :: i, j, n, col
      integer(int32), allocatable :: inst_counter(:)

      inst_counter = int(zeros(this%size()))
      CCS%col_ptr = int(zeros(size(this%row_ptr)))
      CCS%row_idx = int(zeros(size(this%col_idx)))
      CCS%val = zeros(size(this%val))

      do i = 1, size(this%col_idx)
         CCS%col_ptr(this%col_idx(i)) = CCS%col_ptr(this%col_idx(i)) + 1
      end do

      ![2,3,3, 3, 2, 0]
      do i = 1, size(CCS%col_ptr) - 1
         CCS%col_ptr(i + 1) = CCS%col_ptr(i + 1) + CCS%col_ptr(i)
      end do
      ! [2,3,3, 3, 2,  0]
      !>[2,5,8,11,13, 13]
      do i = size(CCS%col_ptr), 2, -1
         CCS%col_ptr(i) = CCS%col_ptr(i - 1)
      end do
      ! [2,5,8,11,13, 13]
      !>[2,2,5, 8,11, 13]
      CCS%col_ptr(:) = CCS%col_ptr(:) + 1

      ! [2,2,5, 8,11, 13]
      !>[2,3,6, 9,12, 14]
      CCS%col_ptr(1) = 1
      ! [2,3,6, 9,12, 14]
      !>[1,3,6, 9,12, 14]

      do i = 1, size(this%row_ptr) - 1
         do j = this%row_ptr(i), this%row_ptr(i + 1) - 1
            col = this%col_idx(j)
            CCS%row_idx(CCS%col_ptr(col) + inst_counter(col)) = i
            CCS%val(CCS%col_ptr(col) + inst_counter(col)) = this%val(j)
            inst_counter(col) = inst_counter(col) + 1
         end do
      end do

   end function
!####################################################
   subroutine loadCRS(this, CCS, Position)
      class(CRS_), intent(inout) :: this
      type(CCS_), optional, intent(in) :: CCS
      character(*), optional, intent(in) :: Position
      integer(int32) :: i, j

      if (present(CCS)) then
         if (Position == "L") then
            print *, CCS%col_ptr
            print *, CCS%row_idx
            print *, CCS%val

            do i = 1, size(CCS%col_ptr) - 1
               do j = ccs%col_ptr(i), ccs%col_ptr(i + 1) - 1

                  if (ccs%row_idx(j) <= i) then
                     cycle
                  else
                     print *, ccs%row_idx(j), i
                     call this%update(row=ccs%row_idx(j), col=i, val=ccs%val(j))
                  end if
               end do
            end do
         end if
      end if
   end subroutine

   subroutine eyesCOO(this, n)
      class(COO_) :: this
      integer(int32), intent(in) :: n
      integer(int32) :: i

      call this%init(n)
      do i = 1, n
         call this%set(i, i, 1.0d0)
      end do

   end subroutine

   subroutine poissonCOO(this, n)
      class(COO_) :: this
      integer(int32), intent(in) :: n
      integer(int32) :: i

      call this%init(n)
      call this%set(1, 1, -2.0d0)
      call this%set(1, 1 + 1, 1.0d0)
      do i = 2, n - 1
         call this%set(i, i - 1, 1.0d0)
         call this%set(i, i, -4.0d0)
         call this%set(i, i + 1, 1.0d0)
      end do
      call this%set(n, n - 1, 1.0d0)
      call this%set(n, n, -2.0d0)

   end subroutine

   subroutine LOBPCG_single_CRS(A, B, lambda, X, alpha, tol, debug)
      type(CRS_), intent(in) :: A, B
      real(real64), allocatable, intent(inout) :: X(:)
      real(real64), allocatable, intent(inout) :: lambda
      real(real64), intent(in) :: alpha
      logical, optional, intent(in) :: debug
      logical :: debug_mode

      type(Random_) :: random

      real(real64), allocatable :: r(:), rho
      integer(int32) :: n, i
      integer(int32) :: MAX_ITR = 1000000
      real(real64), intent(in) :: tol

      debug_mode = input(default=.false., option=debug)
      ! BLOPEX IN HYPRE AND PETSC, SIAM Eq. (2.2)
      ! number of eigen value :: m
      ! initialize X and lambda
      n = A%size()
      X = 2.0d0*random%randn(n) - 1.0d0

      lambda = 0.0d0

      !Single-vector version
      do i = 1, MAX_ITR
         rho = dot_product(x, A%matmul(x))/dot_product(x, B%matmul(x))
         r = A%matmul(x) - rho*B%matmul(x)
         x = x - alpha*r
         if (debug_mode) then
            print *, i, norm(r)
         end if
         if (norm(r) < tol) then
            if (debug_mode) then
               print *, "[OK] converged."
            end if
            exit
         else
            cycle
         end if
      end do
      if (i == MAX_ITR) print *, "[ERROR] LOBPCG NOT CONVERGED."
      lambda = rho
   end subroutine

   subroutine LOBPCG_CRS(A, B, lambda, X, m, MAX_ITR, TOL, debug)
      type(CRS_), intent(in) :: A, B
      real(real64), allocatable, intent(out) :: X(:, :)
      real(real64), allocatable, intent(out) :: lambda(:)
      real(real64), intent(in) :: TOL
      integer(int32), intent(in) :: m, MAX_ITR
      logical, optional, intent(in) :: debug
      logical :: debug_mode

      type(Random_) :: random

      real(real64), allocatable :: r(:, :), p(:, :), V(:, :), A_small(:, :), AV(:, :), &
                                   b_small(:, :), lambda_small(:), XAX(:, :), AX(:, :), OR(:, :), BX(:, :), XBX(:, :), &
                                   Bm_small(:, :), BV(:, :)
      real(real64)   :: initial_R
      integer(int32) :: n, i, j, k
      !integer(int32) :: MAX_ITR = 1000000

      ! References:
      ! Knyazev, A. V., Argentati, M. E., Lashuk, I. & Ovtchinnikov, E. E. Block Locally Optimal Preconditioned Eigenvalue Xolvers (BLOPEX) in hypre and PETSc. SIAM J. Sci. Comput. 29, 2224–2239 (2007).
      ! https://en.wikipedia.org/wiki/LOBPCG
      ! https://webpark1378.sakura.ne.jp/nagai/note_141009_LOBPCG.pdf

      debug_mode = input(default=.false., option=debug)
      ! BLOPEX IN HYPRE AND PETSC, SIAM Eq. (2.2)
      ! number of eigen value :: m

      ! debug

      ! initialize X and lambda
      n = A%size()

      !X = random%randn(n,m)
      X = random%randn(n, m)! + random%randn(n,m) !+ random%randn(n,m)
      do i = 1, m
         X(:, i) = X(:, i)/norm(X(:, i))
      end do

      call GramSchmidt(X, size(X, 1), size(X, 2), X)

      lambda = zeros(m)

      R = zeros(n, m)
      P = zeros(n, m)
      AV = zeros(n, 3*m)
      AX = zeros(n, m)
      A_small = zeros(3*m, 3*m)
      V = zeros(n, 3*m)

      ! m x m 固有値問題

      do i = 1, m
         AX(:, i) = A%matmul(X(:, i))
      end do

      BX = zeros(size(AX, 1), size(AX, 2))
      do i = 1, m
         BX(:, i) = B%matmul(X(:, i))
      end do

      XAX = matmul(transpose(X(:, 1:m)), AX)
      XBX = matmul(transpose(X(:, 1:m)), BX)
      call LAPACK_EIG(XAX, XBX, b_small, lambda_small)
      X = matmul(X, b_small)

      do i = 1, m
         AX(:, i) = A%matmul(X(:, i))
      end do
      BX = zeros(size(AX, 1), size(AX, 2))
      do i = 1, m
         BX(:, i) = B%matmul(X(:, i))
      end do

      do i = 1, m
         R(:, i) = AX(:, i) - BX(:, i)*lambda_small(i)
      end do

      V = zeros(n, 2*m)

      V(:, 1:m) = X(:, :)
      V(:, m + 1:2*m) = R(:, :)

      call GramSchmidt(V, size(V, 1), size(V, 2), V)
      do k = 1, size(X, 2)
         V(:, k) = V(:, k)/norm(V(:, k))
      end do

      X(:, :) = V(:, 1:m)
      R(:, :) = V(:, m + 1:2*m)

      AV = zeros(n, 2*m)
      do j = 1, 2*m  ! n x n, n x 2m
         AV(:, j) = A%matmul(V(:, j))
      end do

      BV = zeros(n, 2*m)
      do j = 1, 2*m  ! n x n, n x 2m
         BV(:, j) = B%matmul(V(:, j))
      end do

      A_small = matmul(transpose(V), AV)
      Bm_small = matmul(transpose(V), BV)

      call LAPACK_EIG(A_small, Bm_small, b_small, lambda_small)

      do j = 1, m
         X(:, j) = matmul(V, b_small(:, j))
      end do
      AX = zeros(n, m)
      do i = 1, m
         AX(:, i) = A%matmul(X(:, i))
      end do

      BX = zeros(size(AX, 1), size(AX, 2))
      do j = 1, m
         BX(:, j) = B%matmul(X(:, j))
      end do

      R = zeros(n, m)
      do j = 1, m
         R(:, j) = AX(:, j) - BX(:, j)*lambda_small(j)
      end do

      OR = zeros(n, 2*m)
      OR(:, 1:m) = 0.0d0
      OR(:, m + 1:2*m) = R(:, :)

      P = zeros(n, m)
      do i = 1, m
         P(:, i) = matmul(OR, b_small(:, i))
      end do

      V = zeros(n, 3*m)
      AV = zeros(n, 3*m)
      BV = zeros(n, 3*m)

      AX = zeros(n, m)
      BX = zeros(n, m)

      do i = 1, MAX_ITR
         V(:, 1:m) = X(:, :)
         V(:, m + 1:2*m) = R(:, :)
         V(:, 2*m + 1:3*m) = P(:, :)

         ! Gram-Scmidtを計算する．
         call GramSchmidt(V, size(V, 1), size(V, 2), V)
         do k = 1, size(X, 2)
            V(:, k) = V(:, k)/norm(V(:, k))
         end do
         do k = size(X, 2) + size(R, 2) + 1, size(V, 2)
            V(:, k) = V(:, k)/norm(V(:, k))
         end do

         X(:, :) = V(:, 1:m)
         R(:, :) = V(:, m + 1:2*m)
         P(:, :) = V(:, 2*m + 1:3*m)

         !$OMP parallel do
         do j = 1, 3*m
            AV(:, j) = A%matmul(V(:, j))
         end do
         !$OMP end parallel do

         !$OMP parallel do
         do j = 1, 3*m
            BV(:, j) = B%matmul(V(:, j))
         end do
         !$OMP end parallel do

         A_small = matmul(transpose(V), AV)
         Bm_small = matmul(transpose(V), BV)

         ! LAPACKの固有値求めるやつを呼ぶ
         call LAPACK_EIG(A_small, Bm_small, b_small, lambda_small)

         X = matmul(V, b_small(:, 1:m))

         !$OMP parallel do
         do j = 1, m
            AX(:, j) = A%matmul(X(:, j))
         end do
         !$OMP end parallel do

         !$OMP parallel do
         do j = 1, m
            BX(:, j) = B%matmul(X(:, j))
         end do
         !$OMP end parallel do

         !$OMP parallel do
         do j = 1, m
            R(:, j) = AX(:, j) - BX(:, j)*lambda_small(j)
         end do
         !$OMP end parallel do

         V(:, 1:m) = 0.0d0

         ! matmul:: (n,3*m) x (n,3*m)
         P = matmul(V, b_small(:, 1:m))

         ! Detect convergence
         if (i == 1) then
            initial_R = maxval(abs(R))
         end if

         if (debug_mode) then
            print *, i, maxval(abs(R))/initial_R
         end if
         if (maxval(abs(R))/initial_R < tol) then
            if (debug_mode) then
               print *, "[OK] converged."
            end if
            lambda = lambda_small(1:m)

            exit
         else
            cycle
         end if
      end do

      if (i == MAX_ITR) print *, "[ERROR] LOBPCG NOT CONVERGED."

   end subroutine
! #################################################

! #################################################
   subroutine GramSchmidt(mat_v, m, n, mat_v_out)
      integer, intent(in)::m, n
      real(real64), intent(in)::mat_v(1:m, 1:n)
      real(real64), intent(inout)::mat_v_out(1:m, 1:n)
      integer::i, j
      real(real64) :: nai
      !real(real64) :: v(1:m),vi(1:m),viold(1:m)
      real(real64), allocatable :: v(:), vi(:)
      real(real64), allocatable :: viold(:)
      real(real64)::norm_v

      mat_v_out = mat_v
      allocate (viold(1:m))
      allocate (v(1:m))
      allocate (vi(1:m))

      do i = 1, n
         viold = mat_v_out(:, i)

         if (dot_product(viold, viold) == 0.0d0) cycle

         do j = 1, i - 1
            nai = dot_product(mat_v_out(1:m, j), viold(1:m))
            vi = viold - nai*mat_v_out(:, j)
            viold = vi
         end do

         norm_v = sqrt(dble(dot_product(viold, viold)))
         mat_v_out(:, j) = viold/norm_v

      end do

   end subroutine
! #################################################

! #################################################
   subroutine LAPACK_EIG_DENSE(A, B, x, lambda, debug)
      real(real64), intent(in) :: A(:, :), B(:, :)
      real(real64), allocatable, intent(inout):: x(:, :), lambda(:)
      logical, optional, intent(in) :: debug
      logical :: debug_mode
      !>>>>>>>>>>>>>> INPUT
      integer(int32) :: ITYPE = 1   ! A*x = (lambda)*B*x
      character(1) :: JOBZ = 'V' ! Compute eigenvalues and eigenvectors.
      character(1) :: UPLO = 'U' ! Upper triangles of A and B are stored;
      !<<<<<<<<<<<<<< INPUT

      integer(int32) :: N ! order of matrix
      real(real64), allocatable :: AP(:)
      real(real64), allocatable :: BP(:)
      real(real64), allocatable :: W(:)
      real(real64), allocatable :: Z(:, :), M(:)
      real(real64), allocatable :: WORK(:), ID(:)
      integer(int32), allocatable :: IWORK(:), IDS(:)
      integer(int32) :: LDZ
      integer(int32) :: LWORK
      integer(int32) :: LIWORK
      integer(int32) :: INFO
      integer(int32) :: from, to, k, j, i
      integer(int32), allocatable :: new_id_from_old_id(:)
      real(real64), allocatable :: dense_mat(:, :)
      !logical :: use_lanczos

      type(CRS_) :: crs

      debug_mode = input(default=.false., option=debug)
      !>>>>>>>>>>>>>> INPUT
      N = size(A, 1)
      LDZ = N
      LWORK = 1 + 6*N + 2*N**2
      LIWORK = 3 + 5*N
      !<<<<<<<<<<<<<< INPUT

      !>>>>>>>>>>>>>>  INPUT/OUTPUT
      AP = zeros(N*(N + 1)/2)
      BP = zeros(N*(N + 1)/2)
      ! Upper triangle matrix

      AP = to_UpperTriangle(A)!UpperTriangularMatrix(CRS_val=this%A_CRS_val,CRS_col=this%A_CRS_index_col,&
      !CRS_rowptr=this%A_CRS_index_row)
      BP = to_UpperTriangle(B)!UpperTriangularMatrix(CRS_val=this%B_CRS_val,CRS_col=this%B_CRS_index_col,&
      !CRS_rowptr=this%B_CRS_index_row)
      !<<<<<<<<<<<<<< INPUT/OUTPUT

      !>>>>>>>>>>>>>> INPUT
      N = size(A, 1)
      LDZ = N
      LWORK = 1 + 6*N + 2*N**2
      LIWORK = 3 + 5*N
      !<<<<<<<<<<<<<< INPUT

      !>>>>>>>>>>>>>>  OUTPUT
      W = zeros(N)
      Z = zeros(LDZ, N)
      WORK = zeros(LWORK)
      IWORK = zeros(LIWORK)
      INFO = 0
      !<<<<<<<<<<<<<< OUTPUT

      if (debug_mode) then
         print *, ">> Solver :: LAPACK/DSPGVD"
      end if
      call DSPGVD(ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK, &
                  LWORK, IWORK, LIWORK, INFO)

      X = Z
      lambda = W

   end subroutine
! #################################################
! #################################################
   subroutine LAPACK_EIG_SPARSE(A, B, x, lambda, debug)
      type(CRS_), intent(in) :: A, B
      real(real64), allocatable :: Ad(:, :), Bd(:, :)
      real(real64), allocatable, intent(inout):: x(:, :), lambda(:)
      logical, optional, intent(in) :: debug
      logical :: debug_mode
      !>>>>>>>>>>>>>> INPUT
      integer(int32) :: ITYPE = 1   ! A*x = (lambda)*B*x
      character(1) :: JOBZ = 'V' ! Compute eigenvalues and eigenvectors.
      character(1) :: UPLO = 'U' ! Upper triangles of A and B are stored;
      !<<<<<<<<<<<<<< INPUT

      integer(int32) :: N ! order of matrix
      real(real64), allocatable :: AP(:)
      real(real64), allocatable :: BP(:)
      real(real64), allocatable :: W(:)
      real(real64), allocatable :: Z(:, :), M(:)
      real(real64), allocatable :: WORK(:), ID(:)
      integer(int32), allocatable :: IWORK(:), IDS(:)
      integer(int32) :: LDZ
      integer(int32) :: LWORK
      integer(int32) :: LIWORK
      integer(int32) :: INFO
      integer(int32) :: from, to, k, j, i
      integer(int32), allocatable :: new_id_from_old_id(:)
      real(real64), allocatable :: dense_mat(:, :)
      !logical :: use_lanczos

      type(CRS_) :: crs

      Ad = A%to_dense()
      Bd = B%to_dense()
      debug_mode = input(default=.false., option=debug)
      !>>>>>>>>>>>>>> INPUT
      N = size(Ad, 1)
      LDZ = N
      LWORK = 1 + 6*N + 2*N**2
      LIWORK = 3 + 5*N
      !<<<<<<<<<<<<<< INPUT

      !>>>>>>>>>>>>>>  INPUT/OUTPUT
      AP = zeros(N*(N + 1)/2)
      BP = zeros(N*(N + 1)/2)
      ! Upper triangle matrix

      AP = to_UpperTriangle(Ad)!UpperTriangularMatrix(CRS_val=this%A_CRS_val,CRS_col=this%A_CRS_index_col,&
      !CRS_rowptr=this%A_CRS_index_row)
      BP = to_UpperTriangle(Bd)!UpperTriangularMatrix(CRS_val=this%B_CRS_val,CRS_col=this%B_CRS_index_col,&
      !CRS_rowptr=this%B_CRS_index_row)
      !<<<<<<<<<<<<<< INPUT/OUTPUT

      !>>>>>>>>>>>>>> INPUT
      N = size(Ad, 1)
      LDZ = N
      LWORK = 1 + 6*N + 2*N**2
      LIWORK = 3 + 5*N
      !<<<<<<<<<<<<<< INPUT

      !>>>>>>>>>>>>>>  OUTPUT
      W = zeros(N)
      Z = zeros(LDZ, N)
      WORK = zeros(LWORK)
      IWORK = zeros(LIWORK)
      INFO = 0
      !<<<<<<<<<<<<<< OUTPUT

      if (debug_mode) then
         print *, ">> Solver :: LAPACK/DSPGVD"
      end if
      call DSPGVD(ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK, &
                  LWORK, IWORK, LIWORK, INFO)

      X = Z
      lambda = W

   end subroutine
! #################################################

   function to_UpperTriangle(A) result(ret)
      real(real64), intent(in) :: A(:, :)
      real(real64), allocatable :: ret(:)
      integer(int32) :: i, j, n, m

      n = size(A, 1)
      ret = zeros(n*(n + 1)/2)

      m = 0
      do i = 1, size(A, 2)
         do j = 1, i
            m = m + 1
            ret(m) = A(j, i)
         end do
      end do

   end function

! #################################################

   subroutine BICGSTAB_CRSSparse(this, x, b, debug, tol)
      class(CRS_), intent(inout) :: this
      real(real64), allocatable, intent(inout) :: x(:)
      logical, optional, intent(in) :: debug
      real(real64), optional, intent(in) :: tol
      real(real64) :: er
      real(real64), intent(in) :: b(:)

      if (.not. allocated(x)) then
         x = zeros(size(b))
      end if

      er = dble(1.0e-14)
      if (present(tol)) then
         er = tol
      end if

      call bicgstab_CRS_SparseClass( &
         a=this%val, &
         ptr_i=this%row_ptr, &
         index_j=this%col_idx, &
         x=x, &
         b=b, &
         itrmax=1000000, &
         er=er, &
         relative_er=er, &
         debug=debug &
         )

   end subroutine
! #################################################

! #####################################################
   subroutine bicgstab_CRS_SparseClass(a, ptr_i, index_j, x, b, itrmax, er, relative_er, debug)
      integer(int64), intent(in) :: ptr_i(:)
      integer(int32), intent(in) :: index_j(:)
      integer(int32), intent(in) :: itrmax
      real(real64), intent(in) :: a(:)
      real(real64), intent(in) :: b(:)
      real(real64), intent(in) :: er
      real(real64), optional, intent(in) :: relative_er
      real(real64), intent(inout) :: x(:)
      logical, optional, intent(in) :: debug
      logical :: speak = .false.
      integer(int32) itr, i, j, n
      real(real64) alp, bet, c1, c2, c3, ev, vv, rr, er0, init_rr, re_er0
      real(real64), allocatable:: r(:), r0(:), p(:), y(:), e(:), v(:), pa(:), ax(:)

      er0 = er
      if (present(debug)) then
         speak = debug
      end if

      n = size(b)
      if (speak) print *, "BiCGSTAB STARTED >> DOF:", n
      allocate (r(n), r0(n), p(n), y(n), e(n), v(n))

      r(:) = b(:)
      if (speak) print *, "BiCGSTAB >> [1] initialize"

      call SpMV_CRS_Sparse(CRS_value=a, CRS_col=index_j, &
                           CRS_row_ptr=ptr_i, old_vector=x, new_vector=ax)
      r = b - ax

      if (speak) print *, "BiCGSTAB >> [2] dp1"

      c1 = dot_product(r, r)
      !call omp_dot_product(r,r,c1)

      init_rr = c1
      if (speak) print *, "BiCGSTAB >> [2] init_rr", c1
      !if(speak) print *, "BiCGSTAB >>      |r|^2 = ",init_rr

      !if (c1 < er0) return

      p(:) = r(:)
      r0(:) = r(:)

      do itr = 1, itrmax
         if (speak) print *, "BiCGSTAB >> ["//str(itr)//"] initialize"
         c1 = dot_product(r0, r)
         !call omp_dot_product(r0,r,c1)

         y(:) = 0.0d0
         call SpMV_CRS_Sparse(CRS_value=a, CRS_col=index_j, &
                              CRS_row_ptr=ptr_i, old_vector=p, new_vector=y)

         c2 = dot_product(r0, y)
         !call omp_dot_product(r0,y,c2)

         alp = c1/c2
         e(:) = r(:) - alp*y(:)
         v(:) = 0.0d0
         call SpMV_CRS_Sparse(CRS_value=a, CRS_col=index_j, &
                              CRS_row_ptr=ptr_i, old_vector=e, new_vector=v)

         if (speak) print *, "BiCGSTAB >> ["//str(itr)//"] half"

         ev = dot_product(e, v)
         vv = dot_product(v, v)
         !call omp_dot_product(e,v,ev)
         !call omp_dot_product(v,v,vv)

         if (vv == 0.0d0) stop "Bicgstab devide by zero"
         c3 = ev/vv
         if (speak) print *, "BiCGSTAB >> c3 = ev/vv", c3
         x(:) = x(:) + alp*p(:) + c3*e(:)
         r(:) = e(:) - c3*v(:)

         rr = dot_product(r, r)
         !call omp_dot_product(r,r,rr)

         if (itr == 1) then
            re_er0 = rr
         end if

         if (speak) then
            print *, sqrt(rr)
         end if

         if (present(relative_er)) then
            if (sqrt(rr/re_er0) < relative_er) then
               exit
            end if
         end if
         !    write(*,*) 'itr, er =', itr,rr
         if (sqrt(rr) < er0) exit

         c1 = dot_product(r0, r)
         !call omp_dot_product(r0,r,c1)

         bet = c1/(c2*c3)
         if ((c2*c3) == 0.0d0) stop "Bicgstab devide by zero"
         p(:) = r(:) + bet*(p(:) - c3*y(:))
      end do
   end subroutine
!===============================================================

   subroutine SpMV_CRS_Sparse(CRS_value, CRS_col, CRS_row_ptr, old_vector, new_vector, precondition)
      real(real64), intent(in)  :: CRS_value(:), Old_vector(:)
      integeR(int32), intent(in):: CRS_col(:)
      integeR(int64), intent(in):: CRS_row_ptr(:)
      type(CRS_), optional, intent(in) :: precondition
      real(real64), allocatable, intent(inout) :: new_vector(:)
      real(real64), allocatable :: precon_old_vector(:)
      integer(int32) :: i, j, n, gid, lid, row, col
      integer(int64) :: CRS_id
      !> x_i = A_ij b_j

      n = size(CRS_row_ptr) - 1
      if (size(old_vector) /= n) then
         print *, "ERROR crs_matvec :: inconsistent size for old_vector"
         return
      end if

      if (.not. allocated(new_vector)) then
         new_vector = zeros(n)
      else
         new_vector(:) = 0.0d0
      end if

      if (present(precondition)) then
         call precondition%ILU_matvec(old_vector=old_vector, new_vector=precon_old_vector)

         !$OMP parallel do default(shared) private(CRS_id,col)
         do row = 1, n
            do CRS_id = CRS_row_ptr(row), CRS_row_ptr(row + 1) - 1
               col = CRS_col(CRS_id)
               !$OMP atomic
               new_vector(row) = new_vector(row) + CRS_value(CRS_id)*precon_old_vector(col)
            end do
         end do
         !$OMP end parallel do
      else

         !$OMP parallel do default(shared) private(CRS_id,col)
         do row = 1, n
            do CRS_id = CRS_row_ptr(row), CRS_row_ptr(row + 1) - 1
               col = CRS_col(CRS_id)
               !$OMP atomic
               new_vector(row) = new_vector(row) + CRS_value(CRS_id)*old_vector(col)
            end do
         end do
         !$OMP end parallel do

      end if

   end subroutine
   ! ###################################################################

   function divide_by_CRS(this, diag_vector) result(ret_crs)
      class(CRS_), intent(in) :: this
      real(real64), intent(in) :: diag_vector(:)
      type(CRS_) :: ret_crs
      integer(int64) :: i, j
      ret_crs = this

      if (size(diag_vector) == 1) then
         ret_crs%val(:) = ret_crs%val(:)/diag_vector(1)
         return
      end if

      if (this%size() /= size(diag_vector)) then
         print *, "ERROR >> divide_by_CRS >> this%size()/=size(diag_vector) "
         stop
      end if

      do i = 1, ret_crs%size()
         do j = ret_crs%row_ptr(i), ret_crs%row_ptr(i + 1) - 1
            ret_crs%val(j) = ret_crs%val(j)/diag_vector(i)
         end do
      end do

   end function
! ###################################################################

   function mult_by_CRS(this, diag_vector) result(ret_crs)
      class(CRS_), intent(in) :: this
      real(real64), intent(in) :: diag_vector(:)
      type(CRS_) :: ret_crs
      integer(int64) :: i, j
      ret_crs = this

      if (size(diag_vector) == 1) then
         ret_crs%val(:) = ret_crs%val(:)/diag_vector(1)
         return
      end if

      if (this%size() /= size(diag_vector)) then
         print *, "ERROR >> divide_by_CRS >> this%size()/=size(diag_vector) "
         stop
      end if

      do i = 1, ret_crs%size()
         do j = ret_crs%row_ptr(i), ret_crs%row_ptr(i + 1) - 1
            ret_crs%val(j) = ret_crs%val(j)*diag_vector(i)
         end do
      end do

   end function
! ######################################################
   function matmul_CRS_vec(A, b) result(vec)
      type(CRS_), intent(in) :: A
      real(real64), intent(in) :: b(:)
      real(real64), allocatable :: vec(:)

      vec = A%matmul(b)

   end function

! ######################################################
   function matmul_CRS_CRS(A, B) result(CRS)
      type(CRS_), intent(in) :: A, B
      type(COO_) :: COO
      type(CRS_) :: CRS
      type(CCS_) :: B_CCS
      real(real64), allocatable :: col_vec(:)
      integer(int32) :: col, row, col_ptr, i, j
      integer(int32), allocatable :: A_col_idx(:), B_row_idx(:)
      real(real64), allocatable :: A_col_val(:), B_row_val(:)

      ! Considerably slow!! DO NOT USE!
      ! Considerably slow!! DO NOT USE!
      ! Considerably slow!! DO NOT USE!
      ! Considerably slow!! DO NOT USE!
      ! Considerably slow!! DO NOT USE!
      ! Considerably slow!! DO NOT USE!
      ! Considerably slow!! DO NOT USE!
      ! Considerably slow!! DO NOT USE!
      ! Considerably slow!! DO NOT USE!
      ! Considerably slow!! DO NOT USE!
      ! Considerably slow!! DO NOT USE!

      ! matrix multiplication
      ! C_ij = A_ik B_kj
      call COO%init(A%size())
      B_CCS = B%to_CCS()

!    ! avoiding fill-in (inaccurate)
!    if(A%size() > B%size() )then
!        CRS = A
!        CRS%val(:) = 0.0d0
!    else
!        CRS = B
!        CRS%val(:) = 0.0d0
!    endif
!
!    do row=1,size(CRS%row_ptr)-1
!        do col_ptr=CRS%row_ptr(row),CRS%row_ptr(row+1)-1
!            col = CRS%col_idx(col_ptr)
!            A_col_idx = A%col_idx( A%row_ptr(row):A%row_ptr(row+1)-1 )
!            B_row_idx = B_CCS%row_idx( B_CCS%col_ptr(col):B_CCS%col_ptr(col+1)-1 )
!            A_col_val = A%val( A%row_ptr(row):A%row_ptr(row+1)-1 )
!            B_row_val = B_CCS%val( B_CCS%col_ptr(col):B_CCS%col_ptr(col+1)-1 )
!            if(minval(A_col_idx) > maxval(B_row_idx))cycle
!            if(maxval(A_col_idx) < minval(B_row_idx))cycle
!            do i=1,size(A_col_idx)
!                do j=1, size(B_row_idx)
!                    if(A_col_idx(i)==B_row_idx(j) )then
!                        CRS%val(col) = CRS%val(col) + A_col_val(i)*B_row_val(j)
!                    endif
!                enddo
!            enddo
!        enddo
!    enddo

      do col = 1, size(B_CCS%col_ptr) - 1
         col_vec = A%matmul(B_CCS%get_column(col))
        !!$OMP parallel do
         do row = 1, size(col_vec)
            if (col_vec(row) /= 0.0d0) then
                !!$OMP atomic
               call COO%set(row, col, col_vec(row))
            end if
         end do
        !!$OMP end parallel do
      end do
      CRS = COO%to_CRS()

   end function
! ###################################################
   function get_column_CCS(this, col) result(ret)
      class(CCS_), intent(in) :: this
      integer(int32), intent(in) :: col
      real(real64), allocatable :: ret(:)
      integer(int32) :: row, n

      n = size(this%col_ptr, 1) - 1
      ret = zeros(n)

      do row = this%col_ptr(col), this%col_ptr(col + 1) - 1
         ret(this%row_idx(row)) = this%val(row)
      end do

   end function

! #####################################################
   subroutine randomCOO(this, n, percent)
      class(COO_), intent(inout) :: this
      integer(int32), intent(in) :: n
      real(real32), optional, intent(in) :: percent
      real(real32) :: fill_percent
      integer(int32) :: m, i, row, col
      real(real64) :: val
      type(Random_) :: random

      fill_percent = input(default=10.0, option=percent)
      m = maxval([int(dble(n)*dble(n)*dble(percent)/100.0d0), 1])
      call this%init(n)
      do i = 1, m
         row = int(n*random%random()) + 1
         col = int(n*random%random()) + 1
         val = random%gauss(mu=0.0d0, sigma=1.0d0)
         call this%set(row, col, val)
         call this%set(col, row, val)
      end do

   end subroutine
! #####################################################

   subroutine eyesCRS(this, n)
      class(CRS_), intent(inout) :: This
      type(COO_) :: coo
      type(CRS_) :: buf
      integer(int32), intent(in) :: n
      integer(int32) :: i

      call coo%init(n)
      do i = 1, n
         call coo%set(i, i, 1.0d0)
      end do
      buf = coo%to_CRS()
      this%row_ptr = buf%row_ptr
      this%col_idx = buf%col_idx
      this%val = buf%val
   end subroutine
! ###################################################

! ###################################################
   function tensor_exponential_crs(this, itr_tol, tol, x, dt, fix_idx, fix_val) result(expA_v)
      class(CRS_), intent(in) :: this
      real(real64), intent(in) :: x(:)
      real(real64), optional, intent(in) :: dt
      real(real64), allocatable :: expA_v(:), increA_v(:), bhat(:)

      real(real64), intent(in) :: tol
      integer(int32), intent(in)::itr_tol
!    real(real64),allocatable::increA(:,:)
      real(real64)   :: increment, NN, t
      integer(int32) :: i, j, n

      real(real64), optional, intent(in) :: fix_val(:)
      integer(int32), optional, intent(in)::fix_idx(:)
      type(CRS_) :: Amatrix
!    if(.not. allocated(expA) )allocate(expA(size(A,1),size(A,2) ))
!    allocate(increA(size(A,1),size(A,2) ))
!    if(size(A,1)/=size(A,2)) stop "ERROR:tensor exp is not a square matrix"

!    expA_v = x
!    do n=1,size(expA,1)
!        expA(n,n)=1.0d0
!    enddo
      t = input(default=1.0d0, option=dt)
      ! exp(At) = I + At + 1/2*(At)^2 + 1/6*(At)^3 + ....
      ! exp(At)v = Iv + (At)v + 1/2*(At)^2 v + 1/6*(At)^3 v+ ....
      ! exp(At)v = v + (At)v + 1/2*(At)^2 v + 1/6*(At)^3 v+ ....
!    expA_v = x
!    do i=1,itr_tol
!        increA_v = x
!        do j=1,i
!            increA_v = t/dble(j)*this%matmul(increA_v)
!        enddo
!        expA_v = expA_v + increA_v
!        if(dot_product(increA_v,increA_v) < tol )exit
!
!    enddo
!    return

      ! i==1,2
      ! 0-th order term
      if (present(fix_idx)) then
         Amatrix = this
         bhat = zeros(this%size())
         call Amatrix%fix(idx=fix_idx, RHS=bhat, val=fix_val)

         increA_v = x
         expA_v = zeros(size(x))
         expA_v = expA_v + increA_v
         ! 1-st order term
         increA_v = Amatrix%matmul(t*increA_v) - bhat*dt
         expA_v = expA_v + increA_v

         do i = 2, itr_tol
            increA_v = 1.0d0/(i)*Amatrix%matmul(t*increA_v) !- bhat*dt
            expA_v = expA_v + increA_v

            if (dot_product(increA_v, increA_v) < tol) exit
         end do
      else

         increA_v = x
         expA_v = zeros(size(x))
         expA_v = expA_v + increA_v
         ! 1-st order term
         increA_v = this%matmul(t*increA_v)
         expA_v = expA_v + increA_v

         do i = 2, itr_tol
            increA_v = 1.0d0/(i)*this%matmul(t*increA_v)
            expA_v = expA_v + increA_v

            if (dot_product(increA_v, increA_v) < tol) exit
         end do
      end if
   end function

! ###################################################
   function tensor_exp_sqrt_crs(this, v, tol, coeff, itrmax, binomial) result(exp_sqrtA_v)
      class(CRS_), intent(in) :: this
      real(real64), intent(in) :: v(:)
      real(real64), allocatable :: dv(:), exp_sqrtA_v(:)
      logical, optional, intent(in) :: binomial
      complex(real64), optional, intent(in) :: coeff
      integer(int32) :: i
      integer(int32), intent(in) :: itrmax
      real(real64), intent(in) :: tol
      complex(real64) :: coeffi

      if (present(coeff)) then
         coeffi = coeff
      else
         coeffi = 1.0d0
      end if

      dv = v
      exp_sqrtA_v = zeros(size(v))
      exp_sqrtA_v = exp_sqrtA_v + dv

      ! 1-st order term
      dv = coeffi*this%tensor_sqrt(v=dv, tol=tol, itrmax=itrmax, binomial=binomial)

      exp_sqrtA_v = exp_sqrtA_v + dv

      do i = 2, itrmax
         if (i == 1) then
            cycle
         end if

         dv = 1.0d0/(i)*coeffi*this%tensor_sqrt(v=dv, tol=tol, itrmax=itrmax, binomial=binomial)
         exp_sqrtA_v = exp_sqrtA_v + dv

         if (dot_product(dv, dv) < tol) exit
      end do

   end function
! #####################################################

! ###################################################
   function tensor_sqrt_crs(this, v, tol, itrmax, binomial, r) result(sqrtA_v)
      class(CRS_), intent(in) :: this
      real(real64), intent(in) :: v(:)
      real(real64), allocatable :: dv(:), sqrtA_v(:), logA_v(:)
      integer(int32) :: k
      integer(int32), intent(in) :: itrmax
      real(real64), intent(in) :: tol
      real(real64) :: coeff, nCr
      logical, optional, intenT(in) :: binomial
      real(real64), optional, intent(in) :: r

      if (present(binomial)) then
         if (binomial) then

            !  二項係数によるsqrt(A)の近似
            if (present(r)) then
               coeff = 1.0d0/maxval(this%val*r)
            else
               coeff = 1.0d0/maxval(this%val*5)
            end if

            sqrtA_v = 0.0d0*v
            dv = v
            do k = 0, itrmax
               nCr = gamma(0.50d0 + 1.0d0)/gamma(k + 1.0d0)/gamma(0.50d0 - k + 1)
               sqrtA_v = sqrtA_v + nCr*dv
               dv = (-dv + coeff*this%matmul(dv))
               if (norm(dv)/size(dv) < tol) exit
            end do
            sqrtA_v = sqrtA_v/sqrt(coeff)
            return
         end if
      end if

      dv = v
      sqrtA_v = zeros(size(v))
      sqrtA_v = sqrtA_v + dv
      ! 1-st order term
      dv = 0.50d0*this%tensor_log(v=dv, tol=tol, itrmax=itrmax)
      sqrtA_v = sqrtA_v + dv

      do k = 2, itrmax
         if (k == 1) then
            cycle
         end if

         dv = 1.0d0/dble(k)*0.50d0*this%tensor_log(v=dv, tol=tol, itrmax=itrmax)
         sqrtA_v = sqrtA_v + dv

         if (dot_product(dv, dv) < tol) exit
      end do

   end function
! #####################################################

! ###################################################
   function tensor_log_crs_modified_Sparse(this, v, tol, itrmax, r) result(sqrtA_v)
      class(CRS_), intent(in) :: this
      real(real64), intent(in) :: v(:)
      real(real64), allocatable :: dv(:), sqrtA_v(:)
      integer(int32) :: k, i
      integer(int32), intent(in) :: itrmax
      real(real64), intent(in) :: tol
      real(real64), optional, intent(in) :: r

      !real(real64) :: c
      real(real64), allocatable :: c(:), lambda_mat(:, :), X(:, :), lambda(:)
      real(real64) :: c_val

      type(CRS_) :: I_CRS

!    call I_CRS%eyes(this%size() )
!    call LAPACK_EIG(A=this,B=I_CRS,x=x,lambda=lambda)
!    lambda_mat = zeros(this%size(),this%size())
!    do i=1,this%size()
!        lambda_mat(i,i) = log(lambda(i))
!    enddo
!
!    ! shor algorithm
!    sqrtA_v = matmul(matmul( x, matmul(lambda_mat,transpose(x)) ),v )
!    return

      !c = maxval(this%val)*1.0d0
      !c = this%diag()*10.0d0 ! テイラー展開中心をdiagで決める．
      if (present(r)) then
         c_val = r
      else
         c_val = maxval(abs(this%val))
      end if
      !c = ones(this%size() )
      !c = 1.0d0
      ! k = 0
      ! Martin, J. F. P. On the exponential representation of solutions of linear differential equations. J. Differ. Equ. 4, 257–279 (1968).
      ! ln(this) = Ln(this) = ln(exp(C) X ) - C
      sqrtA_v = -(c_val)*v

      do k = 1, itrmax
         if (k == 1) then
            !dv = k/c_val*(-c_val*v + this%matmul(v)  )
            dv = 1.0d0/k*(exp(c_val)*this%matmul(v) - v)
         else
            dv = (-1.0d0)*dble(k)/dble(k + 1)*(exp(c_val)*this%matmul(dv) - dv)
         end if
         sqrtA_v = sqrtA_v + dv

         if (norm(abs(dv)) < tol) return
      end do

   end function
! #####################################################

! ###################################################
   function tensor_log_crs(this, v, tol, itrmax, r) result(sqrtA_v)

      ! wikipedia's difinition
      ! it is accurate only when "this" is a diagonally dominant matrix
      class(CRS_), intent(in) :: this
      real(real64), intent(in) :: v(:)
      real(real64), allocatable :: dv(:), sqrtA_v(:)
      integer(int32) :: k
      integer(int32), intent(in) :: itrmax
      real(real64), intent(in) :: tol
      !real(real64) :: c
      real(real64) :: c
      real(real64), optional, intent(in) :: r
      !real(real64) :: c
      !c = maxval(this%diag())
      !c = maxval(this%val)*1.0d0
      c = maxval(abs(this%diag()))*2.0d0 ! テイラー展開中心をdiagで決める．
      !c = 1.0d0
      ! k = 0
      if (present(r)) then
         c = r
      end if

      sqrtA_v = log(c)*v

      do k = 1, itrmax
         if (k == 1) then
            dv = (1.0d0)/k*(v - 1.0d0/c*this%matmul(v))
         else
            dv = (1.0d0)*dble(k)/dble(k + 1)*(dv - 1.0d0/c*this%matmul(dv))
         end if
         sqrtA_v = sqrtA_v - dv

         if (norm(abs(dv)) < tol) return
      end do

   end function
! #####################################################

! #####################################################
   subroutine fixCRS(this, idx, val, RHS, only_row)
      class(CRS_), intent(inout) :: this
      integer(int32), intent(in) :: idx(:)
      real(real64), optional, intent(inout) :: RHS(:)
      real(real64), intent(in) :: val(:)
      integer(int64) :: i, j, k, id

      logical, optional, intent(in) :: only_row

      if (present(only_row)) then
         if (only_row) then

            do i = 1, size(idx)
               id = idx(i)
               do j = this%row_ptr(id), this%row_ptr(id + 1) - 1
                  if (this%col_idx(j) == id) then
                     this%val(j) = 1.0d0
                  else
                     this%val(j) = 0.0d0
                  end if
               end do
            end do
            return
         end if
      end if

      do i = 1, size(idx)
         id = idx(i)
         do j = 1, size(this%row_ptr) - 1
            if (j == id) then
               do k = this%row_ptr(j), this%row_ptr(j + 1) - 1
                  if (this%col_idx(k) == id) then
                     this%val(k) = 0.0d0
                  else
                     this%val(k) = 0.0d0
                  end if
               end do
            else
               do k = this%row_ptr(j), this%row_ptr(j + 1) - 1
                  if (this%col_idx(k) == id) then
                     RHS(j) = RHS(j) - this%val(k)*val(i)
                     this%val(k) = 0.0d0
                  end if
               end do
            end if
         end do
      end do

      do i = 1, size(idx)
         id = idx(i)
         do j = 1, size(this%row_ptr) - 1
            if (j == id) then
               do k = this%row_ptr(j), this%row_ptr(j + 1) - 1
                  if (this%col_idx(k) == id) then
                     this%val(k) = 1.0d0
                  end if
               end do
            end if
         end do
      end do

      do i = 1, size(idx)
         RHS(id) = val(i)
      end do
   end subroutine
! #####################################################

! #####################################################
   subroutine fix_zeroCRS(this, idx)
      class(CRS_), intent(inout) :: this
      integer(int32), intent(in) :: idx(:)

      integer(int64) :: i, j, k, id
      do i = 1, size(idx)
         id = idx(i)
         do j = 1, size(this%row_ptr) - 1
            if (j == id) then
               do k = this%row_ptr(j), this%row_ptr(j + 1) - 1
                  if (this%col_idx(k) == id) then
                     this%val(k) = 0.0d0
                  else
                     this%val(k) = 0.0d0
                  end if
               end do
            else
               do k = this%row_ptr(j), this%row_ptr(j + 1) - 1
                  if (this%col_idx(k) == id) then

                     this%val(k) = 0.0d0
                  end if
               end do
            end if
         end do
      end do

      do i = 1, size(idx)
         id = idx(i)
         do j = 1, size(this%row_ptr) - 1
            if (j == id) then
               do k = this%row_ptr(j), this%row_ptr(j + 1) - 1
                  if (this%col_idx(k) == id) then
                     this%val(k) = 1.0d0
                  end if
               end do
            end if
         end do
      end do

   end subroutine
! #####################################################

! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> complex >>>>>>>>>>>>>>>>>>>>>>>

! ###################################################
   function tensor_exponential_complex64_crs(this, itr_tol, tol, x, dt, fix_idx, fix_val) result(expA_v)
      class(CRS_), intent(in) :: this
      complex(real64), intent(in) :: x(:)
      real(real64), optional, intent(in) :: dt
      complex(real64), allocatable :: expA_v(:), increA_v(:), bhat(:)

      real(real64), intent(in) :: tol
      integer(int32), intent(in)::itr_tol
!    complex(real64),allocatable::increA(:,:)
      real(real64)   :: increment, NN, t
      integer(int32) :: i, j, n

      complex(real64), optional, intent(in) :: fix_val(:)
      integer(int32), optional, intent(in)::fix_idx(:)
      type(CRS_) :: Amatrix
!    if(.not. allocated(expA) )allocate(expA(size(A,1),size(A,2) ))
!    allocate(increA(size(A,1),size(A,2) ))
!    if(size(A,1)/=size(A,2)) stop "ERROR:tensor exp is not a square matrix"

!    expA_v = x
!    do n=1,size(expA,1)
!        expA(n,n)=1.0d0
!    enddo
      t = input(default=1.0d0, option=dt)
      ! exp(At) = I + At + 1/2*(At)^2 + 1/6*(At)^3 + ....
      ! exp(At)v = Iv + (At)v + 1/2*(At)^2 v + 1/6*(At)^3 v+ ....
      ! exp(At)v = v + (At)v + 1/2*(At)^2 v + 1/6*(At)^3 v+ ....
!    expA_v = x
!    do i=1,itr_tol
!        increA_v = x
!        do j=1,i
!            increA_v = t/dble(j)*this%matmul(increA_v)
!        enddo
!        expA_v = expA_v + increA_v
!        if(dot_product(increA_v,increA_v) < tol )exit
!
!    enddo
!    return

      ! i==1,2
      ! 0-th order term
      if (present(fix_idx)) then
         Amatrix = this
         bhat = zeros(this%size())
         call Amatrix%fix(idx=fix_idx, RHS=bhat, val=fix_val)

         increA_v = x
         expA_v = zeros(size(x))
         expA_v = expA_v + increA_v
         ! 1-st order term
         increA_v = Amatrix%matmul(t*increA_v) - bhat*dt
         expA_v = expA_v + increA_v

         do i = 2, itr_tol
            if (i == 1) then
               cycle
            end if

            increA_v = 1.0d0/(i)*Amatrix%matmul(t*increA_v) !- bhat*dt
            expA_v = expA_v + increA_v

            if (abs(dot_product(increA_v, increA_v)) < tol) exit
         end do
      else

         increA_v = x
         expA_v = zeros(size(x))
         expA_v = expA_v + increA_v
         ! 1-st order term
         increA_v = this%matmul(t*increA_v)
         expA_v = expA_v + increA_v

         do i = 2, itr_tol
            if (i == 1) then

               cycle
            end if

            increA_v = 1.0d0/(i)*this%matmul(t*increA_v)
            expA_v = expA_v + increA_v

            if (abs(dot_product(increA_v, increA_v)) < tol) exit
         end do
      end if
   end function

! ###################################################
   function tensor_exp_sqrt_complex64_crs(this, v, tol, itrmax, coeff, fix_idx, binomial, r) result(exp_sqrtA_v)
      class(CRS_), intent(in) :: this
      complex(real64), intent(in) :: v(:)
      complex(real64), optional, intent(in) :: coeff
      complex(real64), allocatable :: dv(:), exp_sqrtA_v(:), bhat(:), buf(:)
      integer(int32) :: i, k
      integer(int32), intent(in) :: itrmax
      real(real64), intent(in) :: tol
      logical, optional, intent(in) :: binomial
      real(real64), optional, intent(in) :: r

      !real(real64),optional,intent(in) :: fix_val(:) ! only 0-fix is available
      integer(int32), optional, intent(in)::fix_idx(:)
      complex(real64) :: coeffi

      type(CRS_) :: Amatrix
      type(Math_) :: math

      if (present(coeff)) then
         coeffi = coeff
      else
         coeffi = 1.0d0
      end if

      if (present(fix_idx)) then
         !Amatrix = this
         !bhat = zeros(this%size() )
         !call Amatrix%fix(idx=fix_idx,RHS=bhat,val=fix_val)

         !increA_v = x
         !expA_v = zeros(size(x) )
         !expA_v = expA_v + increA_v
         ! 1-st order term
         !increA_v = Amatrix%matmul(t*increA_v) - bhat*dt
         !expA_v = expA_v + increA_v

         !do i=2,itr_tol
         !    if(i==1)then
         !        cycle
         !    endif
         !    increA_v = 1.0d0/(i)*Amatrix%matmul(t*increA_v) !- bhat*dt
         !    expA_v = expA_v + increA_v
         !
         !    if(abs(dot_product(increA_v,increA_v)) < tol )exit
         !enddo

         Amatrix = this

         dv = zeros(size(v) - size(fix_idx))

         k = 0
         do i = 1, size(v)
            if (i.in.fix_idx) then
               cycle
            else
               k = k + 1
               dv(k) = v(i)
            end if
         end do

         !dv(fix_idx(:) ) = fix_val(:)!/exp(1.0d0)

         !call Amatrix%fix(idx=fix_idx,RHS=bhat,val=fix_val+0.0d0*math%i)

         !call Amatrix%fix(idx=fix_idx,RHS=bhat,val=0.0d0*zeros(size(fix_idx) ) +0.0d0*math%i)

         call Amatrix%remove(idx=fix_idx) ! remove row and columns

         !call Amatrix%fix(idx=fix_idx,val=fix_val+0.0d0*math%i,only_row=.true.)
         !call Amatrix%fix(idx=fix_idx,RHS=dv,val=fix_val+0.0d0*math%i)
         !dv = dv + bhat
         !<test>
         !dv(fix_idx) = fix_val

         ! ignore row and columns
         exp_sqrtA_v = zeros(size(v) - size(fix_idx))
         exp_sqrtA_v = exp_sqrtA_v + dv
         !exp_sqrtA_v(fix_idx(:) ) = fix_val(:)

         ! 1-st order term
         dv = coeffi*Amatrix%tensor_sqrt(v=dv, tol=tol, itrmax=itrmax, binomial=binomial, r=r)
         exp_sqrtA_v = exp_sqrtA_v + dv
         !exp_sqrtA_v(fix_idx) = fix_val

         do i = 2, itrmax

            dv = 1.0d0/(i)*coeffi*Amatrix%tensor_sqrt(v=dv, tol=tol, itrmax=itrmax, binomial=binomial, r=r)
            !dv(fix_idx) = 0.0d0
            exp_sqrtA_v = exp_sqrtA_v + dv
            !exp_sqrtA_v(fix_idx) = fix_val
            if (abs(dot_product(dv, dv)) < tol) exit
         end do
         !exp_sqrtA_v(fix_idx(:) ) = fix_val(:)
         buf = exp_sqrtA_v
         exp_sqrtA_v = 0.0d0*v

         k = 0
         do i = 1, size(v)
            if (i.in.fix_idx) then
               exp_sqrtA_v(i) = 0.0d0
            else
               k = k + 1
               exp_sqrtA_v(i) = buf(k)
            end if
         end do

      else

         dv = v
         exp_sqrtA_v = zeros(size(v))
         exp_sqrtA_v = exp_sqrtA_v + dv

         ! 1-st order term
         dv = coeffi*this%tensor_sqrt(v=dv, tol=tol, itrmax=itrmax, binomial=binomial, r=r)
         exp_sqrtA_v = exp_sqrtA_v + dv

         do i = 2, itrmax

            dv = 1.0d0/(i)*coeffi*this%tensor_sqrt(v=dv, tol=tol, itrmax=itrmax, binomial=binomial, r=r)
            exp_sqrtA_v = exp_sqrtA_v + dv

            if (abs(dot_product(dv, dv)) < tol) exit
         end do
      end if

   end function
! #####################################################

! ###################################################
   function tensor_sqrt_complex64_crs(this, v, tol, itrmax, binomial, r) result(sqrtA_v)
      class(CRS_), intent(in) :: this
      complex(real64), intent(in) :: v(:)
      complex(real64), allocatable :: dv(:), sqrtA_v(:), logA_v(:)
      integer(int32) :: k
      integer(int32), intent(in) :: itrmax
      real(real64), intent(in) :: tol
      real(real64) :: coeff, nCr
      logical, optional, intenT(in) :: binomial
      real(real64), optional, intent(in) :: r
      if (present(binomial)) then
         if (binomial) then

            !  二項係数によるsqrt(A)の近似
            if (present(r)) then
               coeff = 1.0d0/maxval(this%val*r)
            else
               coeff = 1.0d0/maxval(this%val*5)
            end if

            sqrtA_v = 0.0d0*v
            dv = v
            do k = 0, itrmax
               nCr = gamma(0.50d0 + 1.0d0)/gamma(k + 1.0d0)/gamma(0.50d0 - k + 1)
               sqrtA_v = sqrtA_v + nCr*dv
               dv = (-dv + coeff*this%matmul(dv))
               if (norm(dble(dv))/size(dv) < tol) exit
            end do
            sqrtA_v = sqrtA_v/sqrt(coeff)
            return
         end if
      end if

      dv = v
      sqrtA_v = zeros(size(v))
      sqrtA_v = sqrtA_v + dv
      ! 1-st order term
      dv = 0.50d0*this%tensor_log(v=dv, tol=tol, itrmax=itrmax)
      sqrtA_v = sqrtA_v + dv

      do k = 2, itrmax
         if (k == 1) then

            cycle
         end if

         dv = 1.0d0/dble(k)*0.50d0*this%tensor_log(v=dv, tol=tol, itrmax=itrmax)
         sqrtA_v = sqrtA_v + dv

         if (abs(dot_product(dv, dv)) < tol) exit
      end do

   end function
! #####################################################

! ###################################################
   function tensor_log_complex64_crs(this, v, tol, itrmax) result(sqrtA_v)
      class(CRS_), intent(in) :: this
      complex(real64), intent(in) :: v(:)
      complex(real64), allocatable :: dv(:), sqrtA_v(:)
      integer(int32) :: k
      integer(int32), intent(in) :: itrmax
      real(real64), intent(in) :: tol
      !complex(real64) :: c
      real(real64), allocatable :: c(:)

      !c = maxval(this%val)*1.
      c = this%diag() ! テイラー展開中心をdiagで決める．
      !c = maxval(this%diag())*3.0d0*ones(size(v)) ! テイラー展開中心をdiagで決める．
      !c = maxval(this%diag(cell_centered=.true.))*ones(size(v)) ! テイラー展開中心をdiagで決める．

      !c = 1.0d0
      ! k = 0
      sqrtA_v = zeros(size(v))
      sqrtA_v = log(c)*v

      do k = 1, itrmax
         if (k == 1) then
            dv = k/c*(-c*v + this%matmul(v))
         else
            dv = (-1.0d0)/c*dble(k)/dble(k + 1)*(-c*dv + this%matmul(dv))
         end if
         sqrtA_v = sqrtA_v + dv

         if (norm(abs(dv)) < tol) return
      end do

   end function
! #####################################################

! ###################################################
   function tensor_sinc_sqrt_complex64_crs(this, v, tol, itrmax, coeff, debug, fix_idx) result(retA_v)
      class(CRS_), intent(in) :: this
      complex(real64), intent(in) :: v(:)
      complex(real64), optional, intent(in) :: coeff
      complex(real64), allocatable :: dv(:), retA_v(:)
      integer(int32), optional, intent(in) :: fix_idx(:)
      integer(int32) :: k
      integer(int32), intent(in) :: itrmax
      logical, optional, intent(in) :: debug
      real(real64), intent(in) :: tol
      type(Math_) :: math
      complex(real64) :: a, b

      if (present(coeff)) then
         a = coeff
      else
         a = 1.0d0
      end if
      ! sinc(sqrt(A) ) = \sum_{n=0}^{\infty}
      !c = 1.0d0
      ! k = 0

      !k=0
      k = 0
      retA_v = v

      ! zero-fixed Dirichlet boundary
      if (present(fix_idx)) then
         retA_v(fix_idx) = 0
      end if

      ! k=1
      k = 1
      dv = a*a*this%matmul(v)

      ! zero-fixed Dirichlet boundary
      if (present(fix_idx)) then
         dv(fix_idx) = 0
      end if

      retA_v = retA_v + ((-1.0d0)**k)*(sqrt(math%pi)*2.0d0**(-1 - 2*k)) &
               /gamma(1.0d0 + k)/gamma(1.5d0 + k)*dv

      do k = 2, itrmax

         dv = a*a*this%matmul(dv)

         ! zero-fixed Dirichlet boundary
         if (present(fix_idx)) then
            dv(fix_idx) = 0
         end if
         b = ((-1.0d0)**k)*(sqrt(math%pi)*2.0d0**(-1 - 2*k)) &
             /gamma(1.0d0 + k)/gamma(1.5d0 + k)
         retA_v = retA_v + b*dv

         if (norm(abs(b*dv)) < tol) return

         if (present(debug)) then
            if (debug) then
               print *, k, norm(abs(b*dv))
            end if
         end if

      end do

   end function
! #####################################################

! ###################################################
   function tensor_sinc_sqrt_real64_crs(this, v, tol, itrmax, coeff, debug, fix_idx) result(retA_v)
      class(CRS_), intent(in) :: this
      real(real64), intent(in) :: v(:)
      real(real64), optional, intent(in) :: coeff
      real(real64), allocatable :: dv(:), retA_v(:)
      integer(int32), optional, intent(in) :: fix_idx(:)
      integer(int32) :: k
      integer(int32), intent(in) :: itrmax
      logical, optional, intent(in) :: debug
      real(real64), intent(in) :: tol
      type(Math_) :: math
      real(real64) :: a, b

      if (present(coeff)) then
         a = coeff
      else
         a = 1.0d0
      end if
      ! sinc(sqrt(A) ) = \sum_{n=0}^{\infty}
      !c = 1.0d0
      ! k = 0

      !k=0
      k = 0
      retA_v = v

      ! zero-fixed Dirichlet boundary
      if (present(fix_idx)) then
         retA_v(fix_idx) = 0
      end if

      ! k=1
      k = 1
      dv = a*a*this%matmul(v)

      ! zero-fixed Dirichlet boundary
      if (present(fix_idx)) then
         dv(fix_idx) = 0
      end if

      retA_v = retA_v + ((-1.0d0)**k)*(sqrt(math%pi)*2.0d0**(-1 - 2*k)) &
               /gamma(1.0d0 + k)/gamma(1.5d0 + k)*dv

      do k = 2, itrmax

         dv = a*a*this%matmul(dv)

         ! zero-fixed Dirichlet boundary
         if (present(fix_idx)) then
            dv(fix_idx) = 0
         end if
         b = ((-1.0d0)**k)*(sqrt(math%pi)*2.0d0**(-1 - 2*k)) &
             /gamma(1.0d0 + k)/gamma(1.5d0 + k)
         retA_v = retA_v + b*dv

         if (norm(abs(b*dv)) < tol) return

         if (present(debug)) then
            if (debug) then
               print *, k, norm(abs(b*dv))
            end if
         end if

      end do

   end function
! #####################################################

   function tensor_wave_kernel_complex_64_crs(this, u0, v0, tol, itrmax, h, t, fix_idx, debug) result(u)
      class(CRS_), intent(in) :: this
      complex(real64), intent(in) :: u0(:), v0(:)
      complex(real64), allocatable:: Adu(:), Adv(:), u(:)
      complex(real64) :: cos_coeff, sinc_coeff
      real(real64), intent(in) :: h, t
      logical, optional, intent(in) :: debug

      integer(int32), optional, intent(in) :: itrmax
      real(real64), optional, intent(in) :: tol
      integer(int32), optional, intent(in) :: fix_idx(:)

      integer(int32) :: itr_max = 100
      real(real64)   :: itr_tol = dble(1.0e-16)
      logical :: debug_mode
      type(Math_) :: math
      integer(int32) :: n

      if (present(itrmax)) then
         itr_max = itrmax
      end if

      if (present(tol)) then
         itr_tol = tol
      end if

      ! a + 2 h M^{-1} v + M^{-1} K u = 0
      ! u(t) = exp(-ht)( cos(t*sqrt(M^{-1} K - h^2 I)  ) u
      !      + t*sinc( t*sqrt(M^{-1} K - h^2 I) ) v

      u = exp(-h*t)*this%tensor_cos_sqrt(v=u0, tol=itr_tol, itrmax=itr_max, &
                                         coeff=t + 0*math%i, debug=debug, fix_idx=fix_idx) &
          + exp(-h*t)*t*this%tensor_sinc_sqrt(v=v0, tol=itr_tol, itrmax=itr_max, &
                                              coeff=t + 0*math%i, debug=debug, fix_idx=fix_idx)

   end function

   function tensor_wave_kernel_real_64_crs(this, u0, v0, tol, itrmax, h, t, fix_idx, debug) result(u)
      class(CRS_), intent(inout) :: this
      real(real64), intent(in) :: u0(:), v0(:)
      real(real64), intent(in) :: h, t
      logical, optional, intent(in) :: debug

      integer(int32), optional, intent(in) :: itrmax
      real(real64), optional, intent(in) :: tol
      integer(int32), optional, intent(in) :: fix_idx(:)

      real(real64), allocatable:: Adu(:), Adv(:), u(:)
      real(real64) :: cos_coeff, sinc_coeff

      integer(int32) :: itr_max = 100
      real(real64)   :: itr_tol = dble(1.0e-16)
      logical :: debug_mode
      type(Math_) :: math
      integer(int32) :: n

      if (present(itrmax)) then
         itr_max = itrmax
      end if

      if (present(tol)) then
         itr_tol = tol
      end if

      if (present(fix_idx)) then
         call this%fix(fix_idx)
      end if
      ! a + 2 h M^{-1} v + M^{-1} K u = 0
      ! u(t) = exp(-ht)( cos(t*sqrt(M^{-1} K - h^2 I)  ) u
      !      + t*sinc( t*sqrt(M^{-1} K - h^2 I) ) v

      u = exp(-h*t)*this%tensor_cos_sqrt(v=u0, tol=itr_tol, itrmax=itr_max, &
                                         coeff=t, debug=debug, fix_idx=fix_idx) &
          + exp(-h*t)*t*this%tensor_sinc_sqrt(v=v0, tol=itr_tol, itrmax=itr_max, &
                                              coeff=t, debug=debug, fix_idx=fix_idx)

   end function

! ###################################################

   function tensor_wave_kernel_LPF_real_64_crs(this, u0, v0, tol, itrmax, h, t, fix_idx, debug, cutoff_frequency) result(u)
      class(CRS_), intent(in) :: this
      real(real64), intent(in) :: u0(:), v0(:)
      real(real64), allocatable:: Adu(:), Adv(:), u(:)
      real(real64) :: cos_coeff, sinc_coeff
      real(real64), intent(in) :: h, t, cutoff_frequency
      logical, optional, intent(in) :: debug

      integer(int32), optional, intent(in) :: itrmax
      real(real64), optional, intent(in) :: tol
      integer(int32), optional, intent(in) :: fix_idx(:)

      integer(int32) :: itr_max = 100
      real(real64)   :: itr_tol = dble(1.0e-16)
      logical :: debug_mode
      type(Math_) :: math
      integer(int32) :: n

      if (present(itrmax)) then
         itr_max = itrmax
      end if

      if (present(tol)) then
         itr_tol = tol
      end if

      ! a + 2 h M^{-1} v + M^{-1} K u = 0
      ! u(t) = exp(-ht)( cos(t*sqrt(M^{-1} K - h^2 I)  ) u
      !      + t*sinc( t*sqrt(M^{-1} K - h^2 I) ) v

      u = exp(-h*t)*this%tensor_cos_sqrt_LPF(v=u0, tol=itr_tol, itrmax=itr_max, &
                                             coeff=t, debug=debug, fix_idx=fix_idx, cutoff_frequency=cutoff_frequency) &
          + exp(-h*t)*this%tensor_t_sinc_sqrt_LPF(v=v0, tol=itr_tol, itrmax=itr_max, &
                                                  coeff=t, debug=debug, fix_idx=fix_idx, cutoff_frequency=cutoff_frequency)

   end function

! ###################################################

   function tensor_wave_kernel_RHS_real_64_crs(this, RHS, t, tol, itrmax, fix_idx, debug) result(u)
      class(CRS_), intent(in) :: this
      real(real64), intent(in) :: RHS(:)
      real(real64), allocatable:: u(:), du(:)
      real(real64) :: cos_coeff, sinc_coeff
      real(real64), intent(in) :: t
      logical, optional, intent(in) :: debug

      integer(int32), optional, intent(in) :: itrmax
      real(real64), optional, intent(in) :: tol
      integer(int32), optional, intent(in) :: fix_idx(:)

      integer(int32) :: itr_max = 100
      real(real64)   :: itr_tol = dble(1.0e-16)
      logical :: debug_mode
      type(Math_) :: math
      integer(int32) :: n

      if (present(itrmax)) then
         itr_max = itrmax
      end if

      if (present(tol)) then
         itr_tol = tol
      end if

      ! Force-induced displacement
      ! RHS is constant for interval [0, t]

      !u = -1.0d0/2.0d0*t*t*RHS
      u = 0.0d0*RHS
      du = -t*t/2.0d0*RHS
      u = u + du
      do n = 1, itr_max
         du = -t*t/dble(2*n + 1)/dble(2*n + 2)*this%matmul(du)
         u = u + du
         if (present(debug)) then
            if (debug) then
               print *, n, norm(du)
            end if
         end if
         if (norm(du) < itr_tol) exit
      end do

   end function

! ###################################################

   function tensor_wave_kernel_RHS_LPF_real_64_crs(this, RHS, t, tol, itrmax, fix_idx, debug, cutoff_frequency, weights) result(u)
      class(CRS_), intent(in) :: this
      real(real64), intent(in) :: RHS(:), cutoff_frequency
      real(real64), allocatable:: u(:), du(:)
      real(real64) :: cos_coeff, sinc_coeff
      real(real64), intent(in) :: t
      logical, optional, intent(in) :: debug

      integer(int32), optional, intent(in) :: itrmax
      real(real64), optional, intent(in) :: tol
      integer(int32), optional, intent(in) :: fix_idx(:)
      real(real64), optional, intent(in) :: weights(:)

      integer(int32) :: itr_max = 100
      real(real64)   :: itr_tol = dble(1.0e-16)
      real(real64)   :: ddt, hat_t
      logical :: debug_mode
      type(Math_) :: math
      integer(int32) :: n, m
      real(real64), allocatable :: w(:)

      integer(int32) :: lbd, ubd

      if (present(weights)) then
         ubd = size(weights) - (size(weights) + 1)/2
         lbd = -(size(weights) - (size(weights) + 1)/2)
         allocate (w(lbd:ubd))
         w(lbd:ubd) = weights(1:)
      else
         allocate (w(-1:1))
         w(-1) = 0.250d0
         w(0) = 0.500d0
         w(1) = 0.250d0
      end if

      if (present(itrmax)) then
         itr_max = itrmax
      end if

      if (present(tol)) then
         itr_tol = tol
      end if

      ! Force-induced displacement
      ! RHS is constant for interval [0, t]

      !ddt = 1.0d0/4.0d0/cutoff_frequency
      ddt = 1.0d0/cutoff_frequency/2.0d0/math%pi*acos(sqrt(2.0d0) - 1.0d0)

      !u = -1.0d0/2.0d0*t*t*RHS

      u = 0.0d0*RHS

      hat_t = 0.0d0
      do m = lbound(w, 1), ubound(w, 1)
         hat_t = hat_t + w(m)*(t + m*ddt)**2
      end do
      !hat_t = 0.250d0*(t-ddt)**2 &
      !    + 0.50d0*(t)**2 &
      !    + 0.250d0*(t+ddt)**2

      du = -1.0d0/2.0d0*RHS
      u = u + hat_t*du

      do n = 1, itr_max
         do m = lbound(w, 1), ubound(w, 1)
            hat_t = hat_t + w(m)*(t + m*ddt)**(2*n + 2)
         end do
         !hat_t = 0.250d0*(t-ddt)**(2*n+2) &
         !     + 0.50d0*(t)**(2*n+2)&
         !     + 0.250d0*(t+ddt)**(2*n+2)

         du = -1.0d0/dble(2*n + 1)/dble(2*n + 2)*this%matmul(du)
         u = u + hat_t*du
         if (present(debug)) then
            if (debug) then
               print *, n, norm(hat_t*du)
            end if
         end if
         if (norm(hat_t*du) < itr_tol) exit
      end do

   end function

! ###################################################

   function tensor_wave_kernel_RHS_complex_64_crs(this, RHS, t, tol, itrmax, fix_idx, debug) result(u)
      class(CRS_), intent(in) :: this
      complex(real64), intent(in) :: RHS(:)
      complex(real64), allocatable:: u(:), u_n(:), du(:)

      real(real64), intent(in) :: t
      logical, optional, intent(in) :: debug

      integer(int32), optional, intent(in) :: itrmax
      real(real64), optional, intent(in) :: tol
      integer(int32), optional, intent(in) :: fix_idx(:)

      integer(int32) :: itr_max = 100
      real(real64)   :: itr_tol = dble(1.0e-16)
      logical :: debug_mode
      type(Math_) :: math
      integer(int32) :: n

      if (present(itrmax)) then
         itr_max = itrmax
      end if

      if (present(tol)) then
         itr_tol = tol
      end if

      ! Force-induced displacement
      ! RHS is constant for interval [0, t]
      u = 0.0d0*RHS
      du = -t*t/2.0d0*RHS
      u = u + du
      do n = 1, itr_max
         du = -t*t/dble(2*n + 1)/dble(2*n + 2)*this%matmul(du)
         u = u + du
         if (present(debug)) then
            if (debug) then
               print *, n, sqrt(dble(dot_product(du, du)))
            end if
         end if
         if (sqrt(dble(dot_product(du, du))) < itr_tol) exit
      end do

   end function

! ###################################################

! ###################################################
   function tensor_d1_wave_kernel_complex64_crs(this, u, v, tol, itrmax, coeff) result(retA_v)
      class(CRS_), intent(in) :: this
      complex(real64), intent(in) :: u(:), v(:)
      complex(real64), optional, intent(in) :: coeff
      complex(real64), allocatable :: dv(:), retA_v(:), du(:)
      integer(int32) :: k
      integer(int32), intent(in) :: itrmax
      real(real64), intent(in) :: tol
      type(Math_) :: math
      complex(real64) :: a

      ! first derivative of wave kernel function
      ! v - t u w^2 - 1/2 t^2 (v w^2) + 1/6 t^3 u w^4 + 1/24 t^4 v w^4 - 1/120 t^5 (u w^6) + O(t^6)
      !(テイラー級数)
      ! sinc(sqrt(A) ) = \sum_{n=0}^{\infty}
      !c = 1.0d0
      ! k = 0

      if (present(coeff)) then
         a = coeff
      else
         a = 1.0d0
      end if

      !1,2
      du = this%matmul(u)
      retA_v = v - a*du

      !3
      dv = this%matmul(v)
      retA_v = retA_v - 1.0d0/2.0d0*a*a*dv

      !4
      du = this%matmul(du)
      retA_v = retA_v + 1.0d0/6.0d0*a*a*a*du

      !5
      dv = this%matmul(dv)
      retA_v = retA_v + 1.0d0/24.0d0*a*a*a*a*dv

      !6
      du = this%matmul(du)
      retA_v = retA_v - 1.0d0/120.0d0*a*a*a*a*a*du

      ! 6th-order apploximation

   end function
! #####################################################

! ###################################################
   function tensor_cos_sqrt_complex64_crs(this, v, tol, itrmax, coeff, debug, fix_idx) result(retA_v)
      class(CRS_), intent(in) :: this
      complex(real64), intent(in) :: v(:)
      complex(real64), allocatable :: dv(:), retA_v(:)
      complex(real64), optional, intent(in) :: coeff
      integer(int32), optional, intent(in) :: fix_idx(:)
      logical, optional, intent(in) :: debug
      integer(int32) :: k, row, CRS_id
      integer(int32), intent(in) :: itrmax
      real(real64), intent(in) :: tol
      type(Math_) :: math
      complex(real64) :: a

      if (present(coeff)) then
         a = coeff
      else
         a = 1.0d0
      end if
      ! sinc(sqrt(A) ) = \sum_{n=0}^{\infty}
      !c = 1.0d0
      ! k = 0

      !k=0
      k = 0
      retA_v = v
      ! zero-fixed Dirichlet boundary
      if (present(fix_idx)) then
         retA_v(fix_idx) = 0
      end if

      ! a=t

      ! k=1
      k = 1
      dv = this%matmul(v)
      dv = -0.50d0*a*a*dv

      ! zero-fixed Dirichlet boundary
      if (present(fix_idx)) then
         dv(fix_idx) = 0
      end if
      retA_v = retA_v + dv

      !

      do k = 2, itrmax
         ! k = k + 1
         dv = (a*a)*this%matmul(dv)/(2*k)/(2*k - 1)*(-1.0d0)

         ! zero-fixed Dirichlet boundary
         if (present(fix_idx)) then
            dv(fix_idx) = 0
         end if
         retA_v = retA_v + dv

         if (norm(abs(dv)) < tol) return

         if (present(debug)) then
            if (debug) then
               print *, k, norm(abs(dv))
            end if
         end if

      end do

   end function
! #####################################################

! #####################################################
   function wave_kernel_hanning_coefficient(dt, num_sample, t_power) result(ret)
      real(real64), intent(in)::dt
      integer, intent(in) :: num_sample, t_power
      integer(int32) :: m, k
      real(real64) :: ret, t, period_T
      type(Math_) :: math

      k = num_sample/2
      ret = 0.0d0
      period_T = 1.0d0/dt*2
      do m = -k, k
         t = -num_sample*dt + (m - 1)*dt
         ret = ret + (0.50d0 - 0.50d0*cos(2.0d0*math%pi*(t)/period_T)) &
               *(m*dt)**(t_power)
      end do
   end function
! #####################################################

! ###################################################
   function tensor_cos_sqrt_real64_crs(this, v, tol, itrmax, coeff, debug, fix_idx) result(retA_v)
      class(CRS_), intent(in) :: this
      real(real64), intent(in) :: v(:)
      real(real64), allocatable :: dv(:), retA_v(:)
      real(real64), optional, intent(in) :: coeff
      integer(int32), optional, intent(in) :: fix_idx(:)
      logical, optional, intent(in) :: debug
      integer(int32) :: k, row, CRS_id
      integer(int32), intent(in) :: itrmax
      real(real64), intent(in) :: tol
      type(Math_) :: math
      real(real64) :: a

      if (present(coeff)) then
         a = coeff
      else
         a = 1.0d0
      end if
      ! sinc(sqrt(A) ) = \sum_{n=0}^{\infty}
      !c = 1.0d0
      ! k = 0

      !k=0
      k = 0
      retA_v = v
      ! zero-fixed Dirichlet boundary
      if (present(fix_idx)) then
         retA_v(fix_idx) = 0
      end if

      if (present(debug)) then
         if (debug) then
            print *, k, norm(abs(v))

         end if
      end if

      ! k=1
      k = 1
      dv = this%matmul(v)
      dv = -0.50d0*a*a*dv

      ! zero-fixed Dirichlet boundary
      if (present(fix_idx)) then
         dv(fix_idx) = 0
      end if
      retA_v = retA_v + dv

      if (present(debug)) then
         if (debug) then
            print *, k, norm(abs(dv))
         end if
      end if

      do k = 2, itrmax
         ! k = k + 1
         dv = a*a*this%matmul(dv)/(2*k)/(2*k - 1)*(-1.0d0)

         ! zero-fixed Dirichlet boundary
         if (present(fix_idx)) then
            dv(fix_idx) = 0
         end if
         retA_v = retA_v + dv

         if (norm(abs(dv)) < tol) return

         if (present(debug)) then
            if (debug) then
               print *, k, norm(abs(dv))
            end if
         end if

      end do

   end function
! #####################################################

! ###################################################
   function tensor_cos_sqrt_LPF_real64_crs(this, v, tol, itrmax, coeff, debug, fix_idx, cutoff_frequency) result(retA_v)
      class(CRS_), intent(in) :: this
      real(real64), intent(in) :: v(:)
      real(real64), allocatable :: dv(:), retA_v(:)
      real(real64), optional, intent(in) :: coeff
      integer(int32), optional, intent(in) :: fix_idx(:)

      logical, optional, intent(in) :: debug
      integer(int32) :: k, row, CRS_id, num_hanning_sample
      integer(int32), intent(in) :: itrmax
      real(real64), intent(in) :: tol, cutoff_frequency
      type(Math_) :: math
      real(real64) :: a, dt, b, ddt

      !ddt = 1.0d0/(cutoff_frequency/4.0d0)
      !ddt = 1.0d0/cutoff_frequency/math%pi*acos( (sqrt(2.0d0)-1.0d0)/2.0d0 )
      ddt = 1.0d0/cutoff_frequency/2.0d0/math%pi*acos(sqrt(2.0d0) - 1.0d0)
      if (present(coeff)) then
         a = coeff
      else
         a = 1.0d0
      end if
      ! sinc(sqrt(A) ) = \sum_{n=0}^{\infty}
      !c = 1.0d0
      ! k = 0
      dt = a
      dv = v
      !k=0
      k = 0
      retA_v = v
      ! zero-fixed Dirichlet boundary
      if (present(fix_idx)) then
         retA_v(fix_idx) = 0
      end if

      do k = 1, itrmax
         ! k = k + 1

         !dv = a*a*this%matmul(dv)/(2*k)/(2*k-1)*(-1.0d0)
         dv = this%matmul(dv)

         ! zero-fixed Dirichlet boundary
         if (present(fix_idx)) then
            dv(fix_idx) = 0
         end if
         a = ((-1.0d0)**k)/gamma(2.0d0*k + 1.0d0)
         b = 0.250d0*((dt - ddt)**(2*k)) + 0.50d0*((dt)**(2*k)) + 0.250d0*((dt + ddt)**(2*k))
         retA_v = retA_v + a*b*dv

         if (norm(abs(a*b*dv)) < tol) return

         if (present(debug)) then
            if (debug) then
               print *, k, norm(abs(a*b*dv))
            end if
         end if

      end do

   end function
! #####################################################

! ###################################################
   function tensor_t_sinc_sqrt_LPF_real64_crs(this, v, tol, itrmax, coeff, debug, fix_idx, cutoff_frequency) &
      result(retA_v)
      class(CRS_), intent(in) :: this
      real(real64), intent(in) :: v(:)
      real(real64), optional, intent(in) :: coeff
      real(real64), allocatable :: dv(:), retA_v(:)
      integer(int32), optional, intent(in) :: fix_idx(:)
      integer(int32) :: k, num_hanning_sample
      integer(int32), intent(in) :: itrmax
      logical, optional, intent(in) :: debug
      real(real64), intent(in) :: tol, cutoff_frequency
      type(Math_) :: math
      real(real64) :: a, b, dt, ddt
      !num_hanning_sample = input(default=16,option=window_size)
      !ddt = 1.0d0/(cutoff_frequency/4.0d0)
      !ddt = 1.0d0/cutoff_frequency/math%pi*acos( (sqrt(2.0d0)-1.0d0)/2.0d0 )
      ddt = 1.0d0/cutoff_frequency/2.0d0/math%pi*acos(sqrt(2.0d0) - 1.0d0)
      if (present(coeff)) then
         a = coeff
      else
         a = 1.0d0
      end if
      ! sinc(sqrt(A) ) = \sum_{n=0}^{\infty}
      !c = 1.0d0
      ! k = 0
      dt = a

      !k=0
      k = 0
      retA_v = v
      dv = v

      if (present(fix_idx)) then
         dv(fix_idx) = 0
      end if

      retA_v = retA_v + dt*dv

      do k = 1, itrmax

         a = ((-1.0d0)**k)*(sqrt(math%pi)*2.0d0**(-1 - 2*k)) &
             /gamma(1.0d0 + k)/gamma(1.5d0 + k)
         b = 0.250d0*((dt - ddt)**(2*k + 1)) + 0.50d0*((dt)**(2*k + 1)) &
             + 0.250d0*((dt + ddt)**(2*k + 1))

         dv = this%matmul(dv)

         ! zero-fixed Dirichlet boundary
         if (present(fix_idx)) then
            dv(fix_idx) = 0
         end if

         retA_v = retA_v + a*b*dv

         if (norm(abs(a*b*dv)) < tol) return

         if (present(debug)) then
            if (debug) then
               print *, k, norm(abs(a*b*dv))
            end if
         end if

      end do

   end function
! #####################################################

! #####################################################
   subroutine fix_complex64_CRS(this, idx, val, RHS, only_row)
      class(CRS_), intent(inout) :: this
      integer(int32), intent(in) :: idx(:)
      complex(real64), optional, intent(inout) :: RHS(:)
      complex(real64), intent(in) :: val(:)
      integer(int64) :: i, j, k, id

      logical, optional, intent(in) :: only_row

      if (present(only_row)) then
         if (only_row) then

            do i = 1, size(idx)
               id = idx(i)
               do j = this%row_ptr(id), this%row_ptr(id + 1) - 1
                  if (this%col_idx(j) == id) then
                     this%val(j) = 1.0d0
                  else
                     this%val(j) = 0.0d0
                  end if
               end do
            end do
            return
         end if
      end if

      do i = 1, size(idx)
         id = idx(i)
         do j = 1, size(this%row_ptr) - 1
            if (j == id) then
               do k = this%row_ptr(j), this%row_ptr(j + 1) - 1
                  if (this%col_idx(k) == id) then
                     this%val(k) = 0.0d0
                  else
                     this%val(k) = 0.0d0
                  end if
               end do
            else
               do k = this%row_ptr(j), this%row_ptr(j + 1) - 1
                  if (this%col_idx(k) == id) then
                     RHS(j) = RHS(j) - this%val(k)*val(i)
                     this%val(k) = 0.0d0
                  end if
               end do
            end if
         end do
      end do

      do i = 1, size(idx)
         id = idx(i)
         do j = 1, size(this%row_ptr) - 1
            if (j == id) then
               do k = this%row_ptr(j), this%row_ptr(j + 1) - 1
                  if (this%col_idx(k) == id) then
                     this%val(k) = 1.0d0
                  end if
               end do
            end if
         end do
      end do

      do i = 1, size(idx)
         RHS(id) = val(i)
      end do
   end subroutine
! #####################################################

   subroutine removeCRS(this, idx)
      class(CRS_), intent(inout) :: this
      integer(int32), optional, intent(in) :: idx(:)
      integer(int64), allocatable :: num_col(:), col_idx(:), only_k(:), &
                                     copy_idx(:), row_ptr(:)
      real(real64), allocatable :: val(:)
      integer(int64) :: row, col_id, count_id, n, k, i, j

      if (.not. present(idx)) then
         if (allocated(this%col_idx)) then
            deallocate (this%col_idx)
         end if
         if (allocated(this%row_ptr)) then
            deallocate (this%row_ptr)
         end if
         if (allocated(this%val)) then
            deallocate (this%val)
         end if

         return
      end if

      copy_idx = idx

      ! remove row and column listed in idx(:)
      do i = 1, size(idx)
         k = maxval(copy_idx)

         num_col = int(zeros(this%size()))

         ! zerofill col_idx if row==k or col==k
         do row = 1, size(this%row_ptr) - 1
            num_col(row) = this%row_ptr(row + 1) - this%row_ptr(row)
            do col_id = this%row_ptr(row), this%row_ptr(row + 1) - 1
               if (row == k .or. this%col_idx(col_id) == k) then
                  this%col_idx(col_id) = 0
                  num_col(row) = num_col(row) - 1
               end if
            end do
         end do
         !call print(num_col)
         ! create new row_ptr
         row_ptr = int(zeros(this%size() - 1 + 1))
         row_ptr(1) = 1
         n = 0
         do row = 1, size(this%row_ptr) - 1
            if (row == k) then
               cycle
            else
               n = n + 1
               row_ptr(n + 1) = row_ptr(n) + num_col(row)
            end if
         end do

         ! remove zero
         col_idx = int(zeros(sum(num_col)))
         val = zeros(sum(num_col))
         n = 0
         do row = 1, k - 1
            do col_id = this%row_ptr(row), this%row_ptr(row + 1) - 1
               if (this%col_idx(col_id) /= 0) then

                  n = n + 1
                  col_idx(n) = this%col_idx(col_id)
                  val(n) = this%val(col_id)
               end if
            end do
         end do

         do row = k + 1, size(this%row_ptr) - 1
            do col_id = this%row_ptr(row), this%row_ptr(row + 1) - 1
               if (this%col_idx(col_id) /= 0) then
                  n = n + 1
                  col_idx(n) = this%col_idx(col_id)
                  val(n) = this%val(col_id)
               end if
            end do
         end do

         this%col_idx = col_idx
         do j = 1, size(this%col_idx)
            if (this%col_idx(j) >= k) then
               this%col_idx(j) = this%col_idx(j) - 1
            end if
         end do
         this%val = val
         this%row_ptr = row_ptr
         if (size(copy_idx) == 1) then
            exit
         else
            copy_idx = removeif(copy_idx, equal_to=k)
            cycle
         end if

      end do

   end subroutine

   function sum_sum(a, b, a_params, b_params, i_plus_j, ir, jr) result(ret)
      interface ! coefficient #1
         function a(k, params) result(ret)
            use iso_fortran_env
            integer(int32), intent(in) :: k
            complex(real64), intent(in) :: params(:)
            complex(real64) :: ret

         end function
      end interface

      interface ! coefficient #2
         function b(k, params) result(ret)
            use iso_fortran_env
            integer(int32), intent(in) :: k
            complex(real64), intent(in) :: params(:)
            complex(real64) :: ret

         end function
      end interface
      integer(int32), intent(in) :: i_plus_j ! i+j
      complex(real64), intent(in) :: a_params(:), b_params(:)
      integer(int32), intent(in) :: ir(1:2) ! range of i
      integer(int32), intent(in) :: jr(1:2) ! range of j
      integer(int32) :: i, j
      complex(real64) :: ret

      ret = 0.0d0

      do i = ir(1), i_plus_j
         j = i_plus_j - i
         if (j < jr(1) .or. jr(2) < j) cycle
         ret = ret + a(i, a_params)*b(j, b_params)
      end do

   end function

! ###################################################
   function tensor_cos_sqrt_cos_sqrt_complex64_crs(this, v, tol, itrmax, coeff_1, coeff_2) result(retA_v)
      class(CRS_), intent(in) :: this
      complex(real64), intent(in) :: v(:)
      complex(real64), allocatable :: dv(:), retA_v(:)
      complex(real64), intent(in) :: coeff_1, coeff_2
      integer(int32) :: k
      integer(int32), intent(in) :: itrmax
      real(real64), intent(in) :: tol
      type(Math_) :: math
      complex(real64) :: coeff

      do k = 0, itrmax
         ! k = k + 1
         coeff = sum_sum( &
                 a=cos_sqrt_Taylor, &
                 b=cos_sqrt_Taylor, &
                 a_params=[coeff_1], &
                 b_params=[coeff_2], &
                 i_plus_j=k, &
                 ir=[0, itrmax], &
                 jr=[0, itrmax] &
                 )
         if (k == 0) then
            dv = v
            retA_v = coeff*dv
         else
            dv = this%matmul(dv)
            retA_v = retA_v + coeff*dv
         end if

         if (norm(abs(dv)) < tol) return
      end do

   end function
! #####################################################

! ###################################################
   function tensor_sinc_sqrt_cos_sqrt_complex64_crs(this, v, tol, itrmax, coeff_1, coeff_2, debug) result(retA_v)
      class(CRS_), intent(in) :: this
      complex(real64), intent(in) :: v(:)
      complex(real64), allocatable :: dv(:), retA_v(:)
      complex(real64), intent(in) :: coeff_1, coeff_2
      logical, optional, intent(in) :: debug
      integer(int32) :: k

      integer(int32), intent(in) :: itrmax
      real(real64), intent(in) :: tol
      type(Math_) :: math
      complex(real64) :: coeff

      do k = 0, itrmax
         ! k = k + 1
         coeff = sum_sum( &
                 a=sinc_sqrt_Taylor, &
                 b=cos_sqrt_Taylor, &
                 a_params=[coeff_1], &
                 b_params=[coeff_2], &
                 i_plus_j=k, &
                 ir=[0, itrmax], &
                 jr=[0, itrmax] &
                 )
         if (k == 0) then
            dv = v
            retA_v = coeff*dv
         else
            dv = this%matmul(dv)
            retA_v = retA_v + coeff*dv
         end if

         if (norm(abs(dv)) < tol) return
         if (present(debug)) then
            if (debug) then
               print *, k, norm(abs(dv))
            end if
         end if

      end do

   end function
! #####################################################

! ###################################################
   function tensor_cos_sqrt_sinc_sqrt_complex64_crs(this, v, tol, itrmax, coeff_1, coeff_2) result(retA_v)
      class(CRS_), intent(in) :: this
      complex(real64), intent(in) :: v(:)
      complex(real64), allocatable :: dv(:), retA_v(:)
      complex(real64), intent(in) :: coeff_1, coeff_2
      integer(int32) :: k
      integer(int32), intent(in) :: itrmax
      real(real64), intent(in) :: tol
      type(Math_) :: math
      complex(real64) :: coeff

      do k = 0, itrmax
         ! k = k + 1
         coeff = sum_sum( &
                 a=cos_sqrt_Taylor, &
                 b=sinc_sqrt_Taylor, &
                 a_params=[coeff_1], &
                 b_params=[coeff_2], &
                 i_plus_j=k, &
                 ir=[0, itrmax], &
                 jr=[0, itrmax] &
                 )
         if (k == 0) then
            dv = v
            retA_v = coeff*dv
         else
            dv = this%matmul(dv)
            retA_v = retA_v + coeff*dv
         end if

         if (norm(abs(dv)) < tol) return
      end do

   end function
! #####################################################

! ###################################################
   function tensor_sinc_sqrt_sinc_sqrt_complex64_crs(this, v, tol, itrmax, coeff_1, coeff_2) result(retA_v)
      class(CRS_), intent(in) :: this
      complex(real64), intent(in) :: v(:)
      complex(real64), allocatable :: dv(:), retA_v(:)
      complex(real64), intent(in) :: coeff_1, coeff_2
      integer(int32) :: k
      integer(int32), intent(in) :: itrmax
      real(real64), intent(in) :: tol
      type(Math_) :: math
      complex(real64) :: coeff

      do k = 0, itrmax
         ! k = k + 1
         coeff = sum_sum( &
                 a=sinc_sqrt_Taylor, &
                 b=sinc_sqrt_Taylor, &
                 a_params=[coeff_1], &
                 b_params=[coeff_2], &
                 i_plus_j=k, &
                 ir=[0, itrmax], &
                 jr=[0, itrmax] &
                 )
         if (k == 0) then
            dv = v
            retA_v = coeff*dv
         else
            dv = this%matmul(dv)
            retA_v = retA_v + coeff*dv
         end if

         if (norm(abs(dv)) < tol) return
      end do

   end function
! #####################################################

   function cos_sqrt_Taylor(k, params) result(ret)
      integer(int32), intent(in) :: k
      complex(real64), intent(in) :: params(:)
      complex(real64) :: ret
      ! Taylor expanision coefficient for cos(a*sqrt(x) ) around x=0
      ret = ((-1.0d0)**k)*(params(1)**(2*k))/gamma(2.0d0*k + 1.0d0)

   end function

! #####################################################

   function sinc_sqrt_Taylor(k, params) result(ret)
      integer(int32), intent(in) :: k
      complex(real64), intent(in) :: params(:)
      complex(real64) :: ret
      type(Math_) :: math
      ! a = params(1)
      ! Taylor expanision coefficient for sinc(a*sqrt(x) ) around x=0
      ret = dble(2.0d0**(-1 - 2*k))*dble((-1.0d0)**k)*(params(1)**(2*k))*sqrt(math%pi) &
            /Gamma(1.0d0 + k*1.0d0)/Gamma(1.50d0 + k*1.0d0)
      !ret = dble(2.0d0**(-1-2*k))*dble((-1.0d0)**k)!*(params(1)**(2*k) )

   end function

! #####################################################

   function sinc_complex64(x) result(ret)
      complex(real64), intent(in) :: x
      complex(real64) :: ret
      if (abs(x) == 0.0d0) then
         ret = 1.0d0
      else
         ret = sin(x)/x
      end if
   end function

! #####################################################

   function sinc_real64(x) result(ret)
      real(real64), intent(in) :: x
      real(real64) :: ret
      if (abs(x) == 0.0d0) then
         ret = 1.0d0
      else
         ret = sin(x)/x
      end if
   end function
! #####################################################

   function sinc_real64_vec(x) result(ret)
      real(real64), intent(in) :: x(:)
      real(real64), allocatable :: ret(:)
      integer(int32) :: i

      allocate (ret(size(x)))

      do i = 1, size(x)
         ret(i) = sinc(x(i))
      end do
   end function

   function to_diag_vector_to_CRS(diag_vec) result(ret)
      real(real64), intent(in) :: diag_vec(:)
      type(CRS_) :: ret

      call ret%eyes(size(diag_vec))
      ret%val = diag_vec

   end function

! ###################################################
   subroutine to_real32_CRS(this)
      class(CRS_), intent(inout) :: this

      this%val_real32 = real(this%val)
      deallocate (this%val)
      this%dtype = real32

   end subroutine
! ###################################################

!! ###################################################
!function divide_by_diagonal_vector_CRS(this,diag_vector) result(ret)
!    type(CRS_),intent(in) :: this
!    type(CRS_) :: ret
!    real(real64),intent(in)  :: diag_vector(:)
!    integer(int32) :: i
!
!    ret = this
!    do i=1,ret%size()
!        ret%val(ret%row_ptr(i):ret%row_ptr(i+1)-1) = &
!            ret%val(ret%row_ptr(i):ret%row_ptr(i+1)-1)/diag_vector(i)
!    enddo
!end function
!! ###################################################
!
!function divide_by_scalar_CRS(this,scalar_value) result(ret)
!    type(CRS_),intent(in) :: this
!    real(real64),intent(in)  :: scalar_value
!    type(CRS_) :: ret
!
!    ret = this
!    ret%val = ret%val/scalar_value
!end function
!! ###################################################

! ###################################################
   function to_COO_from_DenseMatrix(dense_matrix) result(ret)
      type(COO_) :: ret
      real(real64), intent(in) :: dense_matrix(:, :)
      integer(int32) :: DOF, i, j

      DOF = size(dense_matrix, 1)
      call ret%init(DOF)
      do i = 1, size(dense_matrix, 1)
         do j = 1, size(dense_matrix, 1)
            call ret%add(i, j, dense_matrix(i, j))
         end do
      end do

   end function
! ###################################################

! ###################################################
   function to_COO_from_ArrayObject(arrayobject) result(ret)
      type(COO_) :: ret
      type(Array_), intent(in) :: arrayobject
      real(real64), allocatable :: dense_matrix(:, :)
      integer(int32) :: DOF, i, j

      dense_matrix = arrayobject
      DOF = size(dense_matrix, 1)
      call ret%init(DOF)
      do i = 1, size(dense_matrix, 1)
         do j = 1, size(dense_matrix, 1)
            if (dense_matrix(i, j) == 0.0d0) cycle
            call ret%add(i, j, dense_matrix(i, j))
         end do
      end do

   end function
! ###################################################

! ###################################################
   function to_CRS_from_DenseMatrix(dense_matrix) result(ret)
      type(COO_) :: coo
      type(CRS_) :: ret
      real(real64), intent(in) :: dense_matrix(:, :)

      coo = to_COO(dense_matrix)
      ret = coo%to_CRS()

   end function
! ###################################################

! ###################################################
   function to_CRS_from_ArrayObject(arrayobject) result(ret)
      type(COO_) :: coo
      type(CRS_) :: ret
      type(Array_), intent(in) :: arrayobject
      real(real64), allocatable :: dense_matrix(:, :)

      dense_matrix = arrayobject
      coo = to_COO(dense_matrix)
      ret = coo%to_CRS()

   end function
! ###################################################

! ###################################################
   function imag_ICOO(n) result(ret)
      integer(int32), intent(in) :: n
      type(COO_) :: ret
      integer(int32) :: i
      type(Math_) :: math

      call ret%init(n)
      if (n == 1) then
         call ret%add(1, 1, math%i)
         return
      end if

      if (mod(n, 2) == 0) then
         ! even
         do i = 1, n/2
            call ret%add(i, n - i + 1, -1.0d0)
         end do
         do i = n/2 + 1, n
            call ret%add(i, n - i + 1, 1.0d0)
         end do
      else
         ! odd
         ! even
         do i = 1, (n - 1)/2
            call ret%add(i, n, -1.0d0)
         end do
         call ret%add((n - 1)/2 + 1, n, math%i)
         do i = (n - 1)/2 + 2, n
            call ret%add(i, n, 1.0d0)
         end do
      end if

   end function
! ###################################################

! ###################################################
   function speyeCOO(n) result(ret)
      integeR(int32), intent(in) :: n
      type(COO_) :: ret

      call ret%eyes(n)

   end function
! ###################################################

   subroutine fillSymmetric_CRS(this)
      class(CRS_),intent(in) :: this
      ! fill-in to symmetric parts
      ! under implementation!


   end subroutine

end module SparseClass
