module MathClass
   use, intrinsic :: iso_fortran_env
   !use OouraFFT
   use StringClass
   implicit none

   integer(int32) :: i_i = 0
   integer(int32) :: j_j = 0
   integer(int32) :: k_k = 0

   logical :: true = .True.
   logical :: False = .False.
   !integer(int32) :: i_i = 0

   type :: Math_
      !real(real64) :: PI = 3.14159265358979323846d0
      real(real64) :: PI = 3.14159265358979323846d0
      real(real64) :: E = 2.718281828459045d0
      complex(kind(0d0))         :: i = (0.0d0, 1.0d0)
      complex(kind(0d0))         :: j = (0.0d0, 1.0d0)
   end type

   type :: Real64Ptr_
      real(real64), pointer :: ptr
   end type Real64Ptr_

   integer(int32), parameter :: complex64 = real64
   !real(real64) :: pi=3.141592653589793238d0
   !
   interface nchoosek
      module procedure comb
   end interface

   interface choose
      module procedure comb
   end interface

   interface fact
      module procedure factorialInt32, factorialReal64
   end interface

   interface imag
      module procedure imaginary_partComplex64, imaginary_partComplex32
   end interface

   interface arg
      module procedure arg_complex64, arg_complex64_vector, arg_complex64_tensor
   end interface

   interface norm
      module procedure norm_mat, norm_vec, norm_vec_real32, norm_vec_complex64
   end interface

   interface int
      module procedure fint
   end interface int

   interface float
      module procedure freal
   end interface float

   interface factorial
      module procedure factorialInt32, factorialReal64
   end interface factorial

   interface Bessel_J0
      module procedure Bessel_J0_complex
   end interface

   interface Bessel_J1
      module procedure Bessel_J1_complex
   end interface

   interface sort
      module procedure :: sort_int32
   end interface sort

   interface heapsort
      module procedure :: heapsortInt32, heapsortInt32Int32,heapsortReal64Int32,&
             heapsortReal64, heapsortReal32,&
            heapsort_int32_array,heapsort_real64_array
   end interface

   interface sort_and_remove_duplication
      module procedure  :: sort_and_remove_duplication_int32, sort_and_remove_duplication_real64
   end interface

   interface str
      module procedure fstring_Int, fstring_Int64, fstring_Real, fstring_Real32, &
         fstring_complex, fstring_Int_len, fstring_Real_len, fstring_logical, fstring_String, stringFromChar
   end interface str

   interface fstring
      module procedure fstring_Int, fstring_Int64, fstring_Real, fstring_Int_len, fstring_Real_len, fstring_logical
   end interface fstring

   interface int
      module procedure int_from_logical, int_from_logical_vector
   end interface int

   interface input
      module procedure input_Int, input_Real, input_Real32, input_Complex, input_IntVec, &
         input_RealVec, input_IntArray, input_RealArray, input_String, input_logical
   end interface input

   interface zeroif
      module procedure zeroif_Int, zeroif_Real
   end interface zeroif

   interface removeWord
      module procedure removeWord_String
   end interface

   interface tensor_product
      module procedure :: tensor_product_complex, tensor_product_real64
   end interface

   interface radian
      module procedure radianreal32, radianreal64, radianreal64Vec, radianint
   end interface

   interface array
      module procedure arrayDim1Real64, arrayDim2Real64, arrayDim3Real64
   end interface

   interface RickerFunction
      module procedure RickerFunctionReal64, RickerFunctionReal64Vector
   end interface

   interface derivative
      module procedure derivative_scalar, derivative_vector
   end interface

   interface der
      module procedure derivative_scalar, derivative_vector
   end interface

   interface d_dx
      module procedure derivative_scalar, derivative_vector
   end interface

   interface FFT
      module procedure FFT1D, FFT2D_real, FFT2D_comp, FFT_file_to_file
   end interface

   interface PSD
      module procedure PSD_file_to_file
   end interface

   interface SpectralWhitening
      module procedure SpectralWhitening_real64
   end interface

   interface exp
      module procedure matrix_exponential_real64
   end interface

   public :: assignment(=)
   interface assignment(=)
      module procedure assign_real64, assign_int32
   end interface

contains

   function SpectralWhitening_real64(x, auto) result(ret)
      real(real64), intent(in) :: x(:)
      complex(complex64), allocatable :: x_c(:)
      complex(complex64), allocatable :: f_c(:)
      real(real64), allocatable :: ret(:)
      real(real64) :: r, r_max, total_power, average_power
      integer(int32) :: i, n
      logical, optional, intent(in) :: auto

      ! FFT
      n = int(log10(dble(size(x)))/log10(2.0d0) + 0.00010d0)
      x_c = x(1:2**n)
      f_c = FFT(x_c)
      ! for each frequencies,
      if (present(auto)) then
         if (auto) then
            total_power = sum(abs(f_c(:))*abs(f_c(:)))
            average_power = total_power/size(f_c)
            r_max = sqrt(average_power)
         else
            r_max = 1.0d0
         end if
      else
         r_max = 1.0d0
      end if

      do i = 1, size(f_c)
         r = abs(f_c(i))
         f_c(i) = f_c(i)/r*r_max
      end do
      ret = dble(IFFT(f_c))

   end function

!
   function FFT_file_to_file(infile, outfile, window_size, dt, column, as_abs) result(FourierSpectrum)
      character(*), intent(in) :: infile, outfile
      integer(int32), intent(in) :: window_size, column
      real(real64), intent(in) :: dt
      real(real64) :: max_freq, min_freq
      logical, optional, intent(in) :: as_abs
      logical :: export_as_abs
      complex(real64), allocatable :: FourierSpectrum(:, :), x(:), FFT_X(:)

      integer(int32) :: ifile, ofile, i
      real(real64), allocatable :: line(:)

      allocate (line(column))

      open (newunit=ifile, file=infile, status='old')
      allocate (x(window_size))
      do i = 1, window_size
         read (ifile, *) line(1:column)
         x(i) = line(column)
      end do
      close (ifile)

      allocate (FourierSpectrum(window_size/2, 2))
      ! frequency axis
      max_freq = (1.0d0/dt)/2.0d0
      min_freq = 1.0d0/(window_size/(1.0d0/dt))
      do i = 1, window_size/2
         FourierSpectrum(i, 1) = min_freq + (i - 1)*(max_freq - min_freq)/(window_size/2)
      end do

      FFT_X = FFT(X)
      FourierSpectrum(:, 2) = FFT_X(1:window_size/2)

      export_as_abs = input(default=.false., option=as_abs)

      if (export_as_abs) then
         open (newunit=ofile, file=outfile, status='replace')
         do i = 1, window_size/2
            write (ofile, *) dble(FourierSpectrum(i, 1)), abs(FourierSpectrum(i, 2))
         end do
         close (ofile)
      else
         open (newunit=ofile, file=outfile, status='replace')
         do i = 1, window_size/2
            write (ofile, *) dble(FourierSpectrum(i, 1)), dble(FourierSpectrum(i, 2)), imag(FourierSpectrum(i, 2))
         end do
         close (ofile)
      end if

   end function

! ###############################################
   function PSD_file_to_file(infile, outfile, window_size, dt, column) result(FourierSpectrum)
      character(*), intent(in) :: infile, outfile
      integer(int32), intent(in) :: window_size, column
      real(real64) :: dt, max_freq, min_freq

      complex(real64), allocatable :: FourierSpectrum(:, :), x(:), FFT_X(:)

      integer(int32) :: ifile, ofile, i
      real(real64), allocatable :: line(:)

      allocate (line(column))

      open (newunit=ifile, file=infile, status='old')
      allocate (x(window_size))
      do i = 1, window_size
         read (ifile, *) line(1:column)
         x(i) = line(column)
      end do
      close (ifile)

      allocate (FourierSpectrum(window_size/2, 2))
      ! frequency axis
      max_freq = (1.0d0/dt)/2.0d0
      min_freq = 1.0d0/(window_size/(1.0d0/dt))
      do i = 1, window_size/2
         FourierSpectrum(i, 1) = min_freq + (i - 1)*(max_freq - min_freq)/(window_size/2)
      end do

      FFT_X = FFT(X)
      FourierSpectrum(:, 2) = FFT_X(1:window_size/2)

      open (newunit=ofile, file=outfile, status='replace')
      do i = 1, window_size/2
         write (ofile, *) dble(FourierSpectrum(i, 1)), abs(FourierSpectrum(i, 2)*conjg(FourierSpectrum(i, 2)))
      end do
      close (ofile)

   end function

! ###############################################
   function FFT2D_real(xy) result(hatx)
      real(real64), intent(in) :: xy(:, :)
      complex(real64), allocatable :: hatx(:, :), buf(:)
      integer(int32) :: row, i
      ! type(Math_) :: Math
      ! character(*),optional,intent(in) :: window
      ! real(real64),optional,intent(in) :: T(2) ! range
      ! real(real64) :: Trange(1:2),dt_x,dt_y
      ! integer(int32) :: Nx, Ny

      hatx = xy
      do row = 1, size(xy, 1)
         hatx(row, :) = FFT_core(hatx(row, :))
      end do
      do row = 1, size(xy, 1)
         buf = hatx(row, :)
         hatx(row, size(xy, 1)/2 + 1:size(xy, 1)) = buf(1:size(xy, 1)/2)
         do i = 1, size(xy, 1)/2
            hatx(row, i) = buf(size(xy, 1)/2 - i + 1)
         end do
      end do

      hatx = transpose(hatx)
      do row = 1, size(xy, 2)
         hatx(row, :) = FFT_core(hatx(row, :))
      end do
      do row = 1, size(xy, 2)
         buf = hatx(row, :)
         hatx(row, size(xy, 2)/2 + 1:size(xy, 2)) = buf(1:size(xy, 2)/2)
         do i = 1, size(xy, 2)/2
            hatx(row, i) = buf(size(xy, 2)/2 - i + 1)
         end do
      end do
      hatx = transpose(hatx)

   end function

! ###############################################

! ###############################################
   function FFT2D_comp(xy) result(hatx)
      complex(real64), intent(in) :: xy(:, :)
      complex(real64), allocatable :: hatx(:, :)
      integer(int32) :: row
      ! type(Math_) :: Math
      ! character(*),optional,intent(in) :: window
      ! real(real64),optional,intent(in) :: T(2) ! range
      ! real(real64) :: Trange(1:2),dt_x,dt_y
      ! integer(int32) :: Nx, Ny

      allocate (hatx(size(xy, 1), size(xy, 2)))
      do row = 1, size(xy, 1)
         hatx(row, :) = FFT_core(xy(row, :))
      end do
      hatx = transpose(hatx)
      do row = 1, size(xy, 2)
         hatx(row, :) = FFT_core(hatx(row, :))
      end do
      hatx = transpose(hatx)

   end function

! ###############################################

! ###############################################
   function FFT1D(x, T, window) result(hatx)
      complex(kind(0d0)), intent(in) :: x(:)
      complex(kind(0d0)), allocatable :: hatx(:)
      type(Math_) :: Math
      character(*), optional, intent(in) :: window
      real(real64), optional, intent(in) :: T(2) ! range
      real(real64) :: Trange(1:2), dt
      integer(int32) :: N

      N = size(x)
      if (present(T)) then
         dt = abs(T(2) - T(1))/dble(N)
      else
         dt = 1.0d0
      end if

      hatx = FFT_core(x)

      hatx = dt*hatx

   end function

! ###############################################

! Hanning window
   function hann(L) result(hann_window)
      integeR(int32), intent(in) :: L
      real(real64) :: hann_window(1:L)
      integer(int32) :: i, N
      type(Math_) :: math

      N = L - 1
      do i = 0, L - 1
         hann_window(i + 1) = 0.50d0*(1.0d0 - cos(math%PI*2.0d0*i/N))
      end do

   end function

! ###############################################
   recursive function FFT_core(x) result(hatx)
      complex(real64), intent(in) :: x(:)
      complex(real64), allocatable :: hatx(:), W(:), L(:), R(:)
      real(real64), allocatable :: a(:), wo(:)
      integer(int32), allocatable :: ip(:)
      integer(int32) :: N, i, itr, isgn
      integer(int32), allocatable :: k(:)
      type(Math_) :: Math
      ! This FFT is
      ! Fw(m dw) = T/N \sum_{n=0}^{N-1} f(n dt) e^{-i 2 \pi k/N m n}
      N = size(x)
      allocate (hatx(N))
      hatx(:) = 0.0d0
      allocate (k(N/2))
      allocate (W(N/2))
      allocate (L(N/2))
      allocate (R(N/2))
      do i = 1, size(k)
         k(i) = i - 1
         !print *, exp(-1*Math%i * 2.0d0* Math%PI * k(i)/dble(N))
         W(i) = exp(-1.0d0*Math%i*2.0d0*Math%PI*k(i)/dble(N))
      end do
      if (N == 2) then
         ! butterfly operation
         hatx(1) = x(1) + x(2)
         hatx(2) = x(1) - x(2)
         return
      end if
      if (N >= 4) then
         itr = 0
         do i = 1, N, 2
            itr = itr + 1

            if (itr > size(L)) then
               exit
            end if
            L(itr) = x(i)
         end do
         itr = 0
         do i = 2, N, 2
            itr = itr + 1
            if (itr > size(R)) then
               exit
            end if
            R(itr) = x(i)
         end do
         L = FFT_core(L)
         R = FFT_core(R)
         do i = 1, N/2
            hatx(i) = L(i) + W(i)*R(i)
         end do
         do i = N/2 + 1, N
            if (i - N/2 > size(L)) then
               exit
            end if
            hatx(i) = L(i - N/2) - W(i - N/2)*R(i - N/2)
         end do
         return
      end if
   end function
! ###############################################
   function IFFT(x, T) result(hatx)
      complex(kind(0d0)), intent(in) :: x(:)
      complex(kind(0d0)), allocatable :: hatx(:)
      type(Math_) :: Math
      real(real64), optional, intent(in) :: T(2) ! range
      real(real64) :: Trange(2), TT
      integer(int32) :: N

      ! This IFFT is
      ! ft(n dt) = 1/T \sum_{n=0}^{N-1} Fw(m dw) e^{i 2 \pi k/N m n}

      N = size(x)
      if (present(T)) then
         TT = abs(T(2) - T(1))
      else
         TT = dble(N)
      end if

      hatx = IFFT_core(x)

      hatx = 1.0d0/TT*hatx

   end function

! ###############################################
   recursive function IFFT_core(x) result(hatx)
      complex(kind(0d0)), intent(in) :: x(:)
      complex(kind(0d0)), allocatable :: hatx(:), W(:), L(:), R(:)
      real(real64), allocatable :: a(:), wo(:)
      integer(int32), allocatable :: ip(:)
      integer(int32) :: N, i, itr, isgn
      integer(int32), allocatable :: k(:)
      type(Math_) :: Math

        !!! call Ooura-FFT
      !n = size(x)/2
      !allocate(a(0:2*n-1) )
      !allocate(wo(0:2*n-1) )
      !a(0:2*n-1) = x(1:2*n)
      !isgn = n
      !call cdft(2*n,isgn,a(0:2*n-1),ip,wo(0:n/2-1) )
      !hatx = a
      !
      !return
        !!!
      N = size(x)

      allocate (hatx(N))

      hatx(:) = 0.0d0
      allocate (k(N/2))
      allocate (W(N/2))
      allocate (L(N/2))
      allocate (R(N/2))

      do i = 1, size(k)
         k(i) = i - 1
         !print *, exp(-1*Math%i * 2.0d0* Math%PI * k(i)/dble(N))
         W(i) = exp(Math%i*2.0d0*Math%PI*k(i)/dble(N))
      end do

      if (N == 2) then
         ! butterfly operation
         hatx(1) = x(1) + x(2)
         hatx(2) = x(1) - x(2)
         return
      end if

      if (N >= 4) then
         itr = 0
         do i = 1, N, 2
            itr = itr + 1

            if (itr > size(L)) then
               exit
            end if
            L(itr) = x(i)
         end do

         itr = 0
         do i = 2, N, 2
            itr = itr + 1
            if (itr > size(R)) then
               exit
            end if
            R(itr) = x(i)
         end do

         L = IFFT_core(L)
         R = IFFT_core(R)

         do i = 1, N/2
            hatx(i) = L(i) + W(i)*R(i)
         end do
         do i = N/2 + 1, N
            if (i - N/2 > size(L)) then
               exit
            end if
            hatx(i) = L(i - N/2) - W(i - N/2)*R(i - N/2)
         end do
         return
      end if

   end function
! ###############################################

! ###############################################
   function arrayDim1Real64(size1) result(ret)
      integer(int32), intent(in) :: size1
      real(real64), allocatable :: ret(:)

      allocate (ret(size1))

      ret(:) = 0.0d0

   end function
! ###############################################

! ###############################################
   function arrayDim2Real64(size1, size2) result(ret)
      integer(int32), intent(in) :: size1, size2
      real(real64), allocatable :: ret(:, :)

      allocate (ret(size1, size2))

      ret(:, :) = 0.0d0

   end function
! ###############################################

! ###############################################
   function arrayDim3Real64(size1, size2, size3) result(ret)
      integer(int32), intent(in) :: size1, size2, size3
      real(real64), allocatable :: ret(:, :, :)

      allocate (ret(size1, size2, size3))

      ret(:, :, :) = 0.0d0

   end function
! ###############################################

! ###############################################
   function radianreal32(deg) result(ret)
      real(real32), intent(in) :: deg
      real(real64) :: ret
      ret = deg/180.0d0*3.14159265358979323846d0
   end function
! ###############################################

! ###############################################
   function radianreal64(deg) result(ret)
      real(real64), intent(in) :: deg
      real(real64) :: ret
      ret = deg/180.0d0*3.14159265358979323846d0
   end function
! ###############################################

! ###############################################
   function radianreal64Vec(degs) result(ret)
      real(real64), intent(in) :: degs(:)
      integer(int32) :: i
      real(real64), allocatable :: ret(:)

      ret = degs

      do i = 1, size(degs)
         ret(i) = degs(i)/180.0d0*3.14159265358979323846d0
      end do

   end function
! ###############################################

! ###############################################
   function radianint(deg) result(ret)
      integer(int32), intent(in) :: deg
      real(real64) :: ret
      ret = dble(deg)/180.0d0*3.14159265358979323846d0
   end function
! ###############################################

! ###############################################
   function degrees(rad) result(ret)
      real(real64), intent(in) :: rad
      real(real64) :: ret
      ret = rad/3.14159265358979323846d0*180.0d0
   end function
! ###############################################

!########################################
   function norm_vec(vec) result(a)
      real(real64), intent(in)::vec(:)
      integer(int32) :: n
      real(real64) :: a

      n = size(vec)
      a = dsqrt(dot_product(vec, vec))

   end function
!########################################

!########################################
   function norm_vec_real32(vec) result(a)
      real(real32), intent(in)::vec(:)
      integer(int32) :: n
      real(real32) :: a

      n = size(vec)
      a = sqrt(dot_product(vec, vec))

   end function
!########################################

!########################################
   function norm_vec_complex64(vec) result(a)
      complex(real64), intent(in)::vec(:)
      integer(int32) :: n
      real(real64) :: a

      n = size(vec)
      a = sqrt(dble(dot_product(vec, vec)))

   end function
!########################################

!########################################
   function norm_mat(vec) result(a)
      real(real64), intent(in)::vec(:, :)
      integer(int32) :: n
      real(real64) :: a

      a = dsqrt(sum(vec*vec))

   end function
!########################################

!########################################
   pure function SearchNearestValueID(Vector, x) result(id)
      real(real64), intent(in) :: Vector(:)
      real(real64), intent(in) :: x
      integer(int32) :: id, i

      id = 1
      do i = 1, size(vector)
         if (abs(vector(id) - x) > abs(vector(i) - x)) then
            id = i
            cycle
         end if
      end do

   end function
!########################################

!########################################
   function SearchNearestValueIDs(Vector, x, num) result(id)
      real(real64), intent(in) :: Vector(:)
      real(real64), intent(in) :: x
      integer(int32), intent(in)  :: num
      integer(int32) :: id(num), i, j

      id(:) = 1
      do j = 1, num
         do i = 1, size(vector)
            if (j >= 2) then
               if (abs(minval(id(1:j - 1) - i)) == 0) cycle
            end if
            if (abs(vector(id(j)) - x) > abs(vector(i) - x)) then
               id(j) = i
               cycle
            end if
         end do
      end do
   end function
!########################################

!########################################
   function SearchNearestValue(Vector, x) result(val)
      real(real64), intent(in) :: Vector(:)
      real(real64), intent(in) :: x
      integer(int32) :: id, i
      real(real64) :: val

      id = 1
      do i = 1, size(vector)
         if (abs(vector(id) - x) > abs(vector(i) - x)) then
            id = i
            cycle
         end if
      end do

      val = vector(id)
   end function
!########################################

!########################################
   function SearchNearestCoord(Array, x) result(id)
      real(real64), intent(in) :: Array(:, :)
      real(real64), intent(in) :: x(:)
      integer(int32), allocatable::xr(:)

      integer(int32) :: i, id, n, m, norm, tr_norm

      n = size(Array, 1)
      m = size(Array, 2)
      if (m /= size(x)) then
         stop "ERROR :: SearchNearestCoord >> size(Array,2) should be =size(x)"
      end if

      allocate (xr(m))
      do i = 1, n
         xr(:) = Array(i, :)
         tr_norm = dot_product(xr - x, xr - x)
         if (i == 1) then
            norm = tr_norm
            id = i
         else
            if (norm > tr_norm) then
               norm = tr_norm
               id = i
            else
               cycle
            end if
         end if
      end do

   end function
!########################################

!##################################################
   function SearchIDIntVec(Vec, val) result(id_)
      integer(int32), intent(in) :: Vec(:)
      integer(int32), intent(in) :: val

      integer(int32) :: i, id_

      do i = 1, size(Vec)
         if (Vec(i) == val) then
            id_ = i
            return
         end if
      end do

   end function
!##################################################

!##################################################
   subroutine heapsortReal64(n, array, val)
      integer(int32), intent(in) :: n
      real(real64), intent(inout) :: array(1:n)! rearrange order by this array
      real(real64), optional, intent(inout) :: val(1:n) ! linked data
      real(real64) :: t_real
      integer(int32) ::i, k, j, l
      real(real64) :: t

      if (n .le. 0) then
         write (6, *) "Error, at heapsort"; stop
      end if
      if (n .eq. 1) return

      l = n/2 + 1
      k = n
      do while (k .ne. 1)
         if (l .gt. 1) then
            l = l - 1
            t = array(L)
            if (present(val)) then
               t_real = val(L)
            end if
         else
            t = array(k)
            if (present(val)) then
               t_real = val(k)
            end if

            array(k) = array(1)
            if (present(val)) then
               val(k) = val(1)
            end if

            k = k - 1
            if (k .eq. 1) then
               array(1) = t
               if (present(val)) then
                  val(1) = t_real
               end if
               exit
            end if
         end if
         i = l
         j = l + l
         do while (j .le. k)
            if (j .lt. k) then
               if (array(j) .lt. array(j + 1)) j = j + 1

            end if
            if (t .lt. array(j)) then
               array(i) = array(j)
               if (present(val)) then
                  val(i) = val(j)
               end if
               i = j
               j = j + j
            else
               j = k + 1
            end if
         end do
         array(i) = t
         if (present(val)) then
            val(i) = t_real
         end if
      end do

   end subroutine heapsortReal64

!##################################################
   subroutine heapsortReal32(n, array, val)
      integer(int32), intent(in) :: n
      real(real32), intent(inout) :: array(1:n)! rearrange order by this array
      real(real32), optional, intent(inout) :: val(1:n) ! linked data
      real(real32) :: t_real
      integer(int32) ::i, k, j, l
      real(real32) :: t

      if (n .le. 0) then
         write (6, *) "Error, at heapsort"; stop
      end if
      if (n .eq. 1) return

      l = n/2 + 1
      k = n
      do while (k .ne. 1)
         if (l .gt. 1) then
            l = l - 1
            t = array(L)
            if (present(val)) then
               t_real = val(L)
            end if
         else
            t = array(k)
            if (present(val)) then
               t_real = val(k)
            end if

            array(k) = array(1)
            if (present(val)) then
               val(k) = val(1)
            end if

            k = k - 1
            if (k .eq. 1) then
               array(1) = t
               if (present(val)) then
                  val(1) = t_real
               end if
               exit
            end if
         end if
         i = l
         j = l + l
         do while (j .le. k)
            if (j .lt. k) then
               if (array(j) .lt. array(j + 1)) j = j + 1

            end if
            if (t .lt. array(j)) then
               array(i) = array(j)
               if (present(val)) then
                  val(i) = val(j)
               end if
               i = j
               j = j + j
            else
               j = k + 1
            end if
         end do
         array(i) = t
         if (present(val)) then
            val(i) = t_real
         end if
      end do

   end subroutine heapsortReal32
!##################################################

!##################################################
   function sort_int32(vec) result(ret)
      integer(int32), intent(in) :: vec(:)
      integer(int32), allocatable :: ret(:)

      ret = vec
      call heapsortInt32(size(ret), ret)

   end function
!##################################################
pure   subroutine heapsortInt32(n, array, val)
      integer(int32), intent(in) :: n
      integer(int32), intent(inout) :: array(1:n)! rearrange order by this array
      real(real64), optional, intent(inout) :: val(1:n) ! linked data
      real(real64) :: t_real
      integer(int32) ::i, k, j, l
      integer(int32) :: t

      if (n .le. 0) then
         return
         !write (6, *) "Error, at heapsort"; stop
      end if
      if (n .eq. 1) return

      l = n/2 + 1
      k = n
      do while (k .ne. 1)
         if (l .gt. 1) then
            l = l - 1
            t = array(L)
            if (present(val)) then
               t_real = val(L)
            end if
         else
            t = array(k)
            if (present(val)) then
               t_real = val(k)
            end if

            array(k) = array(1)
            if (present(val)) then
               val(k) = val(1)
            end if

            k = k - 1
            if (k .eq. 1) then
               array(1) = t
               if (present(val)) then
                  val(1) = t_real
               end if
               exit
            end if
         end if
         i = l
         j = l + l
         do while (j .le. k)
            if (j .lt. k) then
               if (array(j) .lt. array(j + 1)) j = j + 1

            end if
            if (t .lt. array(j)) then
               array(i) = array(j)
               if (present(val)) then
                  val(i) = val(j)
               end if
               i = j
               j = j + j
            else
               j = k + 1
            end if
         end do
         array(i) = t
         if (present(val)) then
            val(i) = t_real
         end if
      end do

   end subroutine heapsortInt32

!##################################################
   pure  subroutine heapsortInt32Int32(n, array, val)
      integer(int32), intent(in) :: n
      integer(int32), intent(inout) :: array(1:n)! rearrange order by this array
      integer(int32), intent(inout) :: val(1:n) ! linked data
      integer(int32) :: t_real
      integer(int32) ::i, k, j, l
      integer(int32) :: t

      if (n .le. 0) then
         !write (6, *) "Error, at heapsort"; stop
         !print *, "Error, at heapsort"
         return
      end if
      if (n .eq. 1) return

      l = n/2 + 1
      k = n
      do while (k .ne. 1)
         if (l .gt. 1) then
            l = l - 1
            t = array(L)

            t_real = val(L)

         else
            t = array(k)

            t_real = val(k)

            array(k) = array(1)

            val(k) = val(1)

            k = k - 1
            if (k .eq. 1) then
               array(1) = t

               val(1) = t_real

               exit
            end if
         end if
         i = l
         j = l + l
         do while (j .le. k)
            if (j .lt. k) then
               if (array(j) .lt. array(j + 1)) j = j + 1

            end if
            if (t .lt. array(j)) then
               array(i) = array(j)
               val(i) = val(j)

               i = j
               j = j + j
            else
               j = k + 1
            end if
         end do
         array(i) = t

         val(i) = t_real

      end do

   end subroutine heapsortInt32Int32
!##################################################
   pure  subroutine heapsortReal64Int32(n, array, val)
      integer(int32), intent(in) :: n
      real(real64), intent(inout) :: array(1:n)! rearrange order by this array
      integer(int32), intent(inout) :: val(1:n) ! linked data
      real(real64) :: t_real
      integer(int32) ::i, k, j, l
      real(real64) :: t

      if (n .le. 0) then
         !write (6, *) "Error, at heapsort"; stop
         !print *, "Error, at heapsort"
         return
      end if
      if (n .eq. 1) return

      l = n/2 + 1
      k = n
      do while (k .ne. 1)
         if (l .gt. 1) then
            l = l - 1
            t = array(L)

            t_real = val(L)

         else
            t = array(k)

            t_real = val(k)

            array(k) = array(1)

            val(k) = val(1)

            k = k - 1
            if (k .eq. 1) then
               array(1) = t

               val(1) = t_real

               exit
            end if
         end if
         i = l
         j = l + l
         do while (j .le. k)
            if (j .lt. k) then
               if (array(j) .lt. array(j + 1)) j = j + 1

            end if
            if (t .lt. array(j)) then
               array(i) = array(j)
               val(i) = val(j)

               i = j
               j = j + j
            else
               j = k + 1
            end if
         end do
         array(i) = t

         val(i) = t_real

      end do

   end subroutine heapsortReal64Int32


   !==========================================================
!calculate cross product
!---------------------------
   pure function cross_product(a, b) result(c)
      real(real64), intent(in) :: a(:), b(:)
      real(real64), allocatable :: c(:)

      if (size(a) /= 3 .or. size(b) /= 3) then
         !stop  "wrong number on size a, b"
         return
      end if

      allocate (c(size(a, 1)))
      if (size(c, 1) == 3) then
         c(1) = a(2)*b(3) - a(3)*b(2)
         c(2) = a(3)*b(1) - a(1)*b(3)
         c(3) = a(1)*b(2) - a(2)*b(1)
      else
         !stop "wrong number at cross_product"
         return
      end if

   end function cross_product
!=========================================================
!calculate diadic
!----------------------
   function diadic(a, b) result(c)
      real(real64), intent(in) :: a(:), b(:)
      real(real64), allocatable :: c(:, :)

      integer(int32) n, i, j

      allocate (c(size(a), size(b)))
      do i = 1, size(a)
         do j = 1, size(b)
            c(i, j) = a(i)*b(j)
         end do
      end do

   end function diadic
!==========================================================
!=========================================================
!calculate diadic
!----------------------
   function tensor_product_real64(a, b) result(c)
      real(real64), intent(in) :: a(:), b(:)
      real(real64), allocatable :: c(:, :)

      integer(int32) n, i, j

      allocate (c(size(a), size(b)))
      do i = 1, size(a)
         do j = 1, size(b)
            c(i, j) = a(i)*b(j)
         end do
      end do

   end function
   !==========================================================

!=========================================================
!calculate diadic
!----------------------
   function tensor_product_complex(a, b) result(c)
      complex(real64), intent(in) :: a(:), b(:)
      complex(real64), allocatable :: c(:, :)

      integer(int32) n, i, j

      allocate (c(size(a), size(b)))
      do i = 1, size(a)
         do j = 1, size(b)
            c(i, j) = a(i)*b(j)
         end do
      end do

   end function
   !==========================================================

   !calculate gz
!--------------
   subroutine calcgz(x2, x11, x12, nod_coord, gzi)
      real(real64), intent(in) :: nod_coord(:, :)
      real(real64), intent(out) :: gzi
      integer(int32), intent(in):: x2, x11, x12
      real(real64) l
      real(real64), allocatable::avec(:)

      allocate (avec(2))
      l = dot_product(nod_coord(x12, 1:2) - nod_coord(x11, 1:2), &
                      nod_coord(x12, 1:2) - nod_coord(x11, 1:2))
      l = l**(1.0d0/2.0d0)

      avec(1:2) = (nod_coord(x12, 1:2) - nod_coord(x11, 1:2))/l

      if (l == 0.0d0) then
         print *, "calcgz l=0"
         gzi = 0.0d0
      else
         gzi = 1.0d0/l*dot_product(nod_coord(x2, 1:2) - nod_coord(x11, 1:2), avec(1:2))
      end if

      deallocate (avec)

   end subroutine calcgz
!==========================================================
   function arg_complex64(comp) result(theta)
      complex(real64), intent(in) :: comp
      real(real64) :: theta, re, im
      real(real64) ::pi = 3.14159265358979323846d0

      re = dble(real(comp))
      im = dble(aimag(comp))

      if (re > 0.0d0) then
         theta = atan(im/re)
      elseif (re < 0.0d0 .and. im >= 0.0d0) then
         theta = atan(im/re + pi)
      elseif (re < 0.0d0 .and. im < 0.0d0) then
         theta = atan(im/re - pi)
      elseif (re == 0.0d0 .and. im > 0.0d0) then
         theta = pi/2.0d0
      elseif (re == 0.0d0 .and. im < 0.0d0) then
         theta = -pi/2.0d0
      else
         theta = 0.0d0
      end if

   end function
!==========================================================
!==========================================================
   function arg_complex64_vector(comp) result(theta)
      complex(real64), intent(in) :: comp(:)
      real(real64), allocatable :: theta(:)
      integer(int32) :: i

      allocate (theta(size(comp)))
      do i = 1, size(comp)
         theta(i) = arg_complex64(comp(i))
      end do

   end function
!==========================================================

!==========================================================
   function arg_complex64_tensor(comp) result(theta)
      complex(real64), intent(in) :: comp(:, :)
      real(real64), allocatable :: theta(:, :)
      integer(int32) :: i, j

      allocate (theta(size(comp, 1), size(comp, 2)))
      do i = 1, size(comp, 2)
         do j = 1, size(comp, 1)
            theta(j, i) = arg_complex64(comp(j, i))
         end do
      end do

   end function
!==========================================================

   function cubic_equation(a, b, c, d) result(x)
      real(real64), intent(in) :: a, b, c, d
      real(real64) :: x(3), theta
      real(real64) ::Deq, A_, B_, C_, p, q
      real(real64) ::pi = 3.14159265358979323846d0
      complex(real64)  :: comp
      !https://qiita.com/yotapoon/items/42b1749b69c264d6f486

      A_ = b/a
      B_ = c/a
      C_ = d/a
      p = B_ - A_*A_/3.0d0
      q = 2.0d0*A_*A_*A_/27.0d0 - A_*B_/3.0d0 + C_
      Deq = q*q/4.0d0 + p*p*p/27.0d0

      if (Deq > 0.0d0) then
         print *, "D > 0 :: not implemented now."
      elseif (Deq == 0) then
         print *, "D == 0 "
         x(1) = -2.0d0*(p/2.0d0)**(3)
         x(2) = (p/2.0d0)**(3)
         x(3) = (p/2.0d0)**(3)
         return
      else
         print *, "D < 0 "
         comp = cmplx(-q/2.0d0, sqrt(-Deq))
         theta = arg(comp)
         x(1) = 2.0d0*sqrt(-p/3.0d0)*cos(theta)
         x(2) = 2.0d0*sqrt(-p/3.0d0)*cos((theta + 2.0d0*pi)/3.0d0)
         x(3) = 2.0d0*sqrt(-p/3.0d0)*cos((theta + 4.0d0*pi)/3.0d0)
      end if

   end function

!==========================================================
   subroutine eigen_2d(Amat, eigenvector)
      real(real64), intent(in)::Amat(:, :)
      real(real64), allocatable, intent(inout)::eigenvector(:, :)
      real(real64)::b, c, phy, eigenvalue(2)
      integer(int32) i, j

      eigenvalue = array(size(Amat, 1))
      eigenvector = array(size(Amat, 1), size(Amat, 1))

      b = -1.0d0*(Amat(1, 1) + Amat(2, 2))
      c = Amat(1, 1)*Amat(2, 2) - Amat(1, 2)*Amat(1, 2)

      if (Amat(1, 2) /= Amat(2, 1)) then
         stop "input matrice is not symmetric"
      end if

      do i = 1, 2
         eigenvalue(i) = (-1.0d0*b + ((-1.0d0)**dble(i))*(b*b - 4.0d0*c)**(1.0d0/2.0d0))*(0.50d0)
      end do

      do i = 1, 2
         if (Amat(1, 2) == 0) then
            cycle
         elseif (Amat(1, 2) /= 0) then
            phy = atan((eigenvalue(i) - Amat(1, 1))/Amat(1, 2))

            do j = 1, 2
               eigenvector(i, 1:2) = (/cos(phy), sin(phy)/)
            end do
         end if
      end do

      do i = 1, 2
         eigenvector(i, :) = eigenvalue(i)*eigenvector(i, :)
      end do
   end subroutine eigen_2d
!==========================================================
   function signmm(a) result(b)
      real(real64), intent(in)::a
      real(real64) b

      if (a > 0) then
         b = 1.0d0
      elseif (a < 0) then
         b = -1.0d0
      elseif (a == 0) then
         b = 0.0d0
      else
         stop "ERROR: Invalid real(real64) in function_signm"
      end if

   end function signmm
!==========================================================

! ################################################################
! From 数値計算のためのFortran90/95プログラミング入門 単行本（ソフトカバー） –
! This function is not presented with GPL or any licenses.
! this function will be replaced by LAPACK.

   recursive function det_mat(a, n) result(det)
      integer(int32), intent(in) :: n
      real(real64), intent(in) :: a(n, n)
      real(real64) det, b(n - 1, n - 1)
      integer(int32) i
      if (n > 1) then
         det = 0.0d0
         do i = 1, n
            b(1:i - 1, 1:n - 1) = a(1:i - 1, 2:n)
            b(i:n - 1, 1:n - 1) = a(i + 1:n, 2:n)
            det = det + (-1.0d0)**(i + 1) &
                  *a(i, 1)*det_mat(b, n - 1)
         end do
      else
         det = a(1, 1)
      end if
   end function det_mat
!=====================================================================================

!==========================================================
   recursive function det(a, n) result(det_v)
      integer(int32), intent(in) :: n
      real(real64), intent(in) :: a(n, n)
      real(real64) det_v, b(n - 1, n - 1)
      integer(int32) i
      if (n > 1) then
         det_v = 0.0d0
         do i = 1, n
            b(1:i - 1, 1:n - 1) = a(1:i - 1, 2:n)
            b(i:n - 1, 1:n - 1) = a(i + 1:n, 2:n)
            det_v = det_v + (-1.0d0)**(i + 1) &
                    *a(i, 1)*det(b, n - 1)

         end do
      else
         det_v = a(1, 1)
      end if
   end function det
!=====================================================================================
   subroutine trans_rank_2(A, A_T)
      real(real64), intent(in)::A(:, :)
      real(real64), allocatable, intent(out)::A_T(:, :)
      integer(int32) n, m, i, j

      n = size(A, 1)
      m = size(A, 2)
      if (.not. allocated(A_T)) allocate (A_T(m, n))

      do i = 1, n
         do j = 1, m
            A_T(j, i) = A(i, j)
         end do
      end do

   end subroutine trans_rank_2
!==================================================================================
   function trans1(A) result(A_T)
      real(real64), intent(in)::A(:)
      real(real64), allocatable::A_T(:, :)
      integer(int32) n, m, i, j

      n = size(A)
      if (.not. allocated(A_T)) allocate (A_T(1, n))

      do i = 1, n
         A_T(1, i) = A(i)
      end do

   end function trans1
!==================================================================================
   function trans2(A) result(A_T)
      real(real64), intent(in)::A(:, :)
      real(real64), allocatable::A_T(:, :)
      integer(int32) n, m, i, j

      n = size(A, 1)
      m = size(A, 2)
      if (.not. allocated(A_T)) allocate (A_T(m, n))

      do i = 1, n
         do j = 1, m
            A_T(j, i) = A(i, j)
         end do
      end do

   end function trans2
!==================================================================================
   subroutine inverse_rank_2(A, A_inv)
      real(real64), intent(in)::A(:, :)
      real(real64), allocatable::A_inv(:, :)
      real(real64) detA, detA_1
      integer(int32) m, n

      m = size(A, 1)
      n = size(A, 2)
      if (.not. allocated(A_inv)) allocate (A_inv(m, n))
      detA = det_mat(A, n)
      if (detA == 0.0d0) stop "ERROR: inverse, detA=0"
      detA_1 = 1.0d0/detA
      if (n == 2) then
         A_inv(1, 1) = detA_1*A(2, 2)
         A_inv(1, 2) = -detA_1*A(1, 2)
         A_inv(2, 1) = -detA_1*A(2, 1)
         A_inv(2, 2) = detA_1*A(1, 1)
      elseif (n == 3) then
         A_inv(1, 1) = detA_1*(A(2, 2)*A(3, 3) - A(2, 3)*A(3, 2))
         A_inv(1, 2) = detA_1*(A(1, 3)*A(3, 2) - A(1, 2)*A(3, 3))
         A_inv(1, 3) = detA_1*(A(1, 2)*A(2, 3) - A(1, 3)*A(2, 2))
         A_inv(2, 1) = detA_1*(A(2, 3)*A(3, 1) - A(2, 1)*A(3, 3))
         A_inv(2, 2) = detA_1*(A(1, 1)*A(3, 3) - A(1, 3)*A(3, 1))
         A_inv(2, 3) = detA_1*(A(1, 3)*A(2, 1) - A(1, 1)*A(2, 3))
         A_inv(3, 1) = detA_1*(A(2, 1)*A(3, 2) - A(2, 2)*A(3, 1))
         A_inv(3, 2) = detA_1*(A(1, 2)*A(3, 1) - A(1, 1)*A(3, 2))
         A_inv(3, 3) = detA_1*(A(1, 1)*A(2, 2) - A(1, 2)*A(2, 1))
      else
         print *, "ERROR: Aij with i=j=", n, "/=2or3"
      end if

   end subroutine inverse_rank_2
!==================================================================================
!==================================================================================
   function inverse(A) result(A_inv)
      real(real64), intent(in)::A(:, :)
      real(real64), allocatable::A_inv(:, :)
      real(real64) detA, detA_1
      integer(int32) m, n

      m = size(A, 1)
      n = size(A, 2)
      if (.not. allocated(A_inv)) allocate (A_inv(m, n))
      detA = det_mat(A, n)
      if (detA == 0.0d0) stop "ERROR: inverse, detA=0"
      detA_1 = 1.0d0/detA
      if (n == 2) then
         A_inv(1, 1) = detA_1*A(2, 2)
         A_inv(1, 2) = -detA_1*A(1, 2)
         A_inv(2, 1) = -detA_1*A(2, 1)
         A_inv(2, 2) = detA_1*A(1, 1)
      elseif (n == 3) then
         A_inv(1, 1) = detA_1*(A(2, 2)*A(3, 3) - A(2, 3)*A(3, 2))
         A_inv(1, 2) = detA_1*(A(1, 3)*A(3, 2) - A(1, 2)*A(3, 3))
         A_inv(1, 3) = detA_1*(A(1, 2)*A(2, 3) - A(1, 3)*A(2, 2))
         A_inv(2, 1) = detA_1*(A(2, 3)*A(3, 1) - A(2, 1)*A(3, 3))
         A_inv(2, 2) = detA_1*(A(1, 1)*A(3, 3) - A(1, 3)*A(3, 1))
         A_inv(2, 3) = detA_1*(A(1, 3)*A(2, 1) - A(1, 1)*A(2, 3))
         A_inv(3, 1) = detA_1*(A(2, 1)*A(3, 2) - A(2, 2)*A(3, 1))
         A_inv(3, 2) = detA_1*(A(1, 2)*A(3, 1) - A(1, 1)*A(3, 2))
         A_inv(3, 3) = detA_1*(A(1, 1)*A(2, 2) - A(1, 2)*A(2, 1))
      else
         print *, "ERROR: Aij with i=j=", n, "/=2or3"
      end if

   end function inverse
!==================================================================================
   subroutine tensor_exponential(A, expA, TOL, itr_tol)
      real(real64), intent(in)::A(:, :), TOL
      real(real64), allocatable, intent(inout)::expA(:, :)
      integer(int32), intent(in)::itr_tol
      real(real64), allocatable::increA(:, :)
      real(real64) increment, NN
      integer(int32) i, j, n

      if (.not. allocated(expA)) allocate (expA(size(A, 1), size(A, 2)))
      allocate (increA(size(A, 1), size(A, 2)))
      if (size(A, 1) /= size(A, 2)) stop "ERROR:tensor exp is not a square matrix"

      expA(:, :) = 0.0d0
      do n = 1, size(expA, 1)
         expA(n, n) = 1.0d0
      end do
      NN = 1.0d0
      increA(:, :) = expA(:, :)
      do n = 1, itr_tol
         if (n > 1) then
            NN = NN*(NN + 1.0d0)
         end if
         increA(:, :) = matmul(increA, A)
         expA(:, :) = expA(:, :) + 1.0d0/NN*increA(:, :)

         increment = 0.0d0
         do i = 1, size(A, 1)
            do j = 1, size(A, 2)
               increment = increment + 1.0d0/NN*increA(i, j)*increA(i, j)
            end do
         end do

         if (increment <= TOL) then
            exit
         else
            if (n >= itr_tol) then
               stop "tensor exponential is not converged"
            end if
            cycle
         end if
      end do

      deallocate (increA)

   end subroutine tensor_exponential
!==================================================================================
   function identity_matrix(n) result(mat)
      integer(int32), intent(in) :: n ! rank
      real(real64) :: mat(n, n)
      integer(int32) :: i
      mat(:, :) = 0.0d0
      do i = 1, n
         mat(i, i) = 1.0d0
      end do

   end function
!==================================================================================

!==================================================================================
   function zero_matrix(n) result(mat)
      integer(int32), intent(in) :: n ! rank
      real(real64) :: mat(n, n)
      mat(:, :) = 0.0d0

   end function
!==================================================================================
   subroutine tensor_expo_der(A, expA_A, TOL, itr_tol)
      real(real64), intent(in)::A(:, :), TOL
      real(real64), allocatable, intent(inout)::expA_A(:, :, :, :)
      integer(int32), intent(in)::itr_tol
      real(real64), allocatable::increA_1(:, :), increA_2(:, :), increA_3(:, :, :, :), I_ij(:, :), A_inv(:, :)
      real(real64) increment, NN
      integer(int32) i, j, k, l, n, m, o

      if (.not. allocated(expA_A)) allocate (expA_A(size(A, 1), size(A, 1), size(A, 1), size(A, 1)))
      allocate (I_ij(size(A, 1), size(A, 1)))
      allocate (increA_1(size(A, 1), size(A, 1)))
      allocate (increA_2(size(A, 1), size(A, 1)))
      allocate (increA_3(size(A, 1), size(A, 1), size(A, 1), size(A, 1)))
      if (size(A, 1) /= size(A, 2)) stop "ERROR:tensor exp is not a square matrix"

      call inverse_rank_2(A, A_inv)

      I_ij(:, :) = 0.0d0
      do n = 1, size(expA_A, 1)
         I_ij(n, n) = 1.0d0
      end do
      NN = 1.0d0

      do i = 1, size(A, 1)
         do j = 1, size(A, 1)
            do k = 1, size(A, 1)
               do l = 1, size(A, 1)
                  expA_A(i, j, k, l) = I_ij(i, k)*I_ij(l, j)
               end do
            end do
         end do
      end do

      increA_1(:, :) = I_ij(:, :)
      increA_2(:, :) = I_ij(:, :)
      do n = 1, itr_tol
         if (n > 2) then
            NN = NN*(NN + 1.0d0)
         end if
         increA_1(:, :) = A_inv(:, :)
         increA_2(:, :) = matmul(increA_2, A)

         increA_3(:, :, :, :) = 0.0d0
         do m = 1, n
            increA_1(:, :) = matmul(increA_1, A)

            increA_2(:, :) = matmul(increA_2, A_inv)

            do i = 1, size(A, 1)
               do j = 1, size(A, 1)
                  do k = 1, size(A, 1)
                     do l = 1, size(A, 1)
                        increA_3(i, j, k, l) = increA_3(i, j, k, l) + increA_1(i, k)*increA_2(l, j)
                        expA_A(i, j, k, l) = expA_A(i, j, k, l) + 1.0d0/NN*increA_3(i, j, k, l)
                     end do
                  end do
               end do
            end do
         end do

         do i = 1, size(A, 1)
            do j = 1, size(A, 1)
               do k = 1, size(A, 1)
                  do l = 1, size(A, 1)
                     increment = increment + 1.0d0/NN*increA_3(i, j, k, l) &
                                 *increA_3(i, j, k, l) &
                                 *increA_3(i, j, k, l) &
                                 *increA_3(i, j, k, l)
                  end do
               end do
            end do
         end do

         if (increment <= TOL) then
            exit
         else
            if (n >= itr_tol) then
               stop "tensor exponential is not converged"
            end if
            cycle
         end if
      end do

      deallocate (increA_1, increA_2, increA_3, I_ij, A_inv)

   end subroutine tensor_expo_der
!==================================================================================

   function GetNormRe(a) result(b)
      real(real64), intent(in)::a(:)
      real(real64) :: b
      b = dot_product(a, a)
   end function
!==================================================================================

   function GetNormMatRe(a) result(b)
      real(real64), intent(in)::a(:, :)
      real(real64) :: b
      integer(int32) :: i, j
      b = 0
      do i = 1, size(a, 1)
         do j = 1, size(a, 2)
            b = b + a(i, j)*a(i, j)
         end do
      end do
   end function
!==================================================================================

   function trace(a) result(b)
      real(real64), intent(in)::a(:, :)
      real(real64) :: b
      integer(int32) :: i, j
      b = 0
      do i = 1, size(a, 1)
         b = b + a(i, i)
      end do
   end function

!==================================================================================
   function sym(a, n) result(ret)
      real(real64), intent(in) :: a(:, :)
      real(real64) :: ret(n, n)
      integer(int32) :: i, n

      ret = 0.50d0*(a) + 0.50d0*transpose(a)

   end function
!==================================================================================

!==================================================================================
   function asym(a, n) result(ret)
      real(real64), intent(in) :: a(:, :)
      real(real64) :: ret(n, n)
      integer(int32) :: i, n

      ret = 0.50d0*(a) - 0.50d0*transpose(a)

   end function
!==================================================================================

   function pi_value(n) result(res)
      integer(int32), intent(in)::n
      real(real64) :: ptr
      real(real64) :: an, bn, tn, pn
      real(real64) :: atr, btr, ttr
      real(real64) :: res

      integer(int32) :: i

      an = 1.0d0
      bn = 1.0d0/sqrt(2.0d0)
      tn = 0.250d0
      pn = 1.00d0
      do i = 1, n
         atr = 0.50d0*(an + bn)
         btr = dsqrt(an*bn)
         ttr = tn - pn*(atr - an)*(atr - an)
         ptr = 2.0d0*pn

         an = atr
         bn = btr
         tn = ttr
         pn = ptr

         res = (atr + btr)*(atr + btr)/4.0d0/ttr
      end do

   end function
!==================================================================================

!==================================================================================
   pure function fstring_int(x) result(a)
      integer(int32), intent(in) :: x
      character(len=20):: b
      character(len=:), allocatable        :: a

      write (b, *) x
      a = trim(adjustl(b))

   end function
!==================================================================================

!==================================================================================
   pure function fstring_int64(x) result(a)
      integer(int64), intent(in) :: x
      character(len=40):: b
      character(len=:), allocatable        :: a

      write (b, *) x
      a = trim(adjustl(b))

   end function
!==================================================================================

!==================================================================================
   pure function fstring_logical(x) result(a)
      logical, intent(in) :: x
      character(len=5)        :: a

      write (a, *) x

   end function
!==================================================================================

!==================================================================================
   pure function fstring_String(x) result(a)
      type(String_), intent(in) :: x
      character(len=:), allocatable :: a

      a = trim(x%all)
   end function
!==================================================================================

!==================================================================================
   pure function fstring_int_len(x, length) result(a)
      integer(int32), intent(in) :: x
      integer(int32), intent(in) :: length
      character(len=length)        :: a

      if (x /= x .or. abs(x) >= HUGE(int32)) then
         a = ""
         return
      end if

      write (a, *) x
      a = adjustl(a)
   end function
!==================================================================================

!==================================================================================
   pure function fstring_real(x) result(a)
      real(real64), intent(in) :: x
      character(len=31):: b
      character(len=:), allocatable        :: a

      if (x /= x .or. abs(x) >= HUGE(real64)) then
         a = ""
         return
      end if

      write (b, '(f0.7)') x
      a = trim(adjustl(b))

   end function
!==================================================================================

!==================================================================================
   pure function fstring_real32(x) result(a)
      real(real32), intent(in) :: x
      character(len=20):: b
      character(len=:), allocatable        :: a

      if (x /= x .or. abs(x) >= HUGE(real64)) then
         a = ""
         return
      end if

      write (b, '(f0.8)') x
      a = trim(adjustl(b))

   end function
!==================================================================================

!==================================================================================
   pure function fstring_complex(x) result(a)
      complex(kind(0d0)), intent(in) :: x
      character(len=30):: b
      character(len=:), allocatable        :: a

      if (x /= x .or. abs(x) >= HUGE(real64)) then
         a = ""
         return
      end if

      write (b, fmt='(F0.0,SP,F0.0,"i")') x
      a = trim(adjustl(b))
   end function
!==================================================================================

!==================================================================================
   pure function fstring_real_len(x, length) result(a)
      real(real64), intent(in) :: x
      integer(int32), intent(in) :: length
      character(len=60)        :: a
      character*40                                                :: form

      if (x /= x .or. abs(x) >= HUGE(real64)) then
         a = ""
         return
      end if

      write (a, '(f0.10)') x
      a = adjustl(a)
   end function
!==================================================================================

!==================================================================================
   function fint(ch) result(a)
      character(*), intent(in)                        :: ch
      integer(int32)                                :: a

      read (ch, *, err=1000) a
      return
1000  a = 0

   end function
!==================================================================================

!==================================================================================
   function fint16(ch) result(a)
      character(*), intent(in)                        :: ch
      integer(int16)                                :: a

      read (ch, *, err=1001) a
      return
1001  a = 0

   end function
!==================================================================================

!==================================================================================
   function fint32(ch) result(a)
      character(*), intent(in)                        :: ch
      integer(int32)                                :: a

      read (ch, *, err=1002) a
      return
1002  a = 0

   end function
!==================================================================================

!==================================================================================
   function fint64(ch) result(a)
      character(*), intent(in)                        :: ch
      integer(int64)                                :: a

      read (ch, *, err=1003) a
      return
1003  a = 0

   end function
!==================================================================================

!==================================================================================
   function freal(ch) result(a)
      character(*), intent(in)                        :: ch
      real(real64)                                :: a

      read (ch, *, err=1004) a
      return
1004  a = 0.0d0

   end function
!==================================================================================

!==================================================================================
   function freal32(ch) result(a)
      character(*), intent(in)                        :: ch
      real(real32)                                :: a

      read (ch, *, err=1005) a
      return
1005  a = 0

   end function
!==================================================================================

!==================================================================================
   function freal64(ch) result(a)
      character(*), intent(in)                        :: ch
      real(real64)                                :: a

      read (ch, *, err=1006) a
      return
1006  a = 0

   end function
!==================================================================================

!==================================================================================
   function freal128(ch) result(a)
      character(*), intent(in)                        :: ch
      real(real64)                                :: a

      read (ch, *, err=1007) a
      return
1007  a = 0

   end function
!==================================================================================

!==================================================================================
   function input_Int(default, option) result(val)
      integer(int32), intent(in) :: default
      integer(int32), optional, intent(in)::option
      integer(int32) :: val

      if (present(option)) then
         val = option
      else
         val = default
      end if

   end function
!==================================================================================

!==================================================================================
   function input_Real(default, option) result(val)
      real(real64), intent(in) :: default
      real(real64), optional, intent(in)::option
      real(real64) :: val

      if (present(option)) then
         val = option
      else
         val = default
      end if

   end function
!==================================================================================

!==================================================================================
   function input_Real32(default, option) result(val)
      real(real32), intent(in) :: default
      real(real32), optional, intent(in)::option
      real(real32) :: val

      if (present(option)) then
         val = option
      else
         val = default
      end if

   end function
!==================================================================================

!==================================================================================
   function input_Complex(default, option) result(val)
      complex(real64), intent(in) :: default
      complex(real64), optional, intent(in)::option
      complex(real64) :: val

      if (present(option)) then
         val = option
      else
         val = default
      end if

   end function
!==================================================================================

!==================================================================================
   function input_IntVec(default, option) result(val)
      integer(int32), intent(in) :: default(:)
      integer(int32), optional, intent(in)::option(:)
      integer(int32), allocatable :: val(:)
      integer(int32) :: n, m

      if (present(option)) then
         n = size(option, 1)
         allocate (val(n))
         val(:) = option(:)
      else
         n = size(default, 1)
         allocate (val(n))
         val(:) = default(:)
      end if

   end function
!==================================================================================

!==================================================================================
   function input_Realvec(default, option) result(val)
      real(real64), intent(in) :: default(:)
      real(real64), optional, intent(in)::option(:)
      real(real64), allocatable :: val(:)
      integer(int32) :: n, m

      if (present(option)) then
         n = size(option, 1)
         allocate (val(n))
         val(:) = option(:)
      else
         n = size(default, 1)
         allocate (val(n))
         val(:) = default(:)
      end if

   end function
!==================================================================================

!==================================================================================
   function input_IntArray(default, option) result(val)
      integer(int32), intent(in) :: default(:, :)
      integer(int32), optional, intent(in)::option(:, :)
      integer(int32), allocatable :: val(:, :)
      integer(int32) :: n, m

      if (present(option)) then
         n = size(option, 1)
         m = size(option, 2)
         allocate (val(n, m))
         val(:, :) = option(:, :)
      else
         n = size(default, 1)
         m = size(default, 2)
         allocate (val(n, m))
         val(:, :) = default(:, :)
      end if

   end function
!==================================================================================

!==================================================================================
   function input_RealArray(default, option) result(val)
      real(real64), intent(in) :: default(:, :)
      real(real64), optional, intent(in)::option(:, :)
      real(real64), allocatable :: val(:, :)
      integer(int32) :: n, m

      if (present(option)) then
         n = size(option, 1)
         m = size(option, 2)
         allocate (val(n, m))
         val(:, :) = option(:, :)
      else
         n = size(default, 1)
         m = size(default, 2)
         allocate (val(n, m))
         val(:, :) = default(:, :)
      end if

   end function
!==================================================================================

!==================================================================================
   function input_String(default, option) result(val)
      character(*), intent(in) :: default
      character(*), optional, intent(in)::option
      character(200) :: val

      if (present(option)) then
         val = option
      else
         val = default
      end if

   end function
!==================================================================================

!==================================================================================
   function input_logical(default, option) result(val)
      logical, intent(in) :: default
      logical, optional, intent(in)::option
      logical :: val

      if (present(option)) then
         val = option
      else
         val = default
      end if

   end function
!==================================================================================

   function zeroif_Int(val, negative, positive) result(retval)
      integer(int32), intent(in)::val
      integer(int32) :: retval
      logical, optional, intent(in) :: negative, positive

      if (val /= val) then
         print *, "ERROR :: MAthClass >> zeroif_Int is invalid"
      end if
      retval = val
      if (present(negative)) then
         if (negative .eqv. .true.) then
            if (val < 0) then
               retval = 0
            end if
         end if
      end if

      if (present(positive)) then
         if (positive .eqv. .true.) then
            if (val > 0) then
               retval = 0
            end if
         end if
      end if

   end function

   function zeroif_Real(val, negative, positive) result(retval)
      real(real64), intent(in)::val
      real(real64) :: retval
      logical, optional, intent(in) :: negative, positive

      if (val /= val) then
         print *, "ERROR :: MAthClass >> zeroif_Int is invalid"
      end if
      retval = val
      if (present(negative)) then
         if (negative .eqv. .true.) then
            if (val < 0.0d0) then
               retval = 0.0d0
            end if
         end if
      end if

      if (present(positive)) then
         if (positive .eqv. .true.) then
            if (val > 0.0d0) then
               retval = 0.0d0
            end if
         end if
      end if

   end function

! ########################################################
   subroutine removeWord_String(str, keyword, itr, Compare)
      character(*), intent(inout)::str
      character(*), intent(in)::keyword

      integer(int32) :: len_total, len_kw, i, j, n, itr_max
      integer(int32), optional, intent(in)::itr
      logical, optional, intent(in)::Compare
      logical :: bk

      if (present(Compare)) then
         if (Compare .eqv. .true.) then
            print *, "Before :: ", str
         end if
      end if

      itr_max = input(default=1, option=itr)
      bk = .false.
      len_total = len(str)
      len_kw = len(keyword)

      do i = 1, itr_max
         n = index(str, keyword)
         do j = n, n + len_kw
            str(j:j) = " "
         end do
         if (n == 0) then
            exit
         end if
      end do

      if (present(Compare)) then
         if (Compare .eqv. .true.) then
            print *, "After :: ", str
         end if
      end if

   end subroutine
! ########################################################

! ########################################################
   function Invariant_I1(sigma) result(I1)
      real(real64), intent(in) :: sigma(:, :)
      real(real64) :: I1
      integer(int32) :: i, j

      I1 = 0.0d0
      do i = 1, size(sigma, 1)
         I1 = I1 + sigma(i, i)
      end do

   end function
! ########################################################

! ########################################################
   function Invariant_J2(sigma) result(J2)
      real(real64), intent(in) :: sigma(:, :)
      real(real64) :: I1, J2, delta(3, 3), M_d(3, 3)
      integer(int32) :: i, j

      delta(:, :) = 0.0d0
      delta(1, 1) = 1.0d0
      delta(2, 2) = 1.0d0
      delta(3, 3) = 1.0d0

      I1 = Invariant_I1(sigma)
      M_d(:, :) = sigma(:, :) - I1/3.0d0*delta(:, :)
      J2 = 0.0d0
      do i = 1, size(sigma, 1)
         do j = 1, size(sigma, 1)
            J2 = J2 + 0.50d0*M_d(i, j)*M_d(i, j)
         end do
      end do

   end function
! ########################################################

! ########################################################
   function Invariant_J3(sigma) result(J3)
      real(real64), intent(in) :: sigma(:, :)
      real(real64) :: I1, J3, delta(3, 3), M_d(3, 3)
      integer(int32) :: i, j, k

      delta(:, :) = 0.0d0
      delta(1, 1) = 1.0d0
      delta(2, 2) = 1.0d0
      delta(3, 3) = 1.0d0

      I1 = Invariant_I1(sigma)
      M_d(:, :) = sigma(:, :) - I1/3.0d0*delta(:, :)
      J3 = 0.0d0

      do i = 1, size(sigma, 1)
         do j = 1, size(sigma, 1)
            do k = 1, size(sigma, 1)
               J3 = J3 + 1.0d0/3.0d0*M_d(i, j)*M_d(j, k)*M_d(k, i)
            end do
         end do
      end do

   end function
! ########################################################

! ########################################################
   function Invariant_theta(sigma) result(theta)
      real(real64), intent(in) :: sigma(:, :)
      real(real64) :: I1, J2, J3, delta(3, 3), M_d(3, 3), theta
      integer(int32) :: i, j, k

      delta(:, :) = 0.0d0
      delta(1, 1) = 1.0d0
      delta(2, 2) = 1.0d0
      delta(3, 3) = 1.0d0
      J2 = Invariant_J2(sigma)
      J3 = Invariant_J3(sigma)
      theta = 1.0d0/3.0d0*asin(-3.0d0*sqrt(3.0d0)*0.50d0*J3/J2/sqrt(J2))

   end function
! ########################################################

! ########################################################
   function inv_mod(a_in, m_in, ItrMax) result(x)
      integer(int32), intent(in) :: a_in, m_in
      integer(int32), optional, intent(in) :: ItrMax
      integer(int32) :: d, q, t, Kmat_n(2, 2), Kmat_npp(2, 2), k, itr_tol, r0, r1, r2, i, x, y, m0
      integer(int32) :: a, m

      a = a_in
      m = m_in

      itr_tol = input(default=10000, option=ItrMax)
      ! inverse modula by Extended Euclidean algorithm
      ! d = e^-1 (mod (lambda))
      ! d*e = 1 (mod (lambda))
      ! one integer q
      ! d*e - q*lambda = 1, e, lambda are known, d, q are unknown.
      ! get d, q by extended Euclidean algorithm
      ! gcd(e, lambda) = 1
      !Kmat_npp(1,1)=1
      !Kmat_npp(1,2)=0
      !Kmat_npp(2,1)=0
      !Kmat_npp(2,2)=1
      !r0=e
      !r1=lambda
      !do i=1, itr_tol
      !        r2=mod(r0,r1)
      !        if(r2==0)then
      !                print *, "gcd of ",e," and",lambda,"is", r1
      !                exit
      !        endif
      !        k=(r0-r2)/r1
      !        Kmat_n(1,1)=0
      !        Kmat_n(1,2)=1
      !        Kmat_n(2,1)=1
      !        Kmat_n(2,2)=-k
      !        a=matmul(Kmat_npp,Kmat_n)
      !        Kmat_npp=a
      !        print *, r0,"=",k,"*",r1,"+",r2
      !        r0=r1
      !        r1=r2
      !enddo
      !d = Kmat_npp(1,2)
      !print *, "Kmat_npp=",Kmat_npp
      ! cited by https://www.geeksforgeeks.org/multiplicative-inverse-under-modulo-m/
      m0 = m
      y = 0
      x = 1
      if (gcd(a, m) /= 1) then
         a = mod(a, m)
         do x = 1, m
            if (mod(a*x, m) == 1) then
               return
            end if
         end do
      end if
      if (m == 1) then
         return
      end if

      do i = 1, itr_tol
         if (a > 1) then
            q = (a - mod(a, m))/m

            t = m

            m = mod(a, m)
            a = t
            t = y

            y = x - q*y
            x = t
         else
            exit
         end if
      end do
      if (x < 0) then
         x = x + m0
      end if

   end function
! ########################################################

! ########################################################
   function gcd(a, b, ItrMax) result(c)
      integer(int32), intent(in) :: a, b
      integer(int32), optional, intent(in) :: ItrMax
      integer(int32) :: i, r0, r1, r2, k, itr_tol, c
      c = 1
      itr_tol = input(default=10000, option=ItrMax)
      r0 = a
      r1 = b
      do i = 1, itr_tol
         r2 = mod(r0, r1)
         if (r2 == 0) then
            !print *, "gcd of ",a," and",b,"is", r1
            exit
         end if
         k = (r0 - r2)/r1
         !print *, r0,"=",k,"*",r1,"+",r2
         r0 = r1
         r1 = r2
      end do
      c = r1

   end function
! ########################################################

! ########################################################
   function lcm(a, b, ItrMax) result(c)
      integer(int32), intent(in) :: a, b
      integer(int32), optional, intent(in) :: ItrMax
      integer(int32) :: i, r0, r1, r2, k, itr_tol, c

      itr_tol = input(default=10000, option=ItrMax)
      r0 = a
      r1 = b
      do i = 1, itr_tol
         r2 = mod(r0, r1)
         if (r2 == 0) then
            !print *, "gcd of ",a," and",b,"is", r1
            exit
         end if
         k = (r0 - r2)/r1
         !print *, r0,"=",k,"*",r1,"+",r2
         r0 = r1
         r1 = r2
      end do
      c = a*b/r1

   end function
! ########################################################

! ########################################################
   function convertStringToInteger(message) result(ret)
      character(*), intent(in):: message
      character(1) :: x
      character(2*len(message)) :: ret
      integer(int32) :: i
      ret = ""
      !allocate(ret(len(message)*2 ) )
      do i = 1, len(message)
         x = message(i:i)
         select case (x)
         case (" ")
            cycle
         case ("a", "A")
            ret(2*i - 1:2*i) = "01"
         case ("b", "B")
            ret(2*i - 1:2*i) = "02"
         case ("c", "C")
            ret(2*i - 1:2*i) = "03"
         case ("d", "D")
            ret(2*i - 1:2*i) = "04"
         case ("e", "E")
            ret(2*i - 1:2*i) = "05"
         case ("f", "F")
            ret(2*i - 1:2*i) = "06"
         case ("g", "G")
            ret(2*i - 1:2*i) = "07"
         case ("h", "H")
            ret(2*i - 1:2*i) = "08"
         case ("i", "I")
            ret(2*i - 1:2*i) = "09"
         case ("j", "J")
            ret(2*i - 1:2*i) = "10"
         case ("k", "K")
            ret(2*i - 1:2*i) = "11"
         case ("l", "L")
            ret(2*i - 1:2*i) = "12"
         case ("m", "M")
            ret(2*i - 1:2*i) = "13"
         case ("n", "N")
            ret(2*i - 1:2*i) = "14"
         case ("o", "O")
            ret(2*i - 1:2*i) = "15"
         case ("p", "P")
            ret(2*i - 1:2*i) = "16"
         case ("q", "Q")
            ret(2*i - 1:2*i) = "17"
         case ("r", "R")
            ret(2*i - 1:2*i) = "18"
         case ("s", "S")
            ret(2*i - 1:2*i) = "19"
         case ("t", "T")
            ret(2*i - 1:2*i) = "20"
         case ("u", "U")
            ret(2*i - 1:2*i) = "21"
         case ("v", "V")
            ret(2*i - 1:2*i) = "22"
         case ("w", "W")
            ret(2*i - 1:2*i) = "23"
         case ("x", "X")
            ret(2*i - 1:2*i) = "24"
         case ("y", "Y")
            ret(2*i - 1:2*i) = "25"
         case ("z", "Z")
            ret(2*i - 1:2*i) = "26"
         end select
      end do

   end function
! ########################################################

! ########################################################
   function convertIntegerToString(message) result(ret)
      character(*), intent(in):: message
      character(2) :: x
      character(len(message)) :: ret
      integer(int32) :: i
      ret = ""
      !allocate(ret(len(message)*2 ) )
      do i = 1, len(message)
         x(1:2) = message(2*i - 1:2*i)
         select case (x)
         case ("99")
            cycle
         case (" ")
            cycle
         case ("01")
            ret(i:i) = "a"
         case ("02")
            ret(i:i) = "b"
         case ("03")
            ret(i:i) = "c"
         case ("04")
            ret(i:i) = "d"
         case ("05")
            ret(i:i) = "e"
         case ("06")
            ret(i:i) = "f"
         case ("07")
            ret(i:i) = "g"
         case ("08")
            ret(i:i) = "h"
         case ("09")
            ret(i:i) = "i"
         case ("10")
            ret(i:i) = "j"
         case ("11")
            ret(i:i) = "k"
         case ("12")
            ret(i:i) = "l"
         case ("13")
            ret(i:i) = "m"
         case ("14")
            ret(i:i) = "n"
         case ("15")
            ret(i:i) = "o"
         case ("16")
            ret(i:i) = "p"
         case ("17")
            ret(i:i) = "q"
         case ("18")
            ret(i:i) = "r"
         case ("19")
            ret(i:i) = "s"
         case ("20")
            ret(i:i) = "t"
         case ("21")
            ret(i:i) = "u"
         case ("22")
            ret(i:i) = "v"
         case ("23")
            ret(i:i) = "w"
         case ("24")
            ret(i:i) = "x"
         case ("25")
            ret(i:i) = "y"
         case ("26")
            ret(i:i) = "z"
         end select
      end do

   end function
! ########################################################
! ########################################################
   subroutine rsa_keygen(prime1, prime2, seed, id_rsa, id_rsa_pub)
      integer(int32), intent(in) :: prime1, prime2, seed
      integer(int32), intent(out) :: id_rsa(2), id_rsa_pub(2)
      integer(int32) :: n, e, lambda, d, p, q

      p = prime1
      q = prime2

      n = p*q
      lambda = (p - 1)*(q - 1)/gcd(p - 1, q - 1)
      !print *, "lambda=",lambda

      id_rsa_pub(1) = n
      id_rsa_pub(2) = seed

      id_rsa(1) = n
      id_rsa(2) = inv_mod(seed, lambda) !get d

      print *, "#######################################################"
      print *, "Encrypted by RSA algorithm, public keys "
      print *, "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
      print *, "Multiplication of two prime numbers is ", id_rsa_pub(1)
      print *, "Seed value (1 < seed < ", id_rsa_pub(1), ") is", id_rsa_pub(2)
      print *, "Notice:: message should be (1 < seed < ", id_rsa_pub(1), ")."
      print *, "#######################################################"

   end subroutine
! ########################################################

! ########################################################
   function rsa_encrypt(id_rsa_pub, message) result(ciphertext)
      integer(int32), intent(in) ::id_rsa_pub(2), message
      integer(int32) :: ciphertext, i

      ciphertext = 1
      do i = 1, id_rsa_pub(2)
         ciphertext = mod(ciphertext*message, id_rsa_pub(1))
      end do

   end function
! ########################################################

! ########################################################
   function rsa_decrypt(id_rsa, ciphertext) result(message)
      integer(int32), intent(in) ::id_rsa(2), ciphertext
      integer(int32) :: d, n, e, message, i

      message = 1
      do i = 1, id_rsa(2)
         message = mod(message*ciphertext, id_rsa(1))
      end do

   end function
! ########################################################

   function IsItNumber(char) result(res)
      character(*), intent(inout) :: char
      logical :: res
      integer :: i
      character(1) :: firstchar

      res = .false.
      ! search all
      firstchar = trim(adjustl(char(1:1)))

      if (firstchar == "1") then
         res = .true.
         return
      elseif (firstchar == "2") then
         res = .true.
         return
      elseif (firstchar == "3") then
         res = .true.
         return
      elseif (firstchar == "4") then
         res = .true.
         return
      elseif (firstchar == "5") then
         res = .true.
         return
      elseif (firstchar == "6") then
         res = .true.
         return
      elseif (firstchar == "7") then
         res = .true.
         return
      elseif (firstchar == "8") then
         res = .true.
         return
      elseif (firstchar == "9") then
         res = .true.
         return
      elseif (firstchar == "0") then
         res = .true.
         return
      elseif (firstchar == ".") then
         res = .true.
         return
      else
         return
      end if

   end function IsItNumber

! BitInversion
!recursive function BitInversion(i,numBit) result(ret)
!        integer(int32),intent(in) :: i
!        integer(int32),intent(in) :: numBit
!
!        if(numBit==1)then
!                ! 1 Bit 0 or 1
!
!        elseif(numBit==2)then
!        elseif(numBit==3)then
!        if(numBit > 3) then
!        endif
!
!end function

! Window functions

   function RectangularWindow(original_data, Width) result(ret)
      real(real64), intent(in) :: original_data(:)
      integer(int32), intent(in) :: Width
      real(real64), allocatable :: ret(:)
      integer(int32) :: i, j, n

      allocate (ret(size(original_data)))

      do i = 1, size(original_data)
         n = 0
         do j = -Width/2, Width/2
            if (i + j > size(original_data) .or. i + j < 1) cycle
            n = n + 1
            ret(i) = original_data(i) + original_data(i + j)
         end do
         ret(i) = ret(i)/dble(n)
      end do

   end function

   function DigitalWindow(original_data) result(ret)
      real(real64), intent(in) :: original_data(:)

      real(real64), allocatable :: ret(:)
      integer(int32) :: i, j, n

      allocate (ret(size(original_data)))

      do i = 2, size(original_data) - 1
         ret(i) = 0.250d0*original_data(i - 1) + 0.50d0*original_data(i) + &
                  0.250d0*original_data(i + 1)
      end do

   end function

   function HanningWindow(Width, DataSize) result(ret)
      integer(int32), intent(in) :: Width, DataSize
      real(real64) :: ret(DataSize)
      type(Math_) :: math
      integer(int32) :: i

      print *, "[CAUTION] EXPERIMENTAL!"

      ret = 0.0d0
      do i = 1, width/2
         ret(DataSize/2 - i) &
            = 0.50d0 - 0.50d0*cos(2.0d0*Math%PI*i/(Width/2))
         ret(DataSize/2 + i) &
            = 0.50d0 - 0.50d0*cos(2.0d0*Math%PI*i/(Width/2))
      end do

   end function

   function HammingWindow(Width, DataSize) result(ret)
      integer(int32), intent(in) :: Width, DataSize
      real(real64) :: ret(DataSize)
      type(Math_) :: math
      integer(int32) :: i

      print *, "[CAUTION] EXPERIMENTAL!"

      ret = 0.0d0
      do i = 1, width/2
         ret(DataSize/2 - i) &
            = 0.540d0 - 0.46d0*cos(2.0d0*Math%PI*i/(Width/2))
         ret(DataSize/2 + i) &
            = 0.540d0 - 0.46d0*cos(2.0d0*Math%PI*i/(Width/2))
      end do

   end function
! #######################################################################
   function log2(x) result(ret)
      real(real64), intent(in) :: x
      real(real64) :: ret

      ret = log(x)/log(2.0d0)

   end function
! #######################################################################

! #######################################################################
   pure function day(unit) result(ret)
      character(*), intent(in):: unit
      real(real64) :: ret

      if (unit(1:1) == "S" .or. unit(1:1) == "s") then
         ! day to second
         ret = 24.0d0*60.0d0*60.0d0
         return
      end if

      if (unit(1:1) == "M" .or. unit(1:1) == "m") then
         ! day to minutes
         ret = 24.0d0*60.0d0
         return
      end if

      if (unit(1:1) == "H" .or. unit(1:1) == "h") then
         ! hour to minutes
         ret = 24.0d0
         return
      end if

      if (unit(1:1) == "D" .or. unit(1:1) == "d") then
         ! day to minutes
         ret = 1.0d0
         return
      end if

      if (unit(1:1) == "Y" .or. unit(1:1) == "y") then
         ! day to year
         ret = 1.0d0/365.0d0
         return
      end if

   end function
! #######################################################################

! #######################################################################
   pure recursive function factorialInt32(n) result(ret)
      integer(int32), intent(in) :: n
      integer(int64) :: i, ret

      ret = 1
      do i = 1, n
         ret = ret*i
      end do

   end function
! #######################################################################

! #######################################################################
   pure recursive function factorialReal64(n) result(ret)
      real(real64), intent(in) :: n
      real(real64) :: ret
      integeR(int32) :: i

      ret = 1.0d0
      do i = 1, int(n)
         ret = ret*dble(i)
      end do

   end function
! #######################################################################

   pure function comb(n, r) result(ret)
      integer(int32), intent(in) :: n, r
      real(real64) :: ret
      integer(int32) :: i
      real(real64), allocatable :: buf1(:), buf2(:), buf3(:)

      if (n - r < 0) then
         ret = 0.0d0
         return
      end if

      if (n <= 10) then
         ret = factorial(n)/(factorial(r)*factorial(n - r))
      else
         allocate (buf1(n), buf2(n), buf3(n))
         do concurrent(i=1:n)
            buf1(i) = i
         end do
         do concurrent(i=1:r)
            buf2(i) = i
         end do
         do concurrent(i=1:n - r)
            buf3(i) = i
         end do

         do concurrent(i=1:r)
            buf1(i) = buf1(i)/buf2(i)
         end do
         do concurrent(i=1:n - r)
            buf1(i) = buf1(i)/buf3(i)
         end do

         ret = 1.0d0
         do i = 1, n
            ret = ret*buf1(i)
         end do
         ret = dble(nint(ret))

         !by array
      end if

   end function

   function stringFromChar(charval) result(ret)
      character(*), intent(in):: charval
      type(String_) :: ret

      ret = charval

   end function
! #######################################################################

   pure function zfill(intval, n) result(ret)
      integer(int32), intent(in) :: intval, n
      character(n) :: ret
      !character(:),allocatable :: fmt
      character(len=20) :: zfill_fmt
      character(len=20):: b
      write (b, *) n

      zfill_fmt = '(I'//trim(adjustl(b))//'.'//trim(adjustl(b))//')'
      write (ret(1:n), trim(adjustl(zfill_fmt))) intval

   end function

! ########################################################################
   pure function imaginary_partComplex64(complexValue) result(imgpart)
      complex(real64), intent(in) :: complexValue
      real(real64) :: imgpart
      type(Math_) :: math

      imgpart = real(complexvalue*math%i)

   end function
! ########################################################################

! ########################################################################
   pure function imaginary_partComplex32(complexValue) result(imgpart)
      complex(real32), intent(in) :: complexValue
      real(real32) :: imgpart
      type(Math_) :: math

      imgpart = -real(complexvalue*math%i)

   end function
! ########################################################################

   function hilbert(wave) result(h_top_wave)
      complex(real64), intent(in) :: wave(:)
      complex(real64), allocatable :: h_top_wave(:), spectre(:)

      spectre = fft(wave)
      spectre(1:size(spectre)/2) = 2.0d0*spectre(1:size(spectre)/2)
      spectre(size(spectre)/2 + 1:) = 0.0d0
      h_top_wave = ifft(spectre)

   end function
! ########################################################################

   function short_time_FFT(wave, frame) result(spectre)
      complex(real64), intent(in) :: wave(:)
      complex(real64), allocatable :: spectre(:, :)
      integer(int32), intent(in) :: frame
      integer(int32) :: i, from, to

      ! short-time FFT for n=frame length
      allocate (spectre(size(wave), 2*frame))
      !$OMP parallel do private(from,to)
      do i = 1, size(wave)
         from = i - frame
         to = i + frame - 1
         if (from < frame .or. to > size(wave) - frame) then
            cycle
         end if
         spectre(i, :) = fft(wave(i - frame:i + frame - 1))
      end do
      !$OMP end parallel do

   end function
! ########################################################################

! ########################################################################
   pure function RickerFunctionReal64(t, sigma, center) result(ft)
      real(real64), intent(in) :: t, sigma
      real(real64), optional, intent(in) :: center
      type(Math_)  :: math
      real(real64) ::ft128
      real(real64) :: ft, b

      if (present(center)) then
         b = center
      else
         b = 0.0d0
      end if

      ft128 = 2.0d0/(sqrt(3.0d0*sigma)*math%pi**(0.25))* &
              (1.0d0 - ((t - b)/sigma)*((t - b)/sigma))*exp(-(t - b)*(t - b)/2.0d0/sigma/sigma)

      ft = dble(ft128)

   end function
! ########################################################################

! ########################################################################
   pure function RickerFunctionReal64Vector(t, sigma, center) result(ft)
      real(real64), intent(in) :: t(:), sigma
      real(real64), optional, intent(in) :: center
      type(Math_)  :: math
      real(real64) ::ft128
      integer(int32) :: i
      real(real64), allocatable :: ft(:)

      allocate (ft(size(t)))
      do i = 1, size(t)
         ft(i) = RickerFunction(t=t(i), sigma=sigma, center=center)
      end do

   end function
! ########################################################################

! ########################################################
   real(real64) function derivative_scalar(func, x, eps)
      ! >>> Define func()
      interface
         real(real64) function func(x)
            use iso_fortran_env
            real(real64), intent(in) :: x
         end function
      end interface

      ! <<<

      ! >>> arg
      real(real64), intent(in) :: x
      real(real64), optional, intent(in) :: eps
      ! <<<

      real(real64)  :: eps_val = dble(1.0e-4)
      if (present(eps)) then
         eps_val = eps
      end if

      ! >>> operation
      ! numerical derivative

      derivative_scalar = (func(x + eps_val) - func(x - eps_val))/(2.0d0*eps_val)
      ! <<<

   end function

! ########################################################
   function derivative_vector(func, x, dim_num, eps) result(ret)
      integer(int32), intent(in) :: dim_num
      ! >>> Define func()
      interface
         function func(x) result(ret)
            use iso_fortran_env
            real(real64), intent(in) :: x(:)
            real(real64), allocatable :: ret(:)
         end function
      end interface
      ! <<<

      ! >>> arg
      real(real64), intent(in) :: x(1:dim_num)
      real(real64), optional, intent(in) :: eps
      ! <<<

      ! >>> output
      real(real64), allocatable :: ret(:)
      ! <<<

      real(real64) :: x_f(1:dim_num)
      real(real64) :: x_b(1:dim_num)
      real(real64)  :: eps_val = dble(1.0e-4)

      if (present(eps)) then
         eps_val = eps
      end if

      ret = x
      x_f = x
      x_f(:) = x_f(:) + eps_val
      x_b = x
      x_b(:) = x_b(:) - eps_val

      ! >>> operation
      ! numerical derivative

      ret = (func(x_f) - func(x_b))/(2.0d0*eps_val)
      ! <<<

   end function
! ########################################################

   real(real64) function polynomial(x, params)
      real(real64), intent(in) :: x
      real(real64), intent(in) :: params(:)
      integer(int32) :: i, n, order_

      n = size(params)
      ! (n-1)-order polynomial
      polynomial = 0.0d0
      order_ = 0
      do i = n - 1, 0, -1
         order_ = order_ + 1
         polynomial = polynomial + params(order_)*(x**i)
      end do

   end function

! ###########################################################
   real(real64) function sigmoid(x, params)
      real(real64), intent(in) :: x, params(:)

      if (size(params) == 0) then
         sigmoid = 1.0d0/(1.0d0 + exp(-(x)))
      elseif (size(params) == 1) then
         sigmoid = 1.0d0/(1.0d0 + exp(-params(1)*(x)))
      elseif (size(params) == 2) then
         sigmoid = 1.0d0/(1.0d0 + exp(-params(1)*(x - params(2))))
      else
         sigmoid = 1.0d0/(1.0d0 + exp(-params(1)*(x - params(2))))*params(3)
      end if

   end function
! ###########################################################

! ###########################################################
   real(real64) function logit(x, params)
      real(real64), intent(in) :: x, params(:)

      logit = log(x/(1 - x))

   end function
! ###########################################################

   function int_from_logical(logical_value) result(ret)
      logical, intent(in) :: logical_value
      integer(int32) :: ret
      if (logical_value) then
         ret = 0
      else
         ret = 1
      end if
   end function

   function int_from_logical_vector(logical_value) result(ret)
      logical, intent(in) :: logical_value(:)
      integer(int32), allocatable :: ret(:)
      integer(int32) :: i

      allocate (ret(size(logical_value)))
      do i = 1, size(ret)
         if (logical_value(i)) then
            ret(i) = 0
         else
            ret(i) = 1
         end if
      end do
   end function
! ###########################################################

!function arg_complex64(z) result(theta)
!        complex(real64),intent(in) :: z
!        real(real64) ::  theta, x, y
!        type(Math_) :: math
!
!        x = dble(z)
!        y = imag(z)
!
!        if(x>0.0d0)then
!                theta = atan(y/x)
!        elseif(x<0.0d0 .and. y>=0.0d0)then
!                theta = atan(y/x) + math%pi
!        elseif(x<0.0d0 .and. y<0.0d0)then
!                theta = atan(y/x) - math%pi
!        elseif(x==0.0d0 .and. y>0.0d0)then
!                theta = math%pi/2.0d0
!        elseif(x==0.0d0 .and. y<0.0d0)then
!                theta = - math%pi/2.0d0
!        else
!                theta = 0.0d0
!        endif
!end function

! ###########################################################
   function matrix_exponential_real64(mat, order) result(ret)
      real(real64), intent(in) :: mat(:, :)
      integer(int32), intent(in) :: order
      real(real64), allocatable :: ret(:, :), d_ret(:, :)
      integer(int32) :: i, k

      ret = 0.0d0*mat
      d_ret = 0.0d0*mat
      do i = 1, size(ret, 1)
         d_ret(i, i) = 1.0d0
      end do

      ! k = 0
      ret = d_ret

      k = 0
      do
         k = k + 1
         d_ret = 1.0d0/dble(k)*matmul(d_ret, mat)
         ret = ret + d_ret
         if (k + 1 > order) exit
      end do

   end function
! ###########################################################

   function Bessel_J0_complex(z) result(ret)
      complex(real64), intent(in) :: z
      complex(real64) :: ret, dret
      integer(int32) :: k

      !ret = 1.0d0-z*z/4+(z**4)/64.0d0 ! + O(x^6)
      ret = 0.0d0
      dret = 1.0d0
      do k = 1, 30
         ret = ret + dret
         dret = dret/dble(k)/dble(k)*(-1.0d0/4.0d0)*z*z
         if (abs(dret) < 1.0e-16) exit
      end do

   end function

! ###########################################################

! ###########################################################

   function Bessel_J1_complex(z) result(ret)
      complex(real64), intent(in) :: z
      complex(real64) :: ret, dret
      integer(int32) :: k

      !ret = z/2.0d0+(z**3)/16.0d0+(z**5)/384.0d0 ! + O(x^6)
      ret = 0.0d0
      dret = 1.0d0/2.0d0*z
      do k = 1, 30
         ret = ret + dret
         dret = dret/dble(k)/dble(k + 1)*(-1.0d0)*(1.0d0/4.0d0)*z*z
         if (abs(dret) < 1.0e-16) exit
      end do
   end function

! ###########################################################



! ###########################################################
   recursive subroutine heapsort_int32_array(array,order,exec_row_sort)
      integer(int32),intent(inout) :: array(:,:)
      integer(int32),allocatable,optional,intent(inout) :: order(:)
      logical,optional,intent(in) :: exec_row_sort
      integer(int32),allocatable :: buf(:),colbuf(:),arraybuf(:,:),new_order(:),orderbuf(:)
      integer(int32),allocatable :: from_to(:,:)
      integer(int32) :: i, j, from, to, k, n_from_to
      logical :: row_sort
      
      !if(present(order) )then
      !   if(.not.allocated(order))then
      !      order = [(i,i=1,size(array,1))]
      !   endif
      !endif
      row_sort = .true.
      if(present(exec_row_sort) )then
         row_sort = exec_row_sort
      endif

      ! 各行をソート
      allocate(buf(size(array,2)) )
      if(size(array,2)>2 .and. row_sort)then
         !$OMP parallel do private(buf) shared(array)
         !do concurrent(i=1:size(array,1))
         do i=1,size(array,1)
            buf(:) = array(i,:)
            ! 行内入れ替えではorderは変えず
            call heapsortInt32(n=size(buf),array=buf)
            array(i,:) = buf(:)
         end do
         !$OMP end parallel do
      end if

      ! 1回1列目でソート
      colbuf = array(:,1)
      new_order = [(i,i=1,size(colbuf))]
      call heapsortInt32Int32(n=size(colbuf),array=colbuf,val=new_order)
      
      ! new_orderによってorderを並び替え
      

      arraybuf = array
      array(:,:) = arraybuf(new_order(:),:)
      if(present(order) )then
         order(:) = order(new_order(:))
      endif
      ! debug
      !return

      if(size(array,2)==1 )then
         return
      endif
      
      ! もしcolumが2以上あれば，再帰
      n_from_to = 0
      j=1
      do 
         if(j>=size(array,1) )exit
         from = j
         do to=j+1,size(array,1)
            if(array(from,1)==array(to,1))then
               cycle
            else
               exit
            endif
         end do
         
      
         if(to-from>1)then
            if(size(array,2)>1 )then
               
               !arraybuf = array(from:to-1,2:)
               !orderbuf = order(from:to-1)
               !
               !call heapsort_int32_array(arraybuf,orderbuf,exec_row_sort=.false.)
               !
               !array(from:to-1,2:) = arraybuf(:,:)
               !order(from:to-1) = orderbuf(:)
               n_from_to = n_from_to + 1
               j = to 
               cycle
            endif
         endif
         j = j + 1
      end do

      allocate(from_to(n_from_to,2))
      n_from_to = 0
      j=1
      do 
         if(j>=size(array,1) )exit
         from = j
         do to=j+1,size(array,1)
            if(array(from,1)==array(to,1))then
               cycle
            else
               exit
            endif
         end do
         
      
         if(to-from>1)then
            if(size(array,2)>1 )then
               
               !arraybuf = array(from:to-1,2:)
               !orderbuf = order(from:to-1)
               !
               !call heapsort_int32_array(arraybuf,orderbuf,exec_row_sort=.false.)
               !
               !array(from:to-1,2:) = arraybuf(:,:)
               !order(from:to-1) = orderbuf(:)
               n_from_to = n_from_to + 1
               from_to(n_from_to,1) = from
               from_to(n_from_to,2) = to 
               j = to 
               cycle
            endif
         endif
         j = j + 1
      end do

      !$OMP parallel do private(from,to,arraybuf,orderbuf) shared(array,order)
      do n_from_to=1,size(from_to,1)
         from = from_to(n_from_to,1)
         to   = from_to(n_from_to,2)
         arraybuf = array(from:to-1,2:)
         if(present(order) )then
            orderbuf = order(from:to-1)
         endif
         
         call heapsort_int32_array(arraybuf,orderbuf,exec_row_sort=.false.)
         
         array(from:to-1,2:) = arraybuf(:,:)
         if(present(order) )then
            order(from:to-1) = orderbuf(:)
         endif
      enddo
      !$OMP end parallel do

      ! >> non-parallelization
      !j=1
      !do 
      !   if(j>=size(array,1) )exit
      !   from = j
      !   do to=j+1,size(array,1)
      !      if(array(from,1)==array(to,1))then
      !         cycle
      !      else
      !         exit
      !      endif
      !   end do
      !   
      !
      !   if(to-from>1)then
      !      if(size(array,2)>1 )then
      !         
      !         arraybuf = array(from:to-1,2:)
      !         orderbuf = order(from:to-1)
      !         
      !         call heapsort_int32_array(arraybuf,orderbuf,exec_row_sort=.false.)
      !         
      !         array(from:to-1,2:) = arraybuf(:,:)
      !         order(from:to-1) = orderbuf(:)
      !         j = to 
      !         cycle
      !      endif
      !   endif
      !   j = j + 1
      !end do

   end subroutine
! ###########################################################



   
! ###########################################################
   recursive subroutine heapsort_real64_array(array,order,exec_row_sort)
      real(real64),intent(inout) :: array(:,:)
      integer(int32),allocatable,optional,intent(inout) :: order(:)
      logical,optional,intent(in) :: exec_row_sort
      real(real64),allocatable :: buf(:),colbuf(:),arraybuf(:,:)
      integer(int32),allocatable :: new_order(:),orderbuf(:)
      integer(int32),allocatable :: from_to(:,:)
      integer(int32) :: i, j, from, to, k, n_from_to
      logical :: row_sort
      
      !if(present(order) )then
      !   if(.not.allocated(order))then
      !      order = [(i,i=1,size(array,1))]
      !   endif
      !endif
      row_sort = .true.
      if(present(exec_row_sort) )then
         row_sort = exec_row_sort
      endif

      ! 各行をソート
      allocate(buf(size(array,2)) )
      if(size(array,2)>2 .and. row_sort)then
         !$OMP parallel do private(buf) shared(array)
         !do concurrent(i=1:size(array,1))
         do i=1,size(array,1)
            buf(:) = array(i,:)
            ! 行内入れ替えではorderは変えず
            call heapsortReal64(n=size(buf),array=buf)
            array(i,:) = buf(:)
         end do
         !$OMP end parallel do
      end if

      ! 1回1列目でソート
      colbuf = array(:,1)
      new_order = [(i,i=1,size(colbuf))]
      call heapsortReal64Int32(n=size(colbuf),array=colbuf,val=new_order)
      
      ! new_orderによってorderを並び替え
      

      arraybuf = array
      array(:,:) = arraybuf(new_order(:),:)
      if(present(order) )then
         order(:) = order(new_order(:))
      endif
      ! debug
      !return

      if(size(array,2)==1 )then
         return
      endif
      
      ! もしcolumが2以上あれば，再帰
      n_from_to = 0
      j=1
      do 
         if(j>=size(array,1) )exit
         from = j
         do to=j+1,size(array,1)
            if(array(from,1)==array(to,1))then
               cycle
            else
               exit
            endif
         end do
         
      
         if(to-from>1)then
            if(size(array,2)>1 )then
               
               !arraybuf = array(from:to-1,2:)
               !orderbuf = order(from:to-1)
               !
               !call heapsort_int32_array(arraybuf,orderbuf,exec_row_sort=.false.)
               !
               !array(from:to-1,2:) = arraybuf(:,:)
               !order(from:to-1) = orderbuf(:)
               n_from_to = n_from_to + 1
               j = to 
               cycle
            endif
         endif
         j = j + 1
      end do

      allocate(from_to(n_from_to,2))
      n_from_to = 0
      j=1
      do 
         if(j>=size(array,1) )exit
         from = j
         do to=j+1,size(array,1)
            if(array(from,1)==array(to,1))then
               cycle
            else
               exit
            endif
         end do
         
      
         if(to-from>1)then
            if(size(array,2)>1 )then
               
               !arraybuf = array(from:to-1,2:)
               !orderbuf = order(from:to-1)
               !
               !call heapsort_int32_array(arraybuf,orderbuf,exec_row_sort=.false.)
               !
               !array(from:to-1,2:) = arraybuf(:,:)
               !order(from:to-1) = orderbuf(:)
               n_from_to = n_from_to + 1
               from_to(n_from_to,1) = from
               from_to(n_from_to,2) = to 
               j = to 
               cycle
            endif
         endif
         j = j + 1
      end do

      !$OMP parallel do private(from,to,arraybuf,orderbuf) shared(array,order)
      do n_from_to=1,size(from_to,1)
         from = from_to(n_from_to,1)
         to   = from_to(n_from_to,2)
         arraybuf = array(from:to-1,2:)
         if(present(order) )then
            orderbuf = order(from:to-1)
         endif
         
         call heapsort_real64_array(arraybuf,orderbuf,exec_row_sort=.false.)
         
         array(from:to-1,2:) = arraybuf(:,:)
         if(present(order) )then
            order(from:to-1) = orderbuf(:)
         endif
      enddo
      !$OMP end parallel do

      ! >> non-parallelization
      !j=1
      !do 
      !   if(j>=size(array,1) )exit
      !   from = j
      !   do to=j+1,size(array,1)
      !      if(array(from,1)==array(to,1))then
      !         cycle
      !      else
      !         exit
      !      endif
      !   end do
      !   
      !
      !   if(to-from>1)then
      !      if(size(array,2)>1 )then
      !         
      !         arraybuf = array(from:to-1,2:)
      !         orderbuf = order(from:to-1)
      !         
      !         call heapsort_int32_array(arraybuf,orderbuf,exec_row_sort=.false.)
      !         
      !         array(from:to-1,2:) = arraybuf(:,:)
      !         order(from:to-1) = orderbuf(:)
      !         j = to 
      !         cycle
      !      endif
      !   endif
      !   j = j + 1
      !end do

   end subroutine
! ###########################################################



! ###########################################################
   subroutine sort_and_remove_duplication_int32(array,order)
      integer(int32),allocatable,intent(inout) :: array(:,:)
      integer(int32),allocatable,optional,intent(inout) :: order(:)
      integer(int32),allocatable :: buf(:),arraybuf(:,:),orderbuf(:)
      integer(int32) :: i,j,offset,count_minus_one,itr

      offset = minval(array)
      array = array - offset + 1
      call heapsort(array,order)

      do i=1,size(array,1)
         buf = array(i,:)
         do j=i+1,size(array,1)-1
            if( maxval(abs(buf(:)-array(j,:)))==0)then
               array(i,:) = -1
               array(j,:) = -1
            else
               exit
            endif
         enddo
      end do
      
      count_minus_one = 0
      do i=1,size(array,1)
         if(array(i,1)==-1 )then
            count_minus_one = count_minus_one + 1
         endif
      enddo
      
      arraybuf = array
      deallocate(array)
      allocate(array(size(arraybuf,1)-count_minus_one,size(arraybuf,2)))
      if(present(order) )then
         orderbuf = order
         order = 0*orderbuf(1:size(arraybuf,1)-count_minus_one)
      endif
      itr = 0
      do i=1,size(arraybuf,1)
         if(arraybuf(i,1)==-1 )then
            cycle
         else
            itr = itr + 1
            array(itr,:) = arraybuf(i,:)
            if(present(order) )then
               order(itr) = orderbuf(i)
            endif
         endif
      enddo
      array = array + offset - 1
      
   end subroutine
! ###########################################################

! ###########################################################
   subroutine sort_and_remove_duplication_real64(array,order)
      real(real64),allocatable,intent(inout) :: array(:,:)
      integer(int32),allocatable,optional,intent(inout) :: order(:)
      integer(int32),allocatable :: buf(:),arraybuf(:,:)
      integer(int32),allocatable :: orderbuf(:)
      integer(int32) :: i,j,count_minus_one,itr
      real(real64) :: offset

      offset = minval(array)
      array = array - offset + 1.0d0
      call heapsort(array,order)

      do i=1,size(array,1)
         buf = array(i,:)
         do j=i+1,size(array,1)-1
            if( maxval(abs(buf(:)-array(j,:)))==0.0d0)then
               array(i,:) = -1.0d0
               array(j,:) = -1.0d0
            else
               exit
            endif
         enddo
      end do
      
      count_minus_one = 0
      do i=1,size(array,1)
         if(array(i,1)==-1 )then
            count_minus_one = count_minus_one + 1
         endif
      enddo
      
      arraybuf = array
      deallocate(array)
      allocate(array(size(arraybuf,1)-count_minus_one,size(arraybuf,2)))
      if(present(order) )then
         orderbuf = order
         order = 0*orderbuf(1:size(arraybuf,1)-count_minus_one)
      endif
      itr = 0
      do i=1,size(arraybuf,1)
         if(arraybuf(i,1)==-1 )then
            cycle
         else
            itr = itr + 1
            array(itr,:) = arraybuf(i,:)
            if(present(order) )then
               order(itr) = orderbuf(i)
            endif
         endif
      enddo
      array = array + offset - 1.0d0
      
   end subroutine
! ###########################################################

! ###########################################################
subroutine assign_real64(x,y)
   character(*),intent(in) :: y
   real(real64),intent(inout) :: x

   x = freal(y)

end subroutine
! ###########################################################



! ###########################################################
subroutine assign_int32(x,y)
   character(*),intent(in) :: y
   integer(int32),intent(inout) :: x

   x = fint(y)

end subroutine
! ###########################################################


end module MathClass
