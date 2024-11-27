module RandomClass
   use, intrinsic :: iso_fortran_env
   use MathClass
   implicit none

   type::Random_
      integer(int32) :: random_int
      integer(int32), allocatable  :: random_int_seed(:)
      integer(int32), allocatable :: random_int_vec(:)
      real(real64) :: random_real
      real(real64), allocatable :: random_real_vec(:)
   contains
      procedure :: init => initRandom
      procedure :: random => getRandom
      procedure :: randint => getRandomInt
      procedure :: name => nameRandom
      procedure :: choiceInt => choiceRandomInt
      procedure :: choiceReal => choiceRandomReal
      procedure :: drawOne => drawOneRandomInt
      procedure :: draw => drawRandomInt
      procedure :: shuffle => shuffleRandomInt
      procedure :: uniform => uniformRandom
      procedure, pass :: gauss_scalar_Random
      procedure, pass :: gauss_vector_Random
      generic :: gauss => gauss_scalar_Random, gauss_vector_Random
      procedure :: ChiSquared => ChiSquaredRandom
      procedure :: Chauchy => ChauchyRandom
      procedure :: Lognormal => LognormalRandom
      procedure :: InverseGauss => InverseGaussRandom
      procedure :: save => saveRandom
      procedure :: white => whiteRandom

      procedure, pass :: randnRandomVector
      procedure, pass :: randnRandomArray
      generic :: randn => randnRandomArray, randnRandomvector

      procedure :: fill => fillRandom
      procedure :: histogram => histogramRandom
      procedure :: scalar => scalarRandom
      procedure :: vector => vectorRandom
      procedure :: matrix => matrixRandom
      procedure :: cube => cubeRandom
      !procedure :: choiceString => choiceRandomString
   end type

   interface randi
      module procedure :: randi_range_rank2, randi_imax, randi_imax_n
   end interface

   interface rand
      module procedure :: rand_n, rand_sz2, rand_sz3
   end interface

   interface EV
      module procedure :: ExpectationValue_r64_i32_i32
   end interface

contains

!##########################################
   subroutine initRandom(obj)
      class(Random_), intent(inout)::obj
      !integer(int32),optional,intent(in)::SeedSize
      integer(int32)::SeedSize

      call random_seed(size=SeedSize)
      if (.not. allocated(obj%random_int_seed)) then
         allocate (obj%random_int_seed(SeedSize))
      end if
      !call random_seed(get=obj%random_real_vec)
      call random_seed(get=obj%random_int_seed)

   end subroutine
!##########################################

!##########################################
   function getRandom(obj, distribution) result(x)
      class(Random_)::obj
      character(*), optional, intent(in)::distribution
      real(real64) :: x, val, y
      integer(int32) :: i

      if (trim(distribution) == "Binomial" .or. trim(distribution) == "binomial") then
         val = 0.0d0
         do i = 1, 20
            call random_number(y)
            val = val + y
         end do
         x = val - 10.0d0
         return
      end if

      call random_number(x)

   end function
!##########################################

!##########################################
   subroutine saveRandom(obj)
      class(Random_), intent(inout)::obj

      call random_seed(put=obj%random_int_seed)
   end subroutine
!##########################################

!##########################################
   function uniformRandom(obj, From, To) result(x)
      class(Random_), intent(in)::obj
      real(real64) :: x, a, diff, val(2)
      real(real64), intent(in) :: From, To

      val(1) = From
      val(2) = To
      diff = abs(from - to)
      call random_number(a)
      x = a*diff + minval(val)

   end function
!##########################################

!##########################################
   function getRandomInt(obj, From, To) result(x)
      class(Random_), intent(in)::obj
      real(real64) :: xr, a, diff, val(2)
      integer(int32) :: x
      integer(int32), intent(in) :: From, To

      val(1) = From
      val(2) = To
      diff = abs(dble(from) - dble(to))

      call random_number(a)
      xr = a*diff + minval(val)
      x = nint(xr)
      if (x == From - 1) then
         x = From
      end if
      if (x == To + 1) then
         x = To
      end if

   end function
!##########################################

!##########################################
   function choiceRandomInt(obj, Vector, Array) result(val)
      class(Random_), intent(in)::obj
      integer(int32), optional, intent(in) :: Vector(:)
      integer(int32), Optional, intent(in) :: Array(:, :)
      integer(int32) :: val, posi, posi2

      ! it should be over-rided
      if (present(Vector)) then
         posi = obj%randint(1, size(Vector))
         val = Vector(posi)
         return
      end if

      if (present(Array)) then
         print *, size(Array, 1)
         posi = obj%randint(1, size(Array, 1))
         posi2 = obj%randint(1, size(Array, 2))
         val = Array(posi, posi2)
         return
      end if

      print *, "No list is imported."

   end function
!##########################################

!##########################################
   function choiceRandomReal(obj, Vector, Array) result(val)
      class(Random_), intent(in)::obj
      real(real64), Optional, intent(in) :: Vector(:)
      real(real64), Optional, intent(in) :: Array(:, :)
      real(real64) :: val
      integer(int32) :: posi, posi2

      ! it should be over-rided
      if (present(Vector)) then
         posi = obj%randint(1, size(Vector))
         val = Vector(posi)
         return
      end if

      if (present(Array)) then
         print *, size(Array, 1)
         posi = obj%randint(1, size(Array, 1))
         posi2 = obj%randint(1, size(Array, 2))
         val = Array(posi, posi2)
         return
      end if

      print *, "No list is imported."

   end function
!##########################################

!##########################################
   function randnRandomArray(obj, d0, d1) result(array)
      class(Random_), intent(inout)::obj
      real(real64), allocatable :: array(:, :)
      integer(int32), intent(in) :: d0, d1
      integer(int32) :: n, m, i, j

      n = d0
      m = d1

      allocate (array(n, m))

      call obj%init()

      do i = 1, n
         do j = 1, m
            array(i, j) = obj%random()
         end do
      end do

   end function
!##########################################

!##########################################
   function randnRandomVector(obj, d0) result(array)
      class(Random_), intent(inout)::obj
      real(real64), allocatable :: array(:)
      integer(int32), intent(in) :: d0
      integer(int32) :: n, m, i, j

      n = d0

      allocate (array(n))

      call obj%init()

      do i = 1, n
         array(i) = obj%random()
      end do

   end function
!##########################################

!##########################################
!function choiceRandomString(obj,Str) result(val)
!    class(Random_),intent(in) :: obj
!    character(*),  intent(in) :: Str
!    character(1) :: val
!    integer(int32) :: posi,length
!
!    length=len(Str)
!
!    ! it should be over-rided
!    posi=obj%randint(1,length )
!    val=Str(posi)
!
!
!end function
!##########################################

!##########################################
   function histogramRandom(obj, list, division) result(histogram)
      class(Random_), intent(inout) :: obj
      real(real64), intent(in)  :: list(:) ! data-list
      real(real64), allocatable :: histogram(:, :)
      integer(int32), optional, intent(in) :: division
      integer(int32) :: i, j, n
      real(real64) :: maxv, minv, val, intval

      n = input(default=10, option=division)

      maxv = maxval(list)
      minv = minval(list)

      intval = (maxv - minv)/dble(n)

      allocate (histogram(n, 2))
      histogram(:, :) = 0

      val = minv - 0.00000010d0
      do i = 1, size(histogram, 1)
         histogram(i, 1) = val
         val = val + intval
      end do

      do i = 1, size(list, 1)
         val = minv - 0.00000010d0
         do j = 1, size(histogram, 1)
            if (val < list(i) .and. list(i) <= val + intval) then
               histogram(j, 2) = histogram(j, 2) + 1.0d0
               exit
            end if
            val = val + intval
         end do
      end do

   end function
!##########################################

!##########################################
   function nameRandom(obj) result(str)
      class(Random_), intent(inout) :: obj
      character(200) :: str
      integer(int32) :: n

      call obj%init()
      n = int(obj%random()*1000000)

      str = "RandName"//fstring_int(n)
!##########################################

   end function

! Reference: omitakahiro.github.io

!##########################################
   function gauss_scalar_Random(obj, mu, sigma) result(ret)
      class(Random_), intent(inout) :: obj
      real(real64), intent(in) :: mu, sigma
      real(real64) :: ret
      real(real64) :: pi = 3.141592653d0

      ret = sqrt(-2.0d0*log(obj%random()))*sin(2.0d0*pi*obj%random())
      ret = mu + sigma*ret

   end function
!##########################################

!##########################################
   function gauss_vector_Random(obj, mu, sigma, n) result(ret)
      class(Random_), intent(inout) :: obj
      real(real64), intent(in) :: mu, sigma
      real(real64) :: ret(n)
      real(real64) :: pi = 3.141592653d0
      integer(int32), intent(in) :: n
      integer(int32) :: i

      do i = 1, n
         ret(i) = obj%gauss(mu=mu, sigma=sigma)
      end do

   end function
!##########################################

!##########################################
   function ChiSquaredRandom(obj, k) result(ret)
      class(Random_), intent(inout) :: obj
      real(real64), intent(in) :: k
      real(real64) :: ret, z, w
      real(real64) :: pi = 3.141592653d0
      integer(int32) :: i

      w = 0.0d0
      z = 0.0d0
      ret = 0.0d0
      do i = 1, int(k)
         z = sqrt(-2.0d0*log(obj%random()))*sin(2.0d0*pi*obj%random())
         w = w + z*z
      end do
      ret = w

   end function
!##########################################

!##########################################
   function ChauchyRandom(obj, mu, gamma) result(ret)
      class(Random_), intent(inout) :: obj
      real(real64), intent(in) :: mu, gamma
      real(real64) :: ret, z, w
      real(real64) :: pi = 3.141592653d0

      ret = mu + gamma*tan(pi*(obj%random() - 0.50d0))

   end function
!##########################################

!##########################################
   function LognormalRandom(obj, mu, sigma) result(ret)
      class(Random_), intent(inout) :: obj
      real(real64), intent(in) :: mu, sigma
      real(real64) :: ret, z, w
      real(real64) :: pi = 3.141592653d0

      ret = obj%gauss(mu=mu, sigma=sigma)
      ret = exp(ret)

   end function
!##########################################

!##########################################
   function InverseGaussRandom(obj, mu, lambda) result(ret)
      class(Random_), intent(inout) :: obj
      real(real64), intent(in) :: mu, lambda
      real(real64) :: ret, x, y, z, w
      real(real64) :: pi = 3.141592653d0

      x = obj%gauss(mu=0.0d0, sigma=1.0d0)
      y = x*x
      w = mu + 0.50d0*y*mu*mu/lambda - (0.50d0*mu/lambda)*sqrt(4.0d0*mu*lambda*y + mu*mu*y*y)
      z = obj%random()

      if (z < mu/(mu + w)) then
         ret = w
      else
         ret = mu*mu/w
      end if
   end function
!##########################################

!##########################################
   subroutine fillRandom(obj, array, vector)
      class(Random_), intent(inout) :: obj
      real(real64), optional, intent(inout)  :: array(:, :), vector(:)
      integer(int32) :: i, j

      call obj%init()
      if (present(array)) then
         do i = 1, size(Array, 2)
            do j = 1, sizE(Array, 1)
               array(j, i) = obj%random()
            end do
         end do
      end if
      if (present(vector)) then
         do i = 1, size(vector, 1)
            vector(i) = obj%random()
         end do
      end if
   end subroutine
!##########################################

!##########################################
   function cubeRandom(obj, size1, size2, size3) result(ret)
      class(Random_), intent(inout) :: obj
      integer(int32), intent(in) :: size1, size2, size3
      real(real64), allocatable :: ret(:, :, :)
      integer(int32) :: i, j, k

      allocate (ret(size1, size2, size3))
      do i = 1, size3
         do j = 1, size2
            do k = 1, size1
               ret(k, j, i) = obj%random()
            end do
         end do
      end do

   end function
!##########################################

!##########################################
   function matrixRandom(obj, size1, size2) result(ret)
      class(Random_), intent(inout) :: obj
      integer(int32), intent(in) :: size1, size2
      real(real64), allocatable :: ret(:, :)
      integer(int32) :: j, k

      allocate (ret(size1, size2))
      do j = 1, size2
         do k = 1, size1
            ret(k, j) = obj%random()
         end do
      end do

   end function
!##########################################

!##########################################
   function vectorRandom(obj, size1) result(ret)
      class(Random_), intent(inout) :: obj
      integer(int32), intent(in) :: size1
      real(real64), allocatable :: ret(:)
      integer(int32) :: k

      allocate (ret(size1))
      do k = 1, size1
         ret(k) = obj%random()
      end do

   end function
!##########################################

!##########################################
   function scalarRandom(obj, size1) result(ret)
      class(Random_), intent(inout) :: obj
      integer(int32), intent(in) :: size1
      real(real64), allocatable :: ret
      integer(int32) :: k

      ret = obj%random()

   end function
!##########################################

!##########################################
   function randi_range_rank2(valrange, size1, size2) result(ret)
      integer(int32), intent(in) :: valrange(2), size1, size2
      type(Random_) :: random
      integer(int32) :: i, j
      integer(int32), allocatable :: ret(:, :)

      allocate (ret(size1, size2))
      do i = 1, size2
         do j = 1, size1
            ret(j, i) = nint(dble(valrange(2) - valrange(1))*random%random()*1.00050d0 + dble(valrange(1)))
         end do
      end do

   end function
!##########################################

!##########################################
   function randi_imax(imax) result(ret)
      integer(int32), intent(in) :: imax
      type(Random_) :: random
      integer(int32) :: ret

      ret = nint(random%random()*dble(imax - 1) + 1.0d0)

   end function
!##########################################

!##########################################
   function randi_imax_n(imax, n) result(ret)
      integer(int32), intent(in) :: imax, n
      type(Random_) :: random
      integer(int32) :: ret(n), i

      do i = 1, n
         ret(i) = nint(random%random()*dble(imax - 1) + 1.0d0)
      end do
   end function
!##########################################

!##########################################
   function rand_n(n) result(ret)
      integer(int32), intent(in) :: n
      type(Random_) :: random
      real(real64) :: ret(n)
      integer(int32) :: i

      do i = 1, n
         ret(i) = random%random()
      end do

   end function
!##########################################

!##########################################
   function rand_sz2(sz1, sz2) result(ret)
      integer(int32), intent(in) :: sz1, sz2
      type(Random_) :: random
      real(real64) :: ret(sz1, sz2)
      integer(int32) :: i, j
      do j = 1, sz2
         do i = 1, sz1
            ret(i, j) = random%random()
         end do
      end do

   end function
!##########################################

!##########################################
   function rand_sz3(sz1, sz2, sz3) result(ret)
      integer(int32), intent(in) :: sz1, sz2, sz3
      type(Random_) :: random
      real(real64) :: ret(sz1, sz2, sz3)
      integer(int32) :: i, j, k
      do k = 1, sz3
         do j = 1, sz2
            do i = 1, sz1
               ret(i, j, k) = random%random()
            end do
         end do
      end do

   end function
!##########################################

   function whiteRandom(this, num_sample, mu, sigma) result(ret)
      class(Random_), intent(inout) :: this
      real(real64), allocatable :: ret(:)
      real(real64), optional, intent(in) :: mu, sigma
      real(real64) :: mu_d, sigma_d
      integer(int32), intent(in) :: num_sample
      integer(int32) :: i

      allocate (ret(num_sample))
      mu_d = input(default=0.0d0, option=mu)
      sigma_d = input(default=1.0d0, option=sigma)
      do i = 1, num_sample
         ret(i) = this%gauss(mu=mu_d, sigma=sigma_d)
      end do

   end function
! ############################################

   function drawOneRandomInt(this, vec) result(ret)
      class(Random_), intent(in) :: this
      integer(int32), allocatable, intent(inout) :: vec(:)
      integer(int32), allocatable :: buf(:)
      integer(int32) :: ret, id

      ret = 0
      if (.not. allocated(vec)) then
         return
      end if

      if (size(vec) == 1) then
         ret = vec(1)
         deallocate (vec)
         return
      end if

      id = this%randint(1, size(vec))
      ret = vec(id)
      if (id == 1) then

         vec = vec(2:size(vec))

      elseif (id == size(vec)) then
         vec = vec(1:size(vec) - 1)
      else
         buf = vec
         deallocate (vec)
         allocate (vec(size(buf) - 1))
         vec(1:id - 1) = buf(1:id - 1)
         vec(id:) = buf(id + 1:)
         return
      end if

   end function

! #######################################################
   recursive function drawRandomInt(this, vec, num) result(ret)
      class(Random_), intent(in) :: this
      integer(int32), intent(in) :: num
      integer(int32), allocatable, intent(inout) :: vec(:)
      integer(int32), allocatable :: buf(:)
      logical :: selected_entity(num)
      integer(int32) :: i, n
      integer(int32), allocatable :: ret(:)

      if (.not. allocated(vec)) then
         allocate (ret(0))
         return
      end if

      if (size(vec) == 0) then
         allocate (ret(0))
         return
      end if

      n = size(vec)
      allocate (ret(minval([num, n])))
      ret = 0
      do i = 1, minval([num, n])
         ret(i) = this%drawOne(vec)
      end do

   end function
! #################################################
   subroutine shuffleRandomInt(this, intvec)
      class(Random_), intent(in) :: this
      integer(int32), intent(inout) :: intvec(:)
      logical, allocatable :: selected_entity(:)
      integer(int32), allocatable :: retvec(:)
      integer(int32) :: i, n, id, itr

      n = size(intvec)
      allocate (selected_entity(n))
      allocate (retvec(n))
      selected_entity(:) = .false.
      itr = 0
      do
         id = this%randint(1, n)
         if (selected_entity(id)) then
            cycle
         else
            itr = itr + 1
            retvec(itr) = intvec(id)
            selected_entity(id) = .true.
            if (itr == n) then
               intvec = retvec
               return
            end if
         end if
      end do

   end subroutine
! #################################################

function ExpectationValue_r64_i32_i32(func,arg1,arg2,arg3) result(ret)
   interface
      function func(arg1,arg2,arg3) result(ret)
         use iso_fortran_env
         real(real64),intent(in) :: arg1
         integer(int32),intent(in) :: arg2,arg3
         real(real64) :: ret
      end function
   end interface
   
   real(real64),intent(in) :: arg1
   integer(int32),intent(in) :: arg2,arg3
   integer(int32) :: i,n
   real(real64) :: ret

   n = 1000
   ret = 0.0d0
   do i=1,n
      ret = ret + func(arg1,arg2,arg3)
   enddo
   ret = ret/dble(n)

end function


end module
