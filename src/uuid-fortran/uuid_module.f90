!*****************************************************************************************
!>
!  UUID generation
!
!  This generates UUIDs according to RFC 4122
!  Only types 1 (time-based) and 4 (pseudo-RNG-based) are implemented.
!
!  UUIDs (see RFC 4122) are Universally Unique IDentifiers.
!  They are a 128-bit number, represented as a 36-character string. For example:
!
!     f81d4fae-7dec-11d0-a765-00a0c91e6bf6
!
!### See also
!  * Based on code from Fox: A Fortran XML Library
!    https://github.com/andreww/fox
!  * http://homepages.see.leeds.ac.uk/~earawa/FoX/DoX/FoX_utils.html
!
!### Licenses
!
!---------------------------------------------------------------------
! FoX - Fortran XML library
!---------------------------------------------------------------------
!
! FoX was originally derived from the xmlf90 codebase,
! (c) Alberto Garcia & Jon Wakelin, 2003-2004.
!
! FoX also includes externally-written code from
! Scott Ladd <scott.ladd@coyotegulch.com>, which is licensed
! as shown in the file utils/fox_m_utils_mtprng.f90
!
! This version of FoX is:
! (c) 2005-2009 Toby White <tow@uszla.me.uk>
! (c) 2007-2009 Gen-Tao Chiang <gtc25@cam.ac.uk>
! (c) 2008-2012 Andrew Walker <a.walker@ucl.ac.uk>
!
! All rights reserved.
!
! * Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are
! met:
!
! * Redistributions of source code must retain the above copyright notice,
! this list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright
! notice, this list of conditions and the following disclaimer in the
! documentation and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
! contributors may be used to endorse or promote products derived from
! this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
! A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
! OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
!---------------------------------------------------------------------
! mtprng.f90 (a Fortran 95 module)
!---------------------------------------------------------------------
!
!  An implementation of the Mersenne Twister algorithm for generating
!  psuedo-random sequences.
!
!  ORIGINAL ALGORITHM COPYRIGHT
!  ============================
!  Copyright (C) 1997,2002 Makoto Matsumoto and Takuji Nishimura.
!  Any feedback is very welcome. For any question, comments, see
!  http://www.math.keio.ac.jp/matumoto/emt.html or email
!  matumoto@math.keio.ac.jp
!
!  COPYRIGHT NOTICE, DISCLAIMER, and LICENSE:
!
!  This notice applies *only* to this specific expression of this
!  algorithm, and does not imply ownership or invention of the
!  implemented algorithm.
!
!  If you modify this file, you may insert additional notices
!  immediately following this sentence.
!
!  Copyright 2001, 2002, 2004 Scott Robert Ladd.
!  All rights reserved, except as noted herein.
!
!  This computer program source file is supplied "AS IS". Scott Robert
!  Ladd (hereinafter referred to as "Author") disclaims all warranties,
!  expressed or implied, including, without limitation, the warranties
!  of merchantability and of fitness for any purpose. The Author
!  assumes no liability for direct, indirect, incidental, special,
!  exemplary, or consequential damages, which may result from the use
!  of this software, even if advised of the possibility of such damage.
!
!  The Author hereby grants anyone permission to use, copy, modify, and
!  distribute this source code, or portions hereof, for any purpose,
!  without fee, subject to the following restrictions:
!
!      1. The origin of this source code must not be misrepresented.
!
!      2. Altered versions must be plainly marked as such and must not
!         be misrepresented as being the original source.
!
!      3. This Copyright notice may not be removed or altered from any
!         source or altered source distribution.
!
!  The Author specifically permits (without fee) and encourages the use
!  of this source code for entertainment, education, or decoration. If
!  you use this source code in a product, acknowledgment is not required
!  but would be appreciated.

module uuid_module

   implicit none

   private

   integer, parameter :: INT64 = selected_int_kind(18)
   integer, parameter :: INT32 = selected_int_kind(9)

   integer(INT32), parameter :: mtprng_N = 624_INT32
   integer(INT32), parameter :: mtprng_M = 397_INT32

   character(len=1), dimension(0:15), parameter :: hexdigits = &
                                                   ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']

   type mtprng_state
      integer(INT32) :: mti = -1_INT32
      integer(INT64), dimension(0:mtprng_N - 1) :: mt = 0_INT64
   end type

   type(mtprng_state) :: rng_state
   logical :: initialized = .false.
   integer :: values_save = 0
   integer(kind=INT32) :: hires_count = 0

   integer, save :: clock_seq = 0  !! clock-seq holds a random number
                                        !! constant for the lifetime of the program
                                        !! using this module. That's the best we
                                        !! can do per S 4.1.5

   public :: generate_uuid

contains
   !*****************************************************************************************

   function generate_uuid(version) result(uuid)

      integer, intent(in), optional :: version    !! identifies the version of UUID to be
                                                        !! used (see section 4.1.3 of the RFC).
                                                        !! Only versions 0, 1, and 4 are supported.
                                                        !! Version 0 generates a nil UUID; version 1 a
                                                        !! time-based UUID, and version 4 a
                                                        !! pseudo-randomly-generated UUID.
                                                        !!
                                                        !! Version 1 is the default, and is recommended.

      character(len=36) :: uuid

      integer(kind=INT64) :: timestamp, node
      integer(kind=INT32) :: clock_sequence
      integer(kind=INT32) :: time_low, time_mid, time_hi_and_version
      integer(kind=INT32) :: clk_seq_hi_res, clk_seq_low
      integer, dimension(8) :: values !! must be default for `date_and_time`
      integer(kind=INT32) :: variant, v

      if (.not. initialized) then
         ! Use the current date and time to init mtprng
         ! but this gives limited varaibility, so mix
         ! the result up.  Can we do better? In any
         ! case, this gets passed through a quick
         ! generator inside mtprng_init.
         call date_and_time(values=values)
         values(7) = values(7)*1000 + values(5)*100 + values(3)*10 + values(1)
         values(8) = values(2)*1000 + values(4)*100 + values(6)*10 + values(8)
         call mtprng_init(int(values(7)*10000 + values(8), INT32), rng_state)
         clock_seq = int(mtprng_rand64(rng_state), INT32)
         initialized = .true.
      end if

      variant = 1

      if (present(version)) then
         v = version
      else
         v = 4
      end if

      select case (v)
      case (0)
         ! Nil UUID  - S 4.1.7
         uuid = repeat('0', 8)//'-'//repeat('0', 4)//'-'//repeat('0', 4)// &
                '-'//repeat('0', 4)//'-'//repeat('0', 12)
         return
      case (1)
         call date_and_time(values=values)
         ! In case of too-frequent requests, we will replace time_low
         ! with the count below ...
         if (all(values == values_save)) then
            hires_count = hires_count + 1
         else
            hires_count = 0
         end if
      case (2:3)
         !Unimplemented
         uuid = ''
         return
      case (4)
         continue
      case (5)
         !Unimplemented
         uuid = ''
         return
      case default
         !Unspecified
         uuid = ''
         return
      end select

      !4.1.4 Timestamp
      select case (v)
      case (1)
         timestamp = get_utc_since_1582(values)
      case (4)
         timestamp = ior(mtprng_rand64(rng_state), ishft(mtprng_rand64(rng_state), 28))
      end select

      !4.1.5 Clock Sequence
      ! 14 bits
      select case (v)
      case (1)
         clock_sequence = clock_seq
      case (4)
         clock_sequence = int(mtprng_rand64(rng_state), INT32)
      end select

      !4.1.6 Node
      ! 48 bits
      select case (v)
      case (1)
         node = ior(mtprng_rand64(rng_state), ishft(mtprng_rand64(rng_state), 16))
         ! No MAC address accessible - see section 4.5 !FIXME
      case (4)
         node = ior(mtprng_rand64(rng_state), ishft(mtprng_rand64(rng_state), 16))
      end select

      time_low = ibits(timestamp, 0, 32)
      time_mid = ibits(timestamp, 32, 16)
      if (hires_count == 0) then
         time_hi_and_version = ior(int(ibits(timestamp, 48, 12), INT32), ishft(v, 12))
      else
         time_hi_and_version = ior(hires_count, ishft(v, 12))
      end if

      clk_seq_low = ibits(clock_sequence, 0, 8)
      clk_seq_hi_res = ior(ibits(clock_sequence, 8, 6), ishft(variant, 6))

      uuid = int32ToHexOctets(time_low, 4)//"-"// &
             int32ToHexOctets(time_mid, 2)//"-"// &
             int32ToHexOctets(time_hi_and_version, 2)//"-"// &
             int32ToHexOctets(clk_seq_hi_res, 1)// &
             int32ToHexOctets(clk_seq_low, 1)//"-"// &
             int64ToHexOctets(node, 6)

   contains

      function int32ToHexOctets(b, n) result(s)

         integer(INT32), intent(in) :: b
         integer, intent(in) :: n ! number of octets to print
         character(len=2*n) :: s

         integer :: i

         do i = 0, 2*n - 1
            s(2*n - i:2*n - i) = hexdigits(ibits(b, i*4, 4))
         end do

      end function int32ToHexOctets

      function int64ToHexOctets(b, n) result(s)
         integer(INT64), intent(in) :: b
         integer, intent(in) :: n ! number of octets to print
         character(len=2*n) :: s

         integer :: i

         do i = 0, 2*n - 1
            s(2*n - i:2*n - i) = hexdigits(ibits(b, i*4, 4))
         end do

      end function int64ToHexOctets

   end function generate_uuid

   function get_utc_since_1582(values) result(ns)

            !! This subroutine is a little broken. It only works
            !! for times after 1/1/2006 and takes no account
            !! of any future leapseconds. It ought to serve regardless.
            !!
            !! It returns the number of 100-ns intervals since 1582-10-15-00-00-00

      integer, dimension(8), intent(in) :: values
      integer(kind=INT64) :: ns

      integer :: days
      integer :: years

      integer, parameter :: days_in_normal_year(12) = &
                            [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

      ns = 23_INT64*1000_INT64*1000_INT64*10_INT64 ! 23 leap seconds until 24:00:00 31/12/2005

      ! A count of the 100-nanosecond intervals since the
      ! beginning of the day.
      ns = ns &
           ! milliseconds
           + int(values(8), INT64)*10_INT64*1000_INT64 &
           ! seconds
           + int(values(7), INT64)*10_INT64*1000_INT64*1000_INT64 &
           ! minutes (with timezone adjustment)
           + int(values(6) + values(4), INT64)*10_INT64*1000_INT64*1000_INT64*60_INT64 &
           ! hours
           + int(values(5), INT64)*10_INT64*1000_INT64*1000_INT64*60_INT64*60_INT64

      ! Number of days this year:
      days = sum(days_in_normal_year(:values(2) - 1))
      days = days + values(3) - 1 !add days in current month
      if (values(2) > 2 .and. isLeapYear(values(1))) then
         days = days + 1
      end if
      !That's all the time since the turn of this year

      days = days + 78 ! From the start of 15th Oct to the end of 31st Dec in 1582
      !That's the additional time before the turn of the year 1583

      days = days + 102  ! 102 leap years from 1584 to 2000 inclusive
      ! That's all the intercalated days until 2000

      years = values(1) - 2000 - 1 ! years since 2000 - not including this year

      days = days + years/4 - years/100 + years/400 !Add extra leap days to this total:
      ! That's all out intercalated days - remaining years are all 365 days long.

      years = years + 418 ! Add the years from 1583-2000 inclusive back on.

      ! Multiply by number of time units in one day & add to today's total.
      ns = ns + 864000000000_INT64*(int(days, INT64) + 365_INT64*int(years, INT64))

   contains
      function isLeapYear(y) result(p)
         integer, intent(in) :: y
         logical :: p
         p = (mod(y, 4) == 0 .and. .not. mod(y, 100) == 0 .or. mod(y, 400) == 0)
      end function isLeapYear

   end function get_utc_since_1582

   subroutine mtprng_init(seed, state)

            !! Initializes the generator with "seed"

      integer(INT32), intent(in)  :: seed
      type(mtprng_state), intent(out) :: state

      integer :: i  !! working storage

      ! save seed
      state%mt(0) = seed

      ! Set the seed using values suggested by Matsumoto & Nishimura, using
      !   a generator by Knuth. See original source for details.
      do i = 1, mtprng_N - 1
         state%mt(i) = iand(4294967295_INT64, 1812433253_INT64*ieor(state%mt(i - 1), ishft(state%mt(i - 1), -30_INT64)) + i)
      end do

      state%mti = mtprng_N

   end subroutine mtprng_init

   function mtprng_rand64(state) result(r)

            !! Obtain the next 32-bit integer in the psuedo-random sequence
            !! Uses the Mersenne Twister algorithm

      type(mtprng_state), intent(inout) :: state
      integer(INT64) :: r

      ! internal constants
      integer(INT64), dimension(0:1), parameter :: mag01 = [0_INT64, -1727483681_INT64]

      ! Period parameters
      integer(INT64), parameter :: UPPER_MASK = 2147483648_INT64
      integer(INT64), parameter :: LOWER_MASK = 2147483647_INT64

      ! Tempering parameters
      integer(INT64), parameter :: TEMPERING_B = -1658038656_INT64
      integer(INT64), parameter :: TEMPERING_C = -272236544_INT64

      ! Note: variable names match those in original example
      integer(INT32) :: kk

      ! Generate N words at a time
      if (state%mti >= mtprng_N) then
         ! The value -1 acts as a flag saying that the seed has not been set.
         if (state%mti == -1) call mtprng_init(4357_INT32, state)

         ! Fill the mt array
         do kk = 0, mtprng_N - mtprng_M - 1
            r = ior(iand(state%mt(kk), UPPER_MASK), iand(state%mt(kk + 1), LOWER_MASK))
            state%mt(kk) = ieor(ieor(state%mt(kk + mtprng_M), ishft(r, -1_INT64)), mag01(iand(r, 1_INT64)))
         end do

         do kk = mtprng_N - mtprng_M, mtprng_N - 2
            r = ior(iand(state%mt(kk), UPPER_MASK), iand(state%mt(kk + 1), LOWER_MASK))
            state%mt(kk) = ieor(ieor(state%mt(kk + (mtprng_M - mtprng_N)), ishft(r, -1_INT64)), mag01(iand(r, 1_INT64)))
         end do

         r = ior(iand(state%mt(mtprng_N - 1), UPPER_MASK), iand(state%mt(0), LOWER_MASK))
         state%mt(mtprng_N - 1) = ieor(ieor(state%mt(mtprng_M - 1), ishft(r, -1)), mag01(iand(r, 1_INT64)))

         ! Start using the array from first element
         state%mti = 0
      end if

      ! Here is where we actually calculate the number with a series of
      !   transformations
      r = state%mt(state%mti)
      state%mti = state%mti + 1

      r = ieor(r, ishft(r, -11))
      r = iand(4294967295_INT64, ieor(r, iand(ishft(r, 7), TEMPERING_B)))
      r = iand(4294967295_INT64, ieor(r, iand(ishft(r, 15), TEMPERING_C)))
      r = ieor(r, ishft(r, -18))

   end function mtprng_rand64

   !*****************************************************************************************
end module uuid_module
!*****************************************************************************************