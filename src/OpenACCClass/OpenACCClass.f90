module OpenACCClass
   use iso_fortran_env
   implicit none

contains

   subroutine acc_crs_matvec(CRS_value, CRS_col, CRS_row_ptr, old_vector, new_vector)
      real(real64), intent(in)  :: CRS_value(:), Old_vector(:)
      integeR(int32), intent(in):: CRS_col(:), CRS_row_ptr(:)

      real(real64), allocatable, intent(inout) :: new_vector(:)
      integer(int32) :: i, j, n, gid, lid, row, CRS_id, col
      !> x_i = A_ij b_j

      n = size(CRS_row_ptr) - 1
      if (size(old_vector) /= n) then
         print *, "ERROR crs_matvec :: inconsistent size for old_vector"
         return
      end if

      if (.not. allocated(new_vector)) then
         allocate (new_vector(n))
         new_vector(:) = 0.0d0
      end if

    !!$acc kernels
      do row = 1, n
         do CRS_id = CRS_row_ptr(row), CRS_row_ptr(row + 1) - 1
            new_vector(row) = new_vector(row) + CRS_value(CRS_id)*old_vector(CRS_col(CRS_id))
         end do
      end do
    !!$acc end kernels

   end subroutine
! ###################################################################

! #####################################################
   subroutine acc_bicgstab_CRS(a, ptr_i, index_j, x, b, itrmax, er, relative_er, debug)
      integer(int32), intent(inout) :: ptr_i(:), index_j(:), itrmax
      real(real64), intent(inout) :: a(:), b(:), er
      real(real64), optional, intent(in) :: relative_er
      real(real64), intent(inout) :: x(:)
      logical, optional, intent(in) :: debug
      logical :: speak = .false.
      integer(int32) :: itr, i, j, n
      real(real64)  :: alp, bet, c1, c2, c3, ev, vv, rr, er0, init_rr, re_er0
      real(real64), allocatable:: r(:), r0(:), p(:), y(:), e(:), v(:), ax(:)

      er0 = er
      if (present(debug)) then
         speak = debug
      end if

      n = size(b)
      if (speak) print *, "BiCGSTAB STARTED >> DOF:", n
      allocate (r(n), r0(n), p(n), y(n), e(n), v(n))

      r(:) = b(:)
      if (speak) print *, "BiCGSTAB >> [1] initialize"

      call acc_crs_matvec(CRS_value=a, CRS_col=index_j, &
                          CRS_row_ptr=ptr_i, old_vector=x, new_vector=ax)
      r = b - ax

      if (speak) print *, "BiCGSTAB >> [2] dp1"

      c1 = dot_product(r, r)

      init_rr = c1
      !if(speak) print *, "BiCGSTAB >>      |r|^2 = ",init_rr

      if (c1 < er0) return
      !endif

      p(:) = r(:)
      r0(:) = r(:)

      do itr = 1, itrmax
         if (speak) print *, "BiCGSTAB >> [", itr, "] initialize"

         c1 = dot_product(r0, r)

         y(:) = 0.0d0
         call acc_crs_matvec(CRS_value=a, CRS_col=index_j, &
                             CRS_row_ptr=ptr_i, old_vector=p, new_vector=y)

         c2 = dot_product(r0, y)

         alp = c1/c2

         e(:) = r(:) - alp*y(:)
         v(:) = 0.0d0
         call acc_crs_matvec(CRS_value=a, CRS_col=index_j, &
                             CRS_row_ptr=ptr_i, old_vector=e, new_vector=v)

         if (speak) print *, "BiCGSTAB >> [", itr, "] half"

         ev = dot_product(e, v)
         vv = dot_product(v, v)

         if (vv == 0.0d0) stop "Bicgstab devide by zero"

         c3 = ev/vv
         x(:) = x(:) + alp*p(:) + c3*e(:)
         r(:) = e(:) - c3*v(:)

         rr = dot_product(r, r)

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

         if (sqrt(rr) < er0) exit

         c1 = dot_product(r0, r)

         bet = c1/(c2*c3)

         if ((c2*c3) == 0.0d0) stop "Bicgstab devide by zero"
         p(:) = r(:) + bet*(p(:) - c3*y(:))

      end do

   end subroutine
!===============================================================

!===============================================================
   subroutine acc_ref_bicgstab_real64(a, b, x, n, itrmax, er)
      integer(int32), intent(in) :: n, itrmax
      real(real64), intent(in) :: a(n, n), b(n), er
      real(real64), intent(inout) :: x(n)
      integer(int32) itr
      real(real64) alp, bet, c1, c2, c3, ev, vv, rr, er0, init_rr
      real(real64) r(n), r0(n), p(n), y(n), e(n), v(n)
      er0 = er

      r(:) = b - matmul(a, x)
      c1 = dot_product(r, r)
      init_rr = c1
      if (c1 < er0) return
      p(:) = r(:)
      r0(:) = r(:)
      do itr = 1, itrmax

         c1 = dot_product(r0, r)

         y(:) = matmul(a, p)

         c2 = dot_product(r0, y)

         alp = c1/c2

         e(:) = r(:) - alp*y(:)
         v(:) = matmul(a, e)

         ev = dot_product(e, v)
         vv = dot_product(v, v)
         if (vv == 0.0d0) stop "Bicgstab devide by zero"

         c3 = ev/vv

         x(:) = x(:) + alp*p(:) + c3*e(:)
         r(:) = e(:) - c3*v(:)
         rr = dot_product(r, r)

         !    write(*,*) 'itr, er =', itr,rr
         if (rr/init_rr < er0) exit
         c1 = dot_product(r0, r)
         bet = c1/(c2*c3)

         if ((c2*c3) == 0.0d0) stop "Bicgstab devide by zero"

         p(:) = r(:) + bet*(p(:) - c3*y(:))

      end do
   end subroutine
   !===============================================================

end module OpenACCClass
