module PhysicsClass
   use iso_fortran_env
   use ArrayClass
   implicit none

   !real(real64) :: c = dble(2.99792458E+008) ! light speed
   !real(real64) :: Na = dble(6.02214179E+023)! Avogadro constant
   !real(real64) :: R  = dble(8.314472)! Avogadro constant　J/(K・mol)
contains

   recursive function SurfaceWave2DispersionCurve(waves, positions, c_axis, fft_size, dt, threshold) result(ret)
      real(real64), intent(in) :: waves(:, :) ! 1st: time-series, 2nd: channel
      real(real64), intent(in) :: positions(:) ! positions for each point
      real(real64), intent(in) :: c_axis(:), dt
      real(real64), optional, intent(in) ::threshold
      integer(int32), intent(in) :: fft_size
      real(real64), allocatable :: ret(:, :) ! |V(w,phi)| of Park (MULTIMODAL ANALYSIS OF HIGH FREQUENCY SURFACE WAVES)
      real(real64) :: w, x, max_freq, f, phi, threshold_val

      type(Math_)  :: math
      complex(real64), allocatable :: V(:, :)
      real(real64), allocatable    :: u(:), freq(:), buf(:, :)
      complex(real64), allocatable :: U_fft(:)
      integer(int32), allocatable  :: segments(:, :)
      integer(int32) :: i, j, k, n, from, to, count_n
      threshold_val = input(default=0.50d0, option=threshold)
      if (size(waves, 1) > fft_size) then
         ! stacking
         ! 50% overlap
         n = size(waves, 1)
         segments = get_segments(data_len=n, segment_len=fft_size, overlap_percent=30) ! 30% overlap
         ret = zeros(fft_size/2, size(c_axis))
         count_n = 0
         do i = 1, size(segments, 1)
            from = segments(i, 1)
            to = segments(i, 2)
            buf = SurfaceWave2DispersionCurve(waves(from:to, :), positions, c_axis, fft_size, dt)
            if (maxval(buf) < threshold_val) then
               cycle
            else
               ret = ret + buf
               count_n = count_n + 1
            end if
         end do
         ret = 1.0d0/dble(count_n)*ret

      else
         V = zeros(fft_size/2, size(c_axis))
         max_freq = 1.0d0/dt/2.0d0
         freq = linspace([0.0d0, max_freq], fft_size/2)
         do i = 1, size(positions)
            x = positions(i)
            u = waves(1:fft_size, i)
            U_fft = FFT(to_complex(u))
            do j = 1, fft_size/2
               f = freq(j)
               w = 2.0d0*math%pi*f
               do k = 1, size(c_axis)
                  phi = w/c_axis(k)
                  V(j, k) = V(j, k) + exp(math%i*phi*x)*U_fft(j)/abs(U_fft(j))
               end do
            end do
         end do
         V = V/size(positions)
         ret = dble(V)
      end if

   end function

end module
