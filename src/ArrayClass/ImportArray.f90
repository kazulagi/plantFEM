
integer i, j, n, m, fh

if (present(OptionalSizeX)) then
   n = OptionalSizeX
elseif (allocated(Mat)) then
   n = size(Mat, 1)
else
   n = 1
   print *, "Caution :: ArrayOperationClass/ImportArray >> No size_X is set"
end if

if (present(OptionalSizeY)) then
   m = OptionalSizeY
elseif (allocated(Mat)) then
   m = size(Mat, 2)
else
   m = 1
   print *, "Caution :: ArrayOperationClass/ImportArray >> No size_Y is set"
end if

if (present(OptionalFileHandle)) then
   fh = OptionalFileHandle
else
   fh = 10
end if

if (.not. allocated(Mat)) then
   allocate (Mat(n, m))
end if

if (size(Mat, 1) /= n .or. size(Mat, 2) /= m) then
   deallocate (Mat)
   allocate (Mat(n, m))
end if

do i = 1, size(Mat, 1)
   read (fh, *) Mat(i, :)
end do
