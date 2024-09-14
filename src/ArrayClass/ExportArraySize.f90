integer :: fh
if (present(OptionalFileHandle)) then
   fh = OptionalFileHandle
end if

write (fh, *) size(Mat, RankNum)
