integer :: fh
if(present(OptionalFileHandle) )then
    fh=OptionalFileHandle
endif

write(fh,*) size(Mat,RankNum)