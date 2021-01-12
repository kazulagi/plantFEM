integer :: fh,i

if(present(OptionalFileHandle) )then
    fh=OptionalFileHandle
else
    fh=10
endif

do i=1,size(Mat,1)
    write(fh,*) Mat(i,:)
enddo
