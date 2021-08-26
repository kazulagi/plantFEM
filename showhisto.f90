program main
    use plantFEM
    implicit none

    type(Random_) ::random
    type(IO_) :: f
    integer(int32) :: i
    real(real64),allocatable :: histogram(:,:)
    real(real64) :: list(37043)

    call f%open("soyvol.txt","r")
    do i=1,size(list)
        list(i) = freal(f%readline() )
    enddo
    call f%close()
    
    histogram =  random%histogram(list=list,division=50)

    call f%open("soyvol_hist.txt")

    do i=1,size(histogram,1)
        write(f%fh,*) histogram(i,1),histogram(i,2)
    enddo
    call f%close()
    

    call f%open("soyvol_raw.txt")
    do i=1,size(list,1)
        write(f%fh,*) i,list(i)
    enddo
    call f%close()

end program