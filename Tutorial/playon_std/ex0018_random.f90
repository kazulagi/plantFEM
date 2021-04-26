program main
    use plantFEM
    implicit none

    type(Random_) ::random
    type(IO_) :: f
    integer(int32) :: i
    real(real64),allocatable :: histogram(:,:)
    real(real64) :: list(10000)

    do i=1,10000
        list(i) = random%gauss(mu=10.0d0,sigma=2.0d0)
    enddo
    
    histogram =  random%histogram(list=list,division=20)

    call f%open("gauss.txt")

    do i=1,size(histogram,1)
        write(f%fh,*) histogram(i,1),histogram(i,2)
    enddo
    call f%close()
    

    do i=1,10000
        list(i) = random%ChiSquared(K=10.0d0)
    enddo
    
    histogram =  random%histogram(list=list,division=20)

    call f%open("ChiSquared.txt")
    
    do i=1,size(histogram,1)
        write(f%fh,*) histogram(i,1),histogram(i,2)
    enddo

    call f%close()

    do i=1,10000
        list(i) = random%Chauchy(mu=10.0d0,gamma=2.0d0)
    enddo
    
    histogram =  random%histogram(list=list,division=20)

    call f%open("Chauchy.txt")
    
    do i=1,size(histogram,1)
        write(f%fh,*) histogram(i,1),histogram(i,2)
    enddo
    call f%close()


    do i=1,10000
        list(i) = random%Lognormal(mu=10.0d0,sigma=2.0d0)
    enddo
    
    histogram =  random%histogram(list=list,division=20)

    call f%open("Lognormal.txt")
    
    do i=1,size(histogram,1)
        write(f%fh,*) histogram(i,1),histogram(i,2)
    enddo
    call f%close()



    do i=1,10000
        list(i) = random%InverseGauss(mu=10.0d0,lambda=2.0d0)
    enddo
    
    histogram =  random%histogram(list=list,division=20)

    call f%open("InverseGauss.txt")
    
    do i=1,size(histogram,1)
        write(f%fh,*) histogram(i,1),histogram(i,2)
    enddo
    
    call f%close()

end program