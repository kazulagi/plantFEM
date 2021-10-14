program main
    use plantFEM
    implicit none

    integer(int32),parameter :: sample=10000
    integer(int32),parameter :: division=10

    type(Random_)::random
    type(IO_) :: f
    
    real(real64) :: list(sample)
    integer(int32) :: i
    real(real64),allocatable :: histogram(:,:)

    call random%init()
    
    ! generate a set of random number
    do i=1,sample
        ! Uniform random numbers
        !list(i) = random%random()
        
        ! binomial distribution
        list(i) = random%random(distribution="binomial")
    enddo

    ! get histogram
    histogram = random%histogram(list=list,division=division)
    
    call print(histogram)

    ! Data
    call f%open("./"//"test"//".txt",'w')
    do i=1,size(histogram,1)
        write(f%fh,*) histogram(i,:)
    enddo
    call f%close()

    ! Gnuplot-script
    call f%open("./"//"test"//".gp",'w')
    call f%write("set yr[0:]")
    call f%write("plot 'test,txt' with boxes")
    call f%write("pause -1")
    call f%close()

    ! plot
    call execute_command_line("gnuplot ./test.gp")
    
end program main