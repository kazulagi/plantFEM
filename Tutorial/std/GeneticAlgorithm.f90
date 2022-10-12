program main
    use GAClass
    use IOClass
    implicit none

    type(GA_) :: solver
    type(IO_) :: f, timeseries
    real(real64),allocatable :: height(:)
    real(real64),allocatable :: NumberOfBranch(:)
    integer(int32) :: i
    real(real64)  :: x, y

    ! ==================================
    !             Problem
    ! ==================================
    ! Maximize f(x) = -2x^3+15x^2-24x-3
    ! subject to 0 <= x <= 100
    ! Ans. max(f(x)) = f(4) = 13
    ! ==================================

    ! ==================================
    ! Solve by Genetic Algorithm (GA)
    ! ==================================

    call f%open("Graph.txt")
    do i=1,100
        x = dble(i)
        y = -2.0d0*x*x*x &
            +15.0d0*x*x &
            -24.0d0*x &
            -3
        call f%write(x,y )
    enddo
    call f%close()

    ! initialize
    call solver%init(&
        num_individual=10000,&
        num_real=2,&
        num_int=0)
    
    ! setup 1st generation

    call solver%setup(&
        DataType=real64,&
        DataID=1,&
        DataRange=[0.0,100.1],&
        DataAnnotation="Height")

    ! dummy data (not necessary)
    call solver%setup(&
        DataType=real64,&
        DataID=2,&
        DataRange=[0.0,10.1],&
        DataAnnotation="NumberOfBranch")

    ! GA optimization loopd
    
    do i=1, 100 ! 100 generations
        
        ! evaluate (random)
        !solver%score = random%randn(solver%num_individual)
        ! select by height
        height = solver%parse(KeyWord="Height")
        NumberOfBranch = solver%parse(KeyWord="NumberOfBranch")
        

        ! score = x*(1-x)
        solver%score(:) = &
        -2.0d0*height(:)*height(:)*height(:) &
            +15.0d0*height(:)*height(:) &
            -24.0d0*height(:) &
            -3.0d0

        call timeseries%open("TimeSeries"//str(i)//".txt")
        do i_i=1,size(height)
            call timeseries%write(height(i_i),solver%score(i_i) )
        enddo
        call timeseries%close()
        
        ! select
        call solver%select(score=solver%score, SurvivalRate=0.01d0)
        
        ! re-generate (crossing)
        call solver%cross()

        if ( mod(i,30)==0 )then
            ! mutation in 1/30
            call solver%mutate(KeyWord="Height",sigma=0.00001d0)
            call solver%mutate(KeyWord="NumberOfBranch",sigma=3.0d0)
        endif

        ! generation, AVE. and SD.
        print *, i,average(height),standardDeviation(height)
    enddo

    height = solver%parse("Height")
    print *, "Height Average : ",average(height)
    print *, "SD : ",standardDeviation(height)

    NumberOfBranch = solver%parse("NumberOfBranch")
    print *, "NumberOfBranch Average : ",average(NumberOfBranch)
    print *, "SD : ",standardDeviation(NumberOfBranch)

end program main