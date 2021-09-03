program main
    use GAClass
    implicit none

    type(GA_) :: solver
    real(real64),allocatable :: height(:)
    real(real64),allocatable :: NumberOfBranch(:)
    integer(int32) :: i

    ! ==================================
    !             Problem
    ! ==================================
    ! Maximize f(x) = x(1-x) 
    ! subject to 0 <= x <= 100
    ! ==================================

    ! ==================================
    ! Solve by Genetic Algorithm (GA)
    ! ==================================

    ! initialize
    call solver%init(&
        num_individual=10000,&
        num_real=1,&
        num_int=1)
    
    ! setup 1st generation

    call solver%setup(&
        DataType=real64,&
        DataID=1,&
        DataRange=[0.0,100.1],&
        DataAnnotation="Height")
    ! dummy data (not necessary)
    call solver%setup(&
        DataType=int32,&
        DataID=1,&
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
        solver%score(:) = height(:)*(1.0d0-height(:))

        ! select
        call solver%select(score=solver%score, SurvivalRate=0.2d0)
        
        ! re-generate (crossing)
        call solver%cross()

        ! generation, AVE. and SD.
        print *, i,standardDeviation(height)
    enddo

        
    ! selected for height
    height = solver%parse("Height")
    print *, "Height Average : ",average(height)
    print *, "SD : ",standardDeviation(height)

    ! and not for number of branch
    NumberOfBranch = solver%parse("NumberOfBranch")
    print *, "NumberOfBranch Average : ",average(NumberOfBranch)
    print *, "SD : ",standardDeviation(NumberOfBranch)




end program main