program main
    use MathClass
    use ArrayClass
    use LinearSolverClass
    implicit none
    
    integer(int32) :: i, num_data,sampling_rate
    real(real64),allocatable :: coeff(:),fit_coeff(:)
    real(real64),allocatable :: true_answer(:)
    real(real64),allocatable :: training_data(:,:)
    
    real(real64) :: err
    type(IO_) :: f
    type(Random_) :: random

    ! create training data
    true_answer = [0.5d0,100.0d0,0.10d0, 1.0d0, 200.0d0, -0.20d0]
    call f%open("step_response.txt","w")
    num_data = 500
    sampling_rate = 100 !Hz

    do i=1,num_data
        write(f%fh,*) dble(i-1)/dble(sampling_rate) ,&
            offset_function(t=dble(i-1)/dble(sampling_rate) ,params=true_answer)&
            +random%gauss(mu=0.0d0,sigma=0.010d0)
    enddo
    
    call f%close()
    call f%plot(option="with lines")
    
    
    ! read training data
    training_data = zeros(num_data,2)
    call f%open("step_response.txt")
    do i=1,size(training_data,1)
        read(f%fh,*) training_data(i,1:2)
    enddo
    call f%close()
    
    ! initial guess
    !(1)coeff = [0.3d0,50.0d0,0.00d0, 1.0d0, 200.0d0, 0.00d0]
    coeff =          [0.10d0,  100.0d0,  0.0d0]
    coeff = coeff // [0.10d0,  200.0d0,  0.0d0]
    coeff = coeff // [0.10d0,  300.0d0,  0.0d0]
    coeff = coeff // [0.10d0,  400.0d0,  0.0d0]
    coeff = coeff // [0.10d0,  500.0d0,  0.0d0]
    coeff = coeff // [0.10d0,  600.0d0,  0.0d0]
    coeff = coeff // [0.10d0,  700.0d0,  0.0d0]

    ! fitting by Robbin-Monro algorithm (SGD)
    do i_i=1,100
        
        fit_coeff = fit(f=offset_function,training_data=training_data, params=coeff ,&
            eta=0.00001d0,max_itr=30, error=err,use_ratio=0.0000030d0,logfile="SGD.log")
        coeff = fit_coeff
        print *, fit_coeff(:)
        
        call f%open("SGD_fit.txt","w")
        do i=0,num_data
            call f%write(dble(i)/100.0d0, offset_function(t=dble(i)/100.0d0,params=fit_coeff ) )
        enddo
        call f%close()

    enddo
    ! show result
    call f%open("training_data.txt","w")
    do i=1,size(training_data,1)
        call f%write(training_data(i,1), training_data(i,2) )
    enddo
    call f%close()

    print *, "--- check ---"
    
    print *, fit_coeff(:)
    print *, true_answer(:)
    
contains

real(real64) function offset_function(t,params)
    real(real64),intent(in) :: t
    real(real64),intent(in) :: params(:)
    integer(int32) :: n
    integer(int32) :: ii

    n = (size(params) - mod(size(params),3))/3
    offset_function = 0.0d0

    do ii=1,n
        offset_function = offset_function + sigmoid(x=t,params=params( (ii-1)*3+1:(ii-1)*3+3 ) ) 
    enddo
    
end function


end program main

