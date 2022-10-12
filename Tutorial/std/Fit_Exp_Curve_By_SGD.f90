program main
    use LinearSolverClass
    implicit none
    
    integer(int32) :: i
    real(real64),allocatable :: coeff(:),fit_coeff(:)
    real(real64) :: true_answer(4)
    real(real64),allocatable :: training_data(:,:)
    real(real64) :: err
    type(IO_) :: f
    type(Random_) :: random

    
    ! create training data
    ! trial : h = 0.70, w = 20.0, A=2.0, phi = 0.0
    true_answer = [0.70d0, 12.0d0,1.0d0,  -1.570d0]
    call f%open("step_response.txt","w")
    do i=1,10000
        write(f%fh,*) dble(i-1)/10000.0d0,&
            sample_function(t=dble(i-1)/10000.0d0,params=true_answer)&
            +random%gauss(mu=0.0d0,sigma=0.0050d0)
    enddo
    call f%close()
    
    ! read training data
    training_data = zeros(10000,2)
    call f%open("step_response.txt")
    do i=1,size(training_data,1)
        read(f%fh,*) training_data(i,1:2)
    enddo
    call f%close()
    
    ! trial : theta_h = 0.50, w = 20.0, A=2.0, phi = 0.0
    coeff = [0.50d0, 20.0d0,2.0d0,0.0d0]

    !sample_function = A*exp(-h*w*t)*cos(w_*t - phi)
    call f%open("SGD_fit_initial.txt","w")
    do i=0,1000
        call f%write(dble(i)/1000.0d0, exp_function(t=dble(i)/1000.0d0,params=coeff ) )
    enddo
    call f%close()
    

    ! fitting by Robbin-Monro algorithm (SGD)
    do i_i=1,2000
        
        fit_coeff = fit(f=exp_function, training_data=training_data, params=coeff ,&
            eta=0.001d0,max_itr=10, error=err,use_ratio=0.00050d0,logfile="SGD.log")
        coeff = fit_coeff
        print *, 1.0d0/(1.0d0 + exp(- fit_coeff(1) ) ),fit_coeff(2:)
        !call f%open("SGD_fit"+zfill(i_i,4)+".txt","w")
        call f%open("SGD_fit.txt","w")
        do i=0,1000
            call f%write(dble(i)/1000.0d0, exp_function(t=dble(i)/1000.0d0,params=fit_coeff ) )
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
    
    print *, 1.0d0/(1.0d0 + exp(- fit_coeff(1) ) ),fit_coeff(2:)
    print *, true_answer(:) 
    

contains


real(real64) function exp_function(t,params)
    real(real64),intent(in) :: t
    real(real64),intent(in) :: params(:)
    real(real64) :: A, h, w, phi, w_,theta_h
    theta_h   = params(1)
    h = 1.0d0/(1.0d0 + exp(- theta_h) )
    w   = params(2)
    A   = params(3)
    phi = params(4)
    
    w_  = w * sqrt(1-h*h)
    
    exp_function = A*exp(-h*w*t)*cos(w_*t - phi)
    
end function


end program main

