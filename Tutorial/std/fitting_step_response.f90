program main
    use IOClass
    use LinearSolverClass
    implicit none

    integer(int32) :: n
    type(IO_) :: f
    character(:),allocatable :: filename
    real(real64),allocatable :: training_data(:,:),params(:)
    
    call f%download("https://plantfem.org/download/test_step_response.txt")

    ! read file and extract step-response parts
    ! each step-response is saved as "test_step_response.txt_"+zfill(i_i,4)+".txt"
    call extractStepResponse(&
        name="test_step_response.txt",&
        trigger_level=0.30d0,&
        buffer_size=10,&
        segment_length=2048,&
        segment_num = n ,&
        dt=1.0d0/100.0d0)
    
    ! fitting 
    do i_i=1,n
        filename = "test_step_response.txt_"+zfill(i_i,4)+".txt"
        training_data = to_array(filename,[ f%numLine(filename) ,2])
        params = dble([0.37,-0.88,-0.47,2.00,-0.100])


        params = StochasticGradientDescent(  &
            fx = step_response, &
            training_data_x  = training_data(:,1),&
            training_data_fx = training_data(:,2),&
            init_params      = params, &
            eta = 0.05d0 ,&
            max_itr = 50000,&!100000,&
            tol = dble(1.0e-22) &
        )
        print *, params
        
    enddo

    call f%open("plot_all_segment.gp","w")
    call f%write("plot 'test_step_response.txt_"+zfill(1,4)+".txt' w l ")    
    print *, n
    do i_i = 2,n-1
        call f%write("replot 'test_step_response.txt_"+zfill(i_i,4)+".txt' w l ")    
    enddo
    call f%close()


contains

function step_response(t,params) result(ret)
    real(real64),intent(in) :: t,params(:)
    real(real64) :: ret,A,w,h,offset,phi

    A = params(1)
    w = params(2)
    h = params(3)
    offset = params(4)
    phi = params(5)

    ret = A*exp(-h*w*t)*cos(-w*sqrt(1-h*h)*t - phi )+offset
    
end function


end program