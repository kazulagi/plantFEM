program  main
    use plantFEM
    implicit none

    type(IO_) :: f
    type(Random_) :: random
    real(real64) :: true_ans(1:3,1:20)
    real(real64) :: params(1:3)
    real(real64) :: t
    real(real64) :: dt
    real(real64) :: vel
    real(real64) :: offset
    integer(int32)::step,epock_idx

    call f%open("test_step_response.txt","w")

    dt = 1.0d0/1000.0d0 ! 1 kHz
    
    t = 0.0d0
    do step=1,1000*20
        t = t + dt
        vel = random%gauss(mu=0.0d0,sigma=0.020d0)
        write(f%fh,*) vel
    enddo


    do i_i=1,20
        true_ans(1:3,i_i) = [2.0d0,4.0d0, 0.30d0 ]
    enddo

    offset = 0.0d0

    do epock_idx = 1,20
        ! ########################################
        params = true_ans(1:3,epock_idx)
        params(1) = - params(1)
        t = 0.0d0
        offset = offset - params(1)
        do step=1,1000*5
            t = t + dt
            vel = step_response(t,params) + random%gauss(mu=0.0d0,sigma=0.02d0)+offset
            write(f%fh,*) vel

        enddo

        params = true_ans(1:3,epock_idx)

        offset = offset - params(1)
        t = 0.0d0
        do step=1,1000*5
            t = t + dt
            vel = step_response(t,params) + random%gauss(mu=0.0d0,sigma=0.02d0)+offset
            write(f%fh,*) vel

        enddo
        ! ########################################
    enddo
    call f%close()

contains
    function step_response(t,params) result(ret)
        
        real(real64),intent(in) :: t,params(:)
        real(real64) :: ret,A,w,h,f
        type(Math_) :: math

        A = params(1)
        f = params(2)
        h = params(3)
        w = math%pi*2*f
        ret = A*exp(-h*w*t)*cos(-w*sqrt(1-h*h)*t )

    end function

end program