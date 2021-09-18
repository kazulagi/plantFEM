program main
    use plantFEM
    implicit none

    type(Random_) :: random
    type(IO_) :: f,f2
    integer(int32) :: i,j,ret(20,5),n
    real(real64) :: val,id_accel(5),accel(20,5),vel(20,6),disp_val(20,6),dt,score(20)


    dt=0.10d0

    id_accel(1)=800.0d0
    id_accel(2)=300.0d0
    id_accel(3)=0.0d0
    id_accel(4)=-200.0d0
    id_accel(5)=-500.0d0

    print *, "GA :1 , just for score : 2"
    read(*,*) n 

    call random%init()
    call f%open("./"//"generation1"//".txt",'w')
    if(n==1)then


        ! create 1st generation
        do i=1,20
            do j=1,5
                val =random%random()
                val = val*5.0d0
                ret(i,j)=int(val)+1
                accel(i,j) = id_accel(ret(i,j))
            enddo
        enddo

    else
        call f2%open("./"//"gene"//".txt",'w')
        do i=1,20
            read(f2%fh,*) ret(i,1:5)
        enddo
        call f2%close()
        do i=1,20
            do j=1,5
                accel(i,j) = id_accel(ret(i,j))
            enddo
        enddo
    endif

    ! compute score
    do i=1,size(accel,1)
        vel(i,1) = 40.0d0
        vel(i,2) = vel(i,1) + accel(i,1)*dt
        vel(i,3) = vel(i,2) + accel(i,2)*dt
        vel(i,4) = vel(i,3) + accel(i,3)*dt
        vel(i,5) = vel(i,4) + accel(i,4)*dt
        vel(i,6) = vel(i,5) + accel(i,5)*dt

        disp_val(i,1) = 0.0d0
        disp_val(i,2) = disp_val(i,1) + vel(i,1)*dt + accel(i,1)*dt*dt
        disp_val(i,3) = disp_val(i,2) + vel(i,2)*dt + accel(i,2)*dt*dt
        disp_val(i,4) = disp_val(i,3) + vel(i,3)*dt + accel(i,3)*dt*dt
        disp_val(i,5) = disp_val(i,4) + vel(i,4)*dt + accel(i,4)*dt*dt
        disp_val(i,6) = disp_val(i,5) + vel(i,5)*dt + accel(i,5)*dt*dt

        score(i) = disp_val(i,6)

        ! 速度制限
        if(vel(i,2) > 100.0d0 .or. vel(i,3) > 100.0d0 )then
            score(i)=-1000.0d0
        endif
        if(vel(i,3) > 50.0d0 .or. vel(i,4) > 50.0d0 )then
            score(i)=-1000.0d0
        endif
        if(vel(i,5) > 100.0d0 .or. vel(i,6) > 100.0d0 )then
            score(i)=-1000.0d0
        endif

    enddo

    do i=1,20
        write(f%fh,*) ret(i,:),"| score : ",score(i)
    enddo
    write(f%fh,*)
    call f%close()


end program main