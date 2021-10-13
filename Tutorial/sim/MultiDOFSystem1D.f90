program main
    use multiDOFsystemClass
    implicit none

    type(FEMDomain_) :: soil
    type(multiDOFsystem_) :: multiDOF
    type(IO_) :: f,gp
    type(Math_) :: math
    integer(int32) :: i
    real(real64) :: u_

    ! CAUTION this code is experimental.
    
    call soil%create(meshtype="Bar1D",x_num=200)
    call soil%resize(x=100.0d0)
    call soil%move(x=-100.0d0)

    call multiDOF%init(soil)
    multiDOF%k(:) = 10000.0d0
    multiDOF%k(70:150) = 1000.0d0
    
    multiDOF%m(:) = 1.0d0
    multiDOF%c(:) = 0.00129d0*multiDOF%k(:) + 0.52400d0*multiDOF%m(:)
    call gp%open("movie.gp")
    
    do i=1,20000
        print *, "STEP : ",i
        u_= 0.0010d0*sin(dble(i-1)/math%PI/50.0d0)
        if(i>1000)then
            u_= 0.0d0
            multiDOF%v(1)=0.0d0
            multiDOF%a(1)=0.0d0
        endif
        multiDOF%itr=i
        call multiDOF%solve(dt=0.0010d0,FixNodeID=1, displacement=u_,Solver="GaussJordan")

        print *, multiDOF%u(1),multiDOF%v(1),multiDOF%a(1)
        
        call f%open("multiDOF_"//str(i)//".txt")
        call f%write(matrix(soil%mesh%nodcoord(:,1),multiDOF%u(:)))
        call f%close()
        call gp%write("set terminal png")
        call gp%write("set yr[-0.002:0.002]")
        call gp%write("plot 'multiDOF_"//str(i)//".txt' w l")
        call gp%write("set output '"//"multiDOF_"//str(i)//".png'")
        call gp%write("replot")

    enddo

    call gp%close()
end program main