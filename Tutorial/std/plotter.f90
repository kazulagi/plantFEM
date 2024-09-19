program main
    use PlotClass
    use TimeClass
    use ArrayClass
    implicit none

    type(Plot_) :: plt
    real(real64),target,allocatable :: x(:),fx(:),gx(:),hx(:)
    type(Time_) ::time

    ! similar to matplotlib
    
    do i_i=1,10
        x = linspace([0.0d0,10.0d0],100000)+dble(i_i)*1.0d0
        fx = sin(x)
        gx = cos(x)
        hx = exp(-0.10d0*x)*cos(x)*sin(x)
        
        call plt%setLayout([3,4])

        plt%subplot(1,1)%xlabel = "x"
        plt%subplot(1,1)%ylabel = "f(x)"
        plt%subplot(1,1)%x => x
        plt%subplot(1,1)%y => fx

        plt%subplot(2,1)%xlabel = "x"
        plt%subplot(2,1)%ylabel = "g(x)"
        plt%subplot(2,1)%x => x
        plt%subplot(2,1)%y => gx

        plt%subplot(1,2)%yr     = [-0.50d0,0.50d0]
        plt%subplot(1,2)%xlabel = "x"
        plt%subplot(1,2)%ylabel = "h(x)"
        plt%subplot(1,2)%x => x
        plt%subplot(1,2)%y => hx

        plt%subplot(3,2)%xlabel = "x"
        plt%subplot(3,2)%ylabel = "f(x)"
        plt%subplot(3,2)%x => x
        plt%subplot(3,2)%y => fx

        !call plt%pdf("my_graphs")
        call plt%png("my_graphs"+zfill(i_i,4))
    enddo
    
end program main