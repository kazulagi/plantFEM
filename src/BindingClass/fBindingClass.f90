
    

program main
    implicit none
    interface
      subroutine hello() bind(c)
      end subroutine
    end interface
    
    call hello
    
end program