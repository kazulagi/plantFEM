program main
    use std
    implicit none

    type(Time_) :: time

    ! stop-watch starts!
    call time%start()
    
    print *, "Hello!"
    
    ! measuring cpu-time.
    call time%show()

    ! reset stop-watch
    call time%reset()

    print *, "How are you today?"
    
    ! measuring cpu-time.
    call time%show()

    ! sleep for 20 sec.
    call time%sleep(20)

end program main