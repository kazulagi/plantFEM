program main
    use plantFEM
    implicit none

    type(Webserver_) :: ws
    character(200)   :: message
    
    ! http サーバーの立ち上げ
    call ws%init()
    
    do 
        print *, "Please input message!"
        read(*,*) message
        ! bodyの記述
        ws%body = "<h1>"//trim(message)//"</h1>"

        ! 更新
        call ws%update()

        if( trim(adjustl(message))=="exit" )then
            exit
        endif
    enddo
    
    
end program main