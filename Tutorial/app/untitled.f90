program main
    use plantFEM
    implicit none

    type(Soybean_)::soy

    call soy%init()
    call soy%move(x=10.0d0)
    
end program main

