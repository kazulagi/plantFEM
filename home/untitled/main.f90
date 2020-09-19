program main
    use soybeanclass
    implicit none

    type(Soybean_) :: soy

    call soy%init("soyconfig.json")
    call soy%show("soy")
    
end program main