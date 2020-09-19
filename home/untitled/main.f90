program main
    use plantFEM
    implicit none

    type(Soybean_) :: soy
    
    call soy%init("soyconfig")
    call soy%gmsh("soy")

end program main