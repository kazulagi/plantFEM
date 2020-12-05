program main
    use plantFEM
    implicit none

    type(Soybean_) :: soy

    call soy%init(config="Tutorial/playon_obj/realSoybeanConfig.json") 
    !call soy%stl(name="soy")
    call soy%msh(name="soy")
    call soy%json(name="soy")

end program main
