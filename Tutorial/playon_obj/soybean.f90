program main
    use plantFEM
    implicit none

    type(Soybean_) :: soy

    call soy%init(config="Tutorial/playon_obj/realSoybeanConfig.json") 
    call soy%msh(name="soy")

end program main