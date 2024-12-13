program main
    use plantFEM
    implicit none

    type(Soybean_) :: soy

    call soy%init(config="Tutorial/obj/realSoybeanConfig.json") 
    call soy%vtk(name="soy",single_file=.true.)
    
end program main