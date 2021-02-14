program main
    use plantFEM
    implicit none

    type(Soybean_) :: soy
    type(Soil_) ::soil
    character(1000) :: clarg
    intrinsic :: get_command_argument

    call get_command_argument(1, clarg)
    call print(trim(clarg))
    return

    call soy%init(config="Tutorial/playon_obj/realSoybeanConfig.json") 
    !call soy%stl(name="soy")
    call soy%msh(name="soy")
    call soy%json(name="soy")

    call soil%create(x_num=3,y_num=3,z_num=1)
    call soil%resize(x=3.0d0, y=3.0d0, z=1.0d0)
    call soil%msh(name="soil")
end program main
