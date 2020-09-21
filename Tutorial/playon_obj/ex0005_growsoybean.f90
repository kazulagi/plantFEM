program main
    use plantFEM
    implicit none

    type(Soybean_) :: soy
    type(Soil_)    :: soil
    type(Light_)    :: light

    call light%init() ! create the Sun

    call soil%init("soilconfig.json")

    !call soil%gmsh("soil")

    call soy%init()
    !call soy%gmsh("output")

    call soy%grow(dt=10.0d0,temp=27.0d0,light=light)
    

end program main