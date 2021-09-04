program main
    !use plantFEM
    use STLClass
    implicit none

    !type(Soybean_) :: soy
    !type(Soil_) ::soil
    type(STL_) :: stl

    call stl%open("full_soy_stem")
    call stl%reduce(ratio=0.07d0)
    call stl%write("full_soy_stem_small")
    call stl%open("full_soy_leaf")
    call stl%reduce(ratio=0.07d0)
    call stl%write("full_soy_leaf_small")
    call stl%open("full_soy_root")
    call stl%reduce(ratio=0.07d0)
    call stl%write("full_soy_root_small")
    !call soy%init(config="Tutorial/obj/realSoybeanConfig.json") 
    !call soy%stl(name="soy")

    !call soy%msh(name="soy")
    !call soy%json(name="soy")

    !call soil%create(x_num=3,y_num=3,z_num=1)
    !call soil%resize(x=3.0d0, y=3.0d0, z=1.0d0)
    !call soil%msh(name="soil")
end program main
