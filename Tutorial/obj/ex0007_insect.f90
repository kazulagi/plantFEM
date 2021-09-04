program main
    use plantFEM
    implicit none

    type(Soybean_) :: soy
    type(Insect_) :: worm(50), insect
    type(Soil_) ::soil
    integer(int32) :: i


    call soy%init(config="Tutorial/obj/realSoybeanConfig.json") 
    
    do i=1,50 ! 50 worms
        call worm(i)%create(eatSpeed = 100.0d0*dble(1.0e-9) ) ! m^3/day
        call worm(i)%set(leaf=soy%leaf(i) )
        call worm(i)%eat(dt=100.0d0) ! 10day

    enddo
    
    !call soy%stl(name="soy")
    call soy%msh(name="soy")
    !call soy%json(name="soy")

    call soil%create(x_num=3,y_num=3,z_num=1)
    call soil%resize(x=3.0d0, y=3.0d0, z=1.0d0)
    call soil%msh(name="soil")


end program main
