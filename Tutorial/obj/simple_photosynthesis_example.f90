program main
    use SoybeanClass
    use ArrayClass
    use IOClass
    implicit none
    
    type(Soybean_) :: soy
    type(Environment_) :: env
    real(real64),allocatable :: photosynthesis(:),volume(:)
    real(real64) :: dt
    
    ! initialize environment
    call env%init(json="Tutorial/obj/env.json")
    
    ! initialize soybean
    call soy%init(config="Tutorial/obj/soy.json")
    
    ! dt
    dt = 1.d0 ! 1 sec

    ! compute photosynthesis ratio (g/s)
    photosynthesis     = soy%getPhotoSynthesis(env=env,dt=dt) ! unit: micro-gram
    call soy%vtk("photosyn_result",scalar_field=photosynthesis,single_file=.true. )
    
    volume     = soy%getVolumePerElement() ! unit: SI
    call soy%vtk("vol_result",scalar_field=volume,single_file=.true. )
    call soy%vtk("photosyn_vol_result",scalar_field=photosynthesis/volume,single_file=.true. )
    
                

end program
    
