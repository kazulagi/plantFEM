program main
    use MeristemClass
    implicit none
    type(Meristem_) :: ms
    type(IO_) :: f
    real(real64) :: dt,params(1:11)
    integer(int32) :: leafset_idx
    
    call f%open("height_and_volume.txt")

    ms%leaf_directional_angle = 180.0d0

    dt = 60.0d0*60.0d0*24.0d0 ! 1 day
    
    ! parameters for first meristem
    params(1) = 40.00d0/1000.0d0 ! max length, 30 mm
    params(2) =   2.50d0/1000.0d0 ! max radius,  2 mm
    params(3) = 60.0d0*60.0d0*24.0d0*20.0d0 ! Time constant for length (20 days)
    params(4) = 60.0d0*60.0d0*24.0d0*20.0d0  ! Time constant for radius (5 days)
    params(5) = 5.0d0/1000.0d0 ! threshold length on subdivision of the top meristem (5mm)

    params(6) = 80.0d0/1000.0d0 ! K_pL, Max length of petiole, 70 mm
    params(7) = 1.50d0/1000.0d0 ! K_pR, max radius of petiole, 1 mm
    params(8) = 100.0d0/1000.0d0 ! K_lL, max length of leaf, 100 mm
    params(9) = 60.0d0*60.0d0*24.0d0* 20.0d0 ! T_pL
    params(10) = 60.0d0*60.0d0*24.0d0*20.0d0 ! T_pR
    params(11) = 60.0d0*60.0d0*24.0d0*20.0d0 ! T_lL

    call ms%init(Meristem_type=PF_MERISTEM_TYPE_SHOOT,params=params, dt=dt)
    call ms%vtk("meristem"+zfill(0,5),single_file=.true.)
    

    !ms%top_meristem_aspect_ratio = params(1)/params(2)*0.50d0
    ! vegitative stage
    do i_i=1,40 ! days
        print *, "day", i_i
        call ms%grow(params=params,dt=dt,no_division=.false.)
        call ms%vtk("meristem"+zfill(i_i,5),single_file=.true.)
        write(f%fh,*) i_i, ms%getHeight(), ms%getVolume(),ms%getBiomass()
        if(i_i==5)then
            call ms%set_branch(stem_idx=1,leafset_idx=1,params=params,dt=dt)
        endif
        if(i_i==15)then
            call ms%set_branch(stem_idx=2,leafset_idx=1,params=params,dt=dt)
        endif
        print *, "NE, NN: ", ms%ne(), ms%nn()
        call f%flush()
        call ms%update()
    enddo

    ! reproductive stage
    do i_i=41,120 ! days
        print *, "day", i_i
        call ms%grow(params=params,dt=dt,no_division=.true.)
        call ms%vtk("meristem"+zfill(i_i,5),single_file=.true.)
        write(f%fh,*) i_i, ms%getHeight(), ms%getVolume(),ms%getBiomass()
        call f%flush()
    enddo

    call f%close()
    
end program main  