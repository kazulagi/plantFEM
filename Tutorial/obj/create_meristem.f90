program main
    use MeristemClass
    implicit none
    type(Meristem_) :: ms
    type(IO_) :: f
    real(real64) :: dt,params(1:11)
    integer(int32) :: leafset_idx
    
    call f%open("height_and_volume.txt")

    dt = 60.0d0*60.0d0*24.0d0 ! 1 day
    
    ! parameters for first meristem
    params(1) = 1.00d0 ! max length, 20 mm
    params(2) = 0.05d0 ! max radius,  4 mm
    params(3) = 60.0d0*60.0d0*24.0d0*10.0d0 ! Time constant for length (20 days)
    params(4) = 60.0d0*60.0d0*24.0d0*10.0d0  ! Time constant for radius (5 days)
    params(5) = 0.50d0 ! threshold length on subdivision of the top meristem (50%)

    params(6) = 1.0d0 ! K_pL
    params(7) = 0.020d0 ! K_pR
    params(8) = 3.0d0 ! K_lL
    params(9) = 60.0d0*60.0d0*24.0d0* 30.0d0 ! T_pL
    params(10) = 60.0d0*60.0d0*24.0d0*30.0d0 ! T_pR
    params(11) = 60.0d0*60.0d0*24.0d0*20.0d0 ! T_lL

    call ms%init(Meristem_type=PF_MERISTEM_TYPE_SHOOT,params=params, dt=dt)
    call ms%vtk("meristem"+zfill(0,5),single_file=.true.)
    

    ! vegitative stage
    do i_i=1,60 ! days
        do j_j=1,size(ms%stem)
            call ms%grow_internode(idx=j_j,params=params,dt=dt)
            print *, "stem :",j_j,", my_t=",ms%stem(j_j)%my_time,",l :",ms%stem(j_j)%getLength()
        enddo
        do j_j=1,size(ms%leafset)
            if(ms%leafset(j_j)%is_empty()) cycle
            call ms%leafset(j_j)%grow_peti_and_leaf(params=params(6:11),dt=dt)
            print *, "leafset :",j_j
        enddo

        call ms%vtk("meristem"+zfill(i_i,5),single_file=.true.)
        
        write(f%fh,*) i_i, ms%getHeight(), ms%getVolume()
        call f%flush()

        call ms%update()
        

        !call ms%grow(dt=dt,temperature=temperature)
        
    enddo

    ! reproductive stage
    do i_i=61,200 ! days
        print *, i_i*dt/60.0d0/60.0d0 ! hour
        do j_j=1,size(ms%stem)
            call ms%grow_internode(idx=j_j,params=params,dt=dt,no_division=.true.)
            print *, "stem :",j_j,", my_t=",ms%stem(j_j)%my_time,",l :",ms%stem(j_j)%getLength()
        enddo
        do j_j=1,size(ms%leafset)
            if(ms%leafset(j_j)%is_empty()) cycle
            call ms%leafset(j_j)%grow_peti_and_leaf(params=params(6:11),dt=dt)
            print *, "leafset :",j_j
        enddo

       
        call ms%vtk("meristem"+zfill(i_i,5),single_file=.true.)
        write(f%fh,*) i_i, ms%getHeight(), ms%getVolume()
        call f%flush()
    enddo

    call f%close()
   
end program main  