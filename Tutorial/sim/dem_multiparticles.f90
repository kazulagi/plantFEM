program main
    use DemDomainClass

    implicit none
    
    type(DEMDomain_) :: dem,wall
    type(FEMDomain_) :: cube, bottom_wall
    integer(int32) :: i
    type(Math_)  :: math
    real(real64) :: dt
    type(Time_) :: time

    !call cube%create("Cube3D",x_num=200,y_num=200,z_num=1000)
    call cube%create("Cube3D",x_num=50,y_num=50,z_num=100)
    call cube%resize(5.0d0,5.0d0,10.0d0)
    call cube%move(x=-cube%x_len()/2.0d0,y=-cube%y_len()/2.0d0,z=-cube%z_len()+2.0d0)
    !call cube%resize((25.0d0-10.0d0)/2.0d0-1.0d0,  25.0d0-2.0d0-1.00d0-1.0d0,  10.0d0-1.0d0)
    !call cube%move(x=-cube%x_len()-3.50d0,y=-cube%y_len()/2.0d0,z=-cube%z_len()+4.0d0)
    
    dem = cube
    call cube%remove()
    dem%r = 0.30d0
    dem%m = 0.40d0
    
    call bottom_wall%create("Cube3D",x_num=50,y_num=50,z_num=40)
    call bottom_wall%resize(25.0d0,25.0d0,20.0d0)
    call bottom_wall%move(x=-25.0d0/2.0d0,y=-25.0d0/2.0d0,z=-10.0d0)
    call bottom_wall%removeElement(&
            x_min=bottom_wall%xmin()+1.0d0,&
            x_max=bottom_wall%xmax()-1.0d0,&
            y_min=bottom_wall%ymin()+1.0d0,&
            y_max=bottom_wall%ymax()-1.0d0,&
            z_min=bottom_wall%zmin()+1.0d0,&
            z_max=bottom_wall%zmax() &
        )
    
    wall = bottom_wall
    call bottom_wall%remove()

    wall%status(:) = DEM_DUMMY_PARTICLE
    wall%r(:) = 1.00d0

    
    
    !call cube%vtk("cube")
    !call bottom_wall%vtk("wall")


    call dem%add(wall)
    !call dem%vtk("init")
    
    

    dem%contact_stiffness = 10000.0d0
    dem%contact_damping   = 100.0d0

    print *, "num_particle",dem%np()
    dt = 0.010d0

    print *, dt , 2.0d0*math%PI*sqrt(dem%m(1)/dem%contact_stiffness)/20.0d0
    
    dem%m(:) = 10.0d0
    
    do i=1,1000
        dem%timestep = dem%timestep + 1

        call time%start()
        call dem%updateForce(dt)
        call time%show()
        stop

        call dem%updateDisplacement(&
            dt=dt,&
            active_range=to_range(&
                    x_min=-100.0d0,x_max=100.0d0,&
                    y_min=-100.0d0,y_max=100.0d0,&
                    z_min= -100.0d0,z_max=100.0d0 &
                ))
        call dem%vtk("dem_step"+zfill(dem%timestep/10,4))

    enddo
    
end program main