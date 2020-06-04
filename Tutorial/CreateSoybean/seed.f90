program main
    use SeedClass
    implicit none

    type(Seed_) :: soy

    ! create seed
    call soy%create(MeshType="Sphere3D",x_num=12,y_num=11,z_num=10,x_len=90.0d0, y_len=80.0d0,z_len=70.0d0)
    
    ! create environment
    call soy%env(disp_x=0.0d0,x_max=1.0d0,x_min=-5.0d0,y_max=100.0d0,y_min=-100.0d0,&
        z_max=100.0d0,z_min=-100.0d0)
    call soy%env(disp_y=0.0d0,x_max=1.0d0,x_min=-5.0d0,y_max=100.0d0,y_min=-100.0d0,&
        z_max=100.0d0,z_min=-100.0d0)
    call soy%env(disp_z=0.0d0,x_max=1.0d0,x_min=-5.0d0,y_max=100.0d0,y_min=-100.0d0,&
        z_max=100.0d0,z_min=-100.0d0)
    call soy%env(WaterContent=1.0d0,x_max=5.0d0,x_min=-1.0d0,y_max=100.0d0,y_min=-100.0d0,&
        z_max=100.0d0,z_min=-100.0d0)
    
    ! start growth
    call soy%grow(timestep=1,dt=10.0d0,Display=.true.,nr_tol=0.010d0,interval=10)
    
    
end program main